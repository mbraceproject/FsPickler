namespace Nessos.FsPickler

    #nowarn "1204"
    #nowarn "42"

    [<AutoOpen>]
    module internal Utils =
        
        open System
        open System.Collections.Generic
        open System.Collections.Concurrent
        open System.IO
        open System.Reflection
        open System.Threading
        open System.Text
        open System.Runtime.Serialization

        open Microsoft.FSharp.Reflection

        // Detect current core library version at runtime
        // as suggested in http://stackoverflow.com/a/8543850
        let isDotNet45OrNewer =
            lazy(Type.GetType("System.Reflection.ReflectionContext") <> null)

        let runsOnMono = lazy(Type.GetType("Mono.Runtime") <> null)

        /// hashset constructor
        let hset (ts : seq<'T>) = new HashSet<_>(ts)

        // bad implementation: would be safer using ExceptionDispatchInfo but would break compatibility with 4.0
        let rec inline reraise' (e : #exn) =
            do remoteStackTraceField.SetValue(e, e.StackTrace + System.Environment.NewLine)
            raise e

        and private remoteStackTraceField : FieldInfo =
            let bfs = BindingFlags.NonPublic ||| BindingFlags.Instance
            match typeof<System.Exception>.GetField("remote_stack_trace", bfs) with
            | null ->
                match typeof<System.Exception>.GetField("_remoteStackTraceString", bfs) with
                | null -> failwith "Could not locate RemoteStackTrace field for System.Exception."
                | f -> f
            | f -> f

        /// Value or exception
        type Exn<'T> =
            | Success of 'T
            | Error of exn
        with
            /// evaluate, re-raising the exception if failed
            member e.Value =
                match e with
                | Success t -> t
                | Error e -> raise e

            member e.IsValue =
                match e with Success _ -> true | Error _ -> false

            member e.IsException =
                match e with Error _ -> true | Success _ -> false

        module Exn =
            let catch (f : unit -> 'T) =
                try f () |> Success with e -> Error e

            let error (e : exn) = Error e

            let map (f : 'T -> 'S) (x : Exn<'T>) =
                match x with
                | Success x -> Success (f x)
                | Error e -> Error e

            let bind (f : 'T -> 'S) (x : Exn<'T>) =
                match x with
                | Success x -> try Success <| f x with e -> Error e
                | Error e -> Error e

        let inline denull x = if x = null then None else Some x

        let inline fastUnbox<'T> (x : obj) = 
            Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.UnboxFast<'T> x


        type Latch () =
            let mutable switch = 0
            member __.Trigger () = System.Threading.Interlocked.CompareExchange(&switch, 1, 0) = 0
            member __.Reset () = switch <- 0

        type IDictionary<'K,'V> with
            member d.TryFind (k : 'K) =
                let mutable v = Unchecked.defaultof<'V>
                let found = d.TryGetValue(k, &v)
                if found then Some v else None

        type Map<'K,'V when 'K : comparison> with
            member m.AddNoOverwrite(key : 'K, value : 'V) =
                if m.ContainsKey key then invalidArg "key" "An item with the same key has already been added."
                else
                    m.Add(key, value)

        let (|InnerExn|_|) (e : #exn) = denull e.InnerException

        /// replacement for IDictionary
        type ICache<'K,'V> =
            abstract Lookup : 'K -> 'V option
            abstract Commit : 'K -> 'V -> 'V

        /// thread safe memo operator
        let memoize (f : 'T -> 'S) =
            let cache = new ConcurrentDictionary<'T,'S> ()
            fun t -> cache.GetOrAdd(t, f)

        /// takes an isomorphic function and its inverse as inputs
        /// memoizes output in both directions
        type BiMemoizer<'T, 'S>(f : 'T -> 'S, g : 'S -> 'T) =
            let cache = new ConcurrentDictionary<'T,'S> ()
            let cache' = new ConcurrentDictionary<'S,'T> ()

            member __.F(t : 'T) =
                let mutable s = Unchecked.defaultof<'S>
                let found = cache.TryGetValue(t, &s)
                if found then s
                else
                    let s = f t
                    cache.TryAdd(t,s) |> ignore
#if DEBUG
#else
                    cache'.TryAdd(s,t) |> ignore
#endif
                    s

            member __.G(s : 'S) =
                let mutable t = Unchecked.defaultof<'T>
                let found = cache'.TryGetValue(s, &t)
                if found then t
                else
                    let t = g s
                    cache'.TryAdd(s,t) |> ignore
#if DEBUG
#else
                    cache.TryAdd(t,s) |> ignore
#endif

                    t

        type SerializationInfo with
            member internal sI.Write<'T> (name : string, x : 'T) =
                sI.AddValue(name, x, typeof<'T>)

            member internal sI.Read<'T>(name : string) =
                sI.GetValue(name, typeof<'T>) :?> 'T

        let inline pickleBinary (f : Stream -> 'T -> unit) (x : 'T) : byte [] =
            use mem = new MemoryStream()
            f mem x
            mem.ToArray()

        let inline unpickleBinary (f : Stream -> 'T) (pickle : byte []) : 'T =
            use mem = new MemoryStream(pickle)
            f mem

        let inline pickleString (f : TextWriter -> 'T -> unit) (x : 'T) : string =
            use sw = new StringWriter()
            f sw x
            sw.ToString()

        let inline unpickleString (f : TextReader -> 'T) (pickle : string) : 'T =
            use sr = new StringReader(pickle)
            f sr

        [<NoComparison>]
        type ReferenceEqualityContainer<'T when 'T : not struct>(value : 'T) =
            member __.Value = value
            override __.GetHashCode() = System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode value
            override __.Equals o =
                match o with
                | :? ReferenceEqualityContainer<'T> as c -> obj.ReferenceEquals(value, c.Value)
                | _ -> false

        // Byte <-> hex converter that emulates the AssemblyQualifiedName format
        // used in public key tokens
        module Bytes =

            [<Literal>]
            let private alph = "0123456789abcdef"
            let private alphInv = alph |> Seq.mapi (fun i s -> (s,byte i)) |> Map.ofSeq

            let toBase16String (bs : byte []) =
                let sb = new StringBuilder()
                for i = 0 to bs.Length - 1 do
                    let b = bs.[i]
                    let c1 = alph.[int (b >>> 4)]
                    let c2 = alph.[int (0x0fuy &&& b)]
                    sb.Append(c1).Append(c2) |> ignore

                sb.ToString()

            let ofBase16String (hex : string) =
                let inline toUInt (c : Char) = alphInv.[Char.ToLower c]
                let size = hex.Length / 2
                let bs = Array.zeroCreate<byte> (size)
                for i = 0 to size - 1 do
                    bs.[i] <- (toUInt hex.[2*i + 1]) ||| ((toUInt hex.[2*i]) <<< 4)

                bs