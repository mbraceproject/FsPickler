namespace MBrace.FsPickler

#nowarn "1204"

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

    let inline isNull (t : 'T) = 
        match t with null -> true | _ -> false

    let inline isNotNull (t : 'T) =
        match t with null -> false | _ -> true

    // Detect current core library version at runtime
    // as suggested in http://stackoverflow.com/a/8543850
    let isDotNet45OrNewer = Type.GetType("System.Reflection.ReflectionContext") |> isNotNull

    let runsOnMono = Type.GetType("Mono.Runtime") |> isNotNull

    /// hashset constructor
    let hset (ts : seq<'T>) = new HashSet<_>(ts)

    let inline ignore2 _ _ = ()

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

    module Option =
        let inline isSome (ts : 'T option) = match ts with None -> false | Some _ -> true

    module Enum =
        /// <summary>
        ///     Checks that set of enumeration flags has given flag
        /// </summary>
        /// <param name="flags">Flags to be checked.</param>
        /// <param name="flag">Flag to be satisfied.</param>
        let inline hasFlag flags flag = (flags &&& flag) = flag

    let inline denull x = if isNull x then None else Some x

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

    /// thread safe memo operator with parametric support
    let memoizeParam (f : 'P -> 'T -> 'S) =
        let cache = new ConcurrentDictionary<'T,'S> ()
        fun p t -> cache.GetOrAdd(t, f p)

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

    /// IEqualityComparer implementation that follows reference equality
    type ReferenceEqualityComparer<'T when 'T : not struct>() =
        interface IEqualityComparer<'T> with
            member __.Equals(t: 'T, t': 'T): bool = obj.ReferenceEquals(t,t')
            member __.GetHashCode(value: 'T): int = System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode value

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

    /// Stream implementation that computes object size, discarding any data
    [<AutoSerializable(false)>]
    type LengthCounterStream () =
        inherit Stream ()
        let mutable length = 0L
        member __.Count = length
        member __.Reset () = length <- 0L

        override __.CanRead = false
        override __.CanSeek = false
        override __.CanTimeout = false
        override __.CanWrite = true
        override __.ReadTimeout = 0
        override __.WriteTimeout = 0
        override __.Seek(_,_) = raise <| new NotSupportedException()
        override __.SetLength _ = raise <| new NotSupportedException()
        override __.Read(_,_,_) = raise <| new NotSupportedException()
        override __.Flush () = ()
        override __.Position with set _ = raise <| new NotSupportedException()

        override __.Length = length
        override __.Position = length
        override __.WriteByte _ = length <- length + 1L
        override __.Write(_, _, count : int) = length <- length + int64 count