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

        /// stackless raise operator
        let inline raise (e: System.Exception) = (# "throw" e : 'U #)

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

        type Atom<'T when 'T : not struct>(value : 'T) =
            let refCell = ref value
    
            let rec swap f = 
                let currentValue = !refCell
                let result = Interlocked.CompareExchange<'T>(refCell, f currentValue, currentValue)
                if obj.ReferenceEquals(result, currentValue) then ()
                else Thread.SpinWait 20; swap f

            let transact f =
                let output = ref Unchecked.defaultof<'S>
                let f' x = let t,s = f x in output := s ; t
                swap f' ; !output
        
            member __.Value with get() : 'T = !refCell
            member __.Swap (f : 'T -> 'T) : unit = swap f
            member __.Set (v : 'T) : unit = swap (fun _ -> v)
            member __.Transact (f : 'T -> 'T * 'S) : 'S = transact f  

        [<RequireQualifiedAccess>]
        module Atom =
            let atom x = Atom<_>(x)
            let get (a : Atom<_>) = a.Value
            let swap (f : 'T -> 'T) (a : Atom<_>) = a.Swap f
            let transact (f : 'T -> 'T * 'R) (a : Atom<_>) = a.Transact f


        let inline denull x = if x = null then None else Some x

        let inline fastUnbox<'T> (x : obj) = 
            Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.UnboxFast<'T> x

        type IDictionary<'K,'V> with
            member d.TryFind (k : 'K) =
                let found, v = d.TryGetValue k
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
                let found, s = cache.TryGetValue t
                if found then s
                else
                    let s = f t
                    cache.TryAdd(t,s) |> ignore
                    cache'.TryAdd(s,t) |> ignore
                    s

            member __.G(s : 'S) =
                let found, t = cache'.TryGetValue s
                if found then t
                else
                    let t = g s
                    cache.TryAdd(t,s) |> ignore
                    cache'.TryAdd(s,t) |> ignore
                    t

        [<RequireQualifiedAccess>]
        module Stream =

            let internal bufferSize = 256    
            let internal buffer = new ThreadLocal<byte []>(fun () -> Array.zeroCreate<byte> bufferSize)

            let inline WriteInt (stream : Stream) (n : int) =
                let buf = buffer.Value
                buf.[0] <- byte n
                buf.[1] <- byte (n >>> 8)
                buf.[2] <- byte (n >>> 16)
                buf.[3] <- byte (n >>> 24)
                stream.Write(buf, 0, 4)

            let inline ReadInt (stream : Stream) = 
                let buf = buffer.Value
                if stream.Read(buf, 0, 4) < 4 then
                    raise <| new EndOfStreamException()

                (int buf.[0])
                    ||| (int buf.[1] <<< 8)
                    ||| (int buf.[2] <<< 16)
                    ||| (int buf.[3] <<< 24)
                

            /// block copy primitive array to stream
            let ReadFromArray (stream : Stream, array : Array) =
                do stream.Flush()

                let buf = buffer.Value
                let totalBytes = Buffer.ByteLength array

                let d = totalBytes / bufferSize
                let r = totalBytes % bufferSize

                for i = 0 to d - 1 do
                    Buffer.BlockCopy(array, i * bufferSize, buf, 0, bufferSize)
                    stream.Write(buf, 0, bufferSize)

                if r > 0 then
                    Buffer.BlockCopy(array, d * bufferSize, buf, 0, r)
                    stream.Write(buf, 0, r)

            /// copy stream contents to preallocated array
            let WriteToArray (stream : Stream, array : Array) =
                let buf = buffer.Value
                let inline readBytes (n : int) =
                    if stream.Read(buf, 0, n) < n then
                        // TODO: this is wrong
                        raise <| new EndOfStreamException()
        
                let totalBytes = Buffer.ByteLength array

                let d = totalBytes / bufferSize
                let r = totalBytes % bufferSize

                for i = 0 to d - 1 do
                    do readBytes bufferSize
                    Buffer.BlockCopy(buf, 0, array, i * bufferSize, bufferSize)

                if r > 0 then
                    do readBytes r
                    Buffer.BlockCopy(buf, 0, array, d * bufferSize, r)

        type SerializationInfo with
            member internal sI.Write<'T> (name : string, x : 'T) =
                sI.AddValue(name, x, typeof<'T>)

            member internal sI.Read<'T>(name : string) =
                sI.GetValue(name, typeof<'T>) :?> 'T

        let inline pickle (f : Stream -> 'T -> unit) (x : 'T) : byte [] =
            use mem = new MemoryStream()
            f mem x
            mem.ToArray()

        let inline unpickle (f : Stream -> 'T) (data : byte []) : 'T =
            use mem = new MemoryStream(data)
            f mem


        [<NoComparison>]
        type ReferenceEqualityContainer<'T when 'T : not struct>(value : 'T) =
            member __.Value = value
            override __.GetHashCode() = System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode value
            override __.Equals o =
                match o with
                | :? ReferenceEqualityContainer<'T> as c -> obj.ReferenceEquals(value, c.Value)
                | _ -> false