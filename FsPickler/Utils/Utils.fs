namespace Nessos.FsPickler

//    #nowarn "1204"
//    #nowarn "42"

//    module internal Config =
//        [<Literal>]
//        let optimizeForLittleEndian = true

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
//
//#if EMIT_IL
//        open Nessos.FsPickler.Emit
//        open System.Reflection.Emit
//#endif

        open Microsoft.FSharp.Reflection

        let runsOnMono = lazy(System.Type.GetType("Mono.Runtime") <> null)

        // Detect current core library version at runtime
        // as suggested in http://stackoverflow.com/a/8543850
        let isDotNet45OrNewer =
            lazy(Type.GetType("System.Reflection.ReflectionContext") <> null)

//        // TODO: include in serialization header
//        let versionTag = 
//            let version = System.Reflection.Assembly.GetExecutingAssembly().GetName().Version
//            sprintf "FsPickler-v%O" version

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

        module Exn =
            let catch (f : unit -> 'T) =
                try f () |> Success with e -> Error e

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


//        // produces a structural hashcode out of a byte array
//        let getByteHashCode (bs : byte []) =
//            let n = bs.Length
//            let mutable i = 0
//            let mutable acc = 0
//
//            let inline bytes2Int x y z w = 
//                int x + (int y <<< 8) + (int z <<< 16) + (int w <<< 24)
//
//            while i + 4 <= n do
//                acc <- acc ^^^ bytes2Int bs.[i] bs.[i + 1] bs.[i + 2] bs.[i + 3]
//                i <- i + 4
//
//            match n - i with
//            | 0 -> ()
//            | 1 -> acc <- acc ^^^ bytes2Int bs.[i] 0uy 0uy 0uy
//            | 2 -> acc <- acc ^^^ bytes2Int bs.[i] bs.[i+1] 0uy 0uy
//            | _ -> acc <- acc ^^^ bytes2Int bs.[i] bs.[i+1] bs.[i+2] 0uy
//
//            acc

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


        // reflection utils

//        let allFields = 
//            BindingFlags.NonPublic ||| BindingFlags.Public ||| 
//                BindingFlags.Instance ||| BindingFlags.FlattenHierarchy 
//
//        let allMembers =
//            BindingFlags.NonPublic ||| BindingFlags.Public |||
//                BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.FlattenHierarchy
//
//        let allConstructors = BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public
//
//        let containsAttr<'T when 'T :> Attribute> (m : MemberInfo) =
//            m.GetCustomAttributes(typeof<'T>, true) |> Seq.isEmpty |> not

//        /// checks if instances of given type can be arrays
//        let isAssignableFromArray =
//            let getCanonicalType (t:Type) = 
//                if t.IsGenericType then t.GetGenericTypeDefinition() 
//                else t
//
//            let arrayIfs =  typeof<int []>.GetInterfaces() |> Array.map getCanonicalType
//
//            fun (t : Type) ->
//                if t.IsAssignableFrom typeof<Array> then true
//                elif t.IsArray then true
//                elif not t.IsInterface then false
//                else
//                    // check interface compatibility
//                    Array.exists ((=) (getCanonicalType t)) arrayIfs


        type SerializationInfo with
            member sI.Write<'T> (name : string, x : 'T) =
                sI.AddValue(name, x, typeof<'T>)

            member sI.Read<'T>(name : string) =
                sI.GetValue(name, typeof<'T>) :?> 'T






        let inline pickle (f : Stream -> 'T -> unit) (x : 'T) : byte [] =
            use mem = new MemoryStream()
            f mem x
            mem.GetBuffer()

        let inline unpickle (f : Stream -> 'T) (data : byte []) : 'T =
            use mem = new MemoryStream(data)
            f mem


//        /// walks up the type hierarchy, gathering all instance fields
//        let gatherFields (t : Type) =
//            // resolve conflicts, index by declaring type and field name
//            let gathered = ref Map.empty<string * string, FieldInfo>
//
//            let rec gather (t : Type) =
//                let fields = t.GetFields(allFields)
//                for f in fields do
//                    let k = f.DeclaringType.AssemblyQualifiedName, f.Name
//                    if not <| gathered.Value.ContainsKey k then
//                        gathered := gathered.Value.Add(k, f)
//
//                match t.BaseType with
//                | null -> ()
//                | t when t = typeof<obj> -> ()
//                | bt -> gather bt
//
//            do gather t
//
//            gathered.Value |> Map.toArray |> Array.map snd



//        type ShallowObjectCopier private () =
//            
//            static let mkCopier (t : Type) =
//                if t.IsValueType then invalidOp t.FullName "not a class."
//
//                let fields = gatherFields t
//#if EMIT_IL
//                let dele =
//                    DynamicMethod.compileAction2<obj, obj> "shallowCopier" (fun source target ilGen ->
//                        for f in fields do
//                            target.Load()
//                            source.Load()
//                            ilGen.Emit(OpCodes.Ldfld, f)
//                            ilGen.Emit(OpCodes.Stfld, f)
//
//                        ilGen.Emit OpCodes.Ret
//                    )
//
//                fun (src : obj) (tgt : obj) -> dele.Invoke(src,tgt)
//#else
//                fun src dst ->
//                    for f in fields do
//                        let v = f.GetValue(src)
//                        f.SetValue(dst, v)
//#endif
//            static let mkCopierMemoized = memoize mkCopier
//
//            static member Copy (t : Type) (source : obj) (target : obj) = 
//                mkCopierMemoized t source target




//
//        // .NET 4.0 backwards compatibility
//
//        [<Sealed>]
//        type NonClosingStreamWrapper(s: Stream) =
//            inherit Stream()
//
//            let mutable closed = false
//            let ns () = raise (NotSupportedException())
//
//            let cc () =
//                if closed then
//                    raise <| ObjectDisposedException("Stream has been closed or disposed")
//
//            override w.BeginRead(a, b, c, d, e) = cc (); s.BeginRead(a, b, c, d, e)
//            override w.BeginWrite(a, b, c, d, e) = cc (); s.BeginWrite(a, b, c, d, e)
//
//            override w.Close() =
//                if not closed then
//                    s.Flush()
//                    closed <- true
//
//            override w.CreateObjRef(t) = ns ()
//            override w.EndRead(x) = cc (); s.EndRead(x)
//            override w.EndWrite(x) = cc (); s.EndWrite(x)
//            override w.Flush() = cc (); s.Flush()
//            override w.InitializeLifetimeService() = ns ()
//            override w.Read(a, b, c) = cc (); s.Read(a, b, c)
//            override w.ReadByte() = cc (); s.ReadByte()
//            override w.Seek(a, b) = cc (); s.Seek(a, b)
//            override w.SetLength(a) = cc (); s.SetLength(a)
//            override w.Write(a, b, c) = cc (); s.Write(a, b, c)
//            override w.WriteByte(a) = cc (); s.WriteByte(a)
//            override w.CanRead = if closed then false else s.CanRead
//            override w.CanSeek = if closed then false else s.CanSeek
//            override w.CanTimeout = if closed then false else s.CanTimeout
//            override w.CanWrite = if closed then false else s.CanWrite
//            override w.Length = cc (); s.Length
//
//            override w.Position
//                with get () = cc (); s.Position
//                and set x = cc (); s.Position <- x
//
//            override w.ReadTimeout = cc (); s.ReadTimeout
//            override w.WriteTimeout = cc (); s.WriteTimeout
//
//        type BinaryWriter with
//            static member Create(output: Stream, encoding: Encoding, leaveOpen: bool) =
//#if NET40
//#else
//                if isDotNet45OrNewer then
//                    new BinaryWriter(output, encoding, leaveOpen)
//                else
//#endif
//                if leaveOpen then
//                    new BinaryWriter(new NonClosingStreamWrapper(output), encoding)
//                else
//                    new BinaryWriter(output, encoding)
//
//        type BinaryReader with
//            static member Create(output: Stream, encoding: Encoding, leaveOpen: bool) =
//#if NET40
//#else
//                if isDotNet45OrNewer then
//                    new BinaryReader(output, encoding, leaveOpen)
//                else
//#endif
//                if leaveOpen then
//                    new BinaryReader(new NonClosingStreamWrapper(output), encoding)
//                else
//                    new BinaryReader(output, encoding)