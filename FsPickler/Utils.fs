namespace Nessos.FsPickler

    #nowarn "1204"
    #nowarn "42"

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

        let runsOnMono = System.Type.GetType("Mono.Runtime") <> null

        // TODO: include in serialization header
        let versionTag = 
            let version = System.Reflection.Assembly.GetExecutingAssembly().GetName().Version
            sprintf "FsPickler-v%O" version

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
            fun t ->
                let found, s = cache.TryGetValue t
                if found then s
                else
                    let s = f t
                    cache.TryAdd(t,s) |> ignore
                    s

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

        // produces a structural hashcode out of a byte array
        let getByteHashCode (bs : byte []) =
            let n = bs.Length
            let mutable i = 0
            let mutable acc = 0

            let inline bytes2Int x y z w = 
                int x + (int y <<< 8) + (int z <<< 16) + (int w <<< 24)

            while i + 4 <= n do
                acc <- acc ^^^ bytes2Int bs.[i] bs.[i + 1] bs.[i + 2] bs.[i + 3]
                i <- i + 4

            match n - i with
            | 0 -> ()
            | 1 -> acc <- acc ^^^ bytes2Int bs.[i] 0uy 0uy 0uy
            | 2 -> acc <- acc ^^^ bytes2Int bs.[i] bs.[i+1] 0uy 0uy
            | _ -> acc <- acc ^^^ bytes2Int bs.[i] bs.[i+1] bs.[i+2] 0uy

            acc

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

        let allFields = 
            BindingFlags.NonPublic ||| BindingFlags.Public ||| 
                BindingFlags.Instance ||| BindingFlags.FlattenHierarchy 

        let allMembers =
            BindingFlags.NonPublic ||| BindingFlags.Public |||
                BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.FlattenHierarchy

        let allConstructors = BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public

        let containsAttr<'T when 'T :> Attribute> (m : MemberInfo) =
            m.GetCustomAttributes(typeof<'T>, true) |> Seq.isEmpty |> not

        /// checks if instances of given type can be arrays
        let isAssignableFromArray =
            let getCanonicalType (t:Type) = 
                if t.IsGenericType then t.GetGenericTypeDefinition() 
                else t

            let arrayIfs =  typeof<int []>.GetInterfaces() |> Array.map getCanonicalType

            fun (t : Type) ->
                if t.IsAssignableFrom typeof<Array> then true
                elif t.IsArray then true
                elif not t.IsInterface then false
                else
                    // check interface compatibility
                    Array.exists ((=) (getCanonicalType t)) arrayIfs


        type SerializationInfo with
            member sI.Write<'T> (name : string, x : 'T) =
                sI.AddValue(name, x, typeof<'T>)

            member sI.Read<'T>(name : string) =
                sI.GetValue(name, typeof<'T>) :?> 'T

        type Delegate with
            static member CreateDelegate<'T when 'T :> Delegate> (m : MethodInfo) =
                System.Delegate.CreateDelegate(typeof<'T>, m) :?> 'T

        type Type with
            member t.GetGenericMethod(isStatic, name : string, genericArgCount : int, paramCount : int) =
                t.GetMethods(allMembers)
                |> Array.find(fun m ->
                    m.Name = name 
                        && m.IsStatic = isStatic
                        && genericArgCount = m.GetGenericArguments().Length
                        && paramCount = m.GetParameters().Length)

            member t.TryGetConstructor(args : Type []) = denull <| t.GetConstructor(allConstructors,null,args, [||])

        type MethodInfo with
            member m.GuardedInvoke(instance : obj, parameters : obj []) =
                try m.Invoke(instance, parameters)
                with :? TargetInvocationException as e when e.InnerException <> null ->
                    reraise' e.InnerException

            member m.GetParameterTypes() = m.GetParameters() |> Array.map (fun p -> p.ParameterType)

        type ConstructorInfo with
            member c.GetParameterTypes() = c.GetParameters() |> Array.map (fun p -> p.ParameterType)


        let isISerializable (t : Type) =
            if typeof<ISerializable>.IsAssignableFrom t then
                t.TryGetConstructor [| typeof<SerializationInfo> ; typeof<StreamingContext> |]
                |> Option.isSome
            else
                false

        let inline pickle (f : Stream -> 'T -> unit) (x : 'T) : byte [] =
            use mem = new MemoryStream()
            f mem x
            mem.ToArray()

        let inline unpickle (f : Stream -> 'T) (data : byte []) : 'T =
            use mem = new MemoryStream(data)
            f mem


        /// walks up the type hierarchy, gathering all instance fields
        let gatherFields (t : Type) =
            // resolve conflicts, index by declaring type and field name
            let gathered = ref Map.empty<string * string, FieldInfo>

            let rec gather (t : Type) =
                let fields = t.GetFields(allFields)
                for f in fields do
                    let k = f.DeclaringType.AssemblyQualifiedName, f.Name
                    if not <| gathered.Value.ContainsKey k then
                        gathered := gathered.Value.Add(k, f)

                match t.BaseType with
                | null -> ()
                | t when t = typeof<obj> -> ()
                | bt -> gather bt

            do gather t

            gathered.Value |> Map.toArray |> Array.map snd


        // perform a shallow copy of the contents of given reference type
        let shallowCopy (t : Type) (src : obj) (dst : obj) =
            let fields = gatherFields t
            for f in fields do
                let v = f.GetValue(src)
                f.SetValue(dst, v)

        //
        // Let 't1 -> t2' be the binary relation between types that denotes the statement 't1 contans a field of type t2'.
        // A type t is defined as being *cyclic* iff either of the following properties hold:
        //     a) t is not sealed or ISerializable,
        //     b) there exists t -> t' such that t' is cyclic
        //     c) there exists a chain (t -> t1 -> ... -> tn) so that t <: tn
        //
        // A type is cyclic iff its instances admit cyclic object graphs.
        //
        // F# union types are treated specially since cyclic bindings cannot be created under normal circumstances
        // for instance, the
        //
        //      type Rec = Rec of Rec
        //
        // is flagged as non-cyclic since defining instances of this type is impossible in F#.
        // However,
        //
        //     type Rec = { Rec : Rec }
        //
        // is flagged as cyclic since a recursive definition is actually possible in F#.
        // Finally, the
        //
        //     type Func = Func of (int -> int)
        //
        // is flagged as cyclic since cyclic bindings *can* be made, for example
        // `let rec f = Func (fun x -> let (Func f0) = f in f0 x + 1)`
        let isCyclicType (excludeUnionRecTypes : bool) (t : Type) =
            let rec aux d (traversed : (int * bool * bool * Type) list) (t : Type) =

                if t.IsValueType then false
                elif typeof<MemberInfo>.IsAssignableFrom t then false
                elif isISerializable t then true
                else

                let recAncestors = traversed |> List.filter (fun (_,_,_,t') -> t.IsAssignableFrom t') 

                if recAncestors.Length > 0 then
                    if excludeUnionRecTypes then
                        // recursive F# union types marked as 'cyclic' only if intertwined with non-union(mutable) types
                        // e.g. 'type Peano = Zero | Succ of Peano ref' is cyclic
                        let isCyclicType (d : int, isUnion : bool, _, _ : Type) =
                            if isUnion then
                                traversed |> List.exists (fun (i,_,isMutable,_) -> i > d && isMutable)
                            else
                                true
                                

                        List.exists isCyclicType recAncestors
                    else
                        true

                elif t.IsArray || t.IsByRef || t.IsPointer then
                    aux (d+1) ((d,false,true,t) :: traversed) <| t.GetElementType()
                elif FSharpType.IsUnion(t, allMembers) then
                    FSharpType.GetUnionCases(t, allMembers)
                    |> Seq.map (fun u -> u.GetFields() |> Seq.map (fun p -> p.PropertyType))
                    |> Seq.concat
                    |> Seq.distinct
                    |> Seq.exists (aux (d+1) ((d,true,false,t) :: traversed))
#if OPTIMIZE_FSHARP
                // System.Tuple is not sealed, but inheriting is not an idiomatic pattern in F#
                elif FSharpType.IsTuple t then
                    t.GetFields(allFields)
                    |> Seq.map (fun f -> f.FieldType)
                    |> Seq.distinct
                    |> Seq.exists (aux (d+1) ((d,false,false,t) :: traversed))
#endif
                elif FSharpType.IsRecord(t, allMembers) then
                    FSharpType.GetRecordFields(t, allMembers)
                    |> Seq.map (fun p -> p.CanWrite, p.PropertyType)
                    |> Seq.distinct
                    |> Seq.exists (fun (isMutable, t') -> aux (d+1) ((d,false,isMutable,t) :: traversed) t')

                // leaves with open hiearchies are treated as cyclic by definition
                elif not t.IsSealed then true
                else
                    gatherFields t
                    |> Seq.map (fun f -> f.FieldType)
                    |> Seq.distinct
                    |> Seq.exists (aux (d+1) ((d,false,true,t) :: traversed))

            aux 0 [] t


        //
        //  types like int * bool, int option, string, etc have object graphs of fixed scale
        //  types like arrays, rectypes, or non-sealed types can have instances of arbitrary graph size
        //

        let rec isOfFixedSize (t : Type) =
            if t.IsPrimitive then true
            elif t = typeof<string> then true
            elif typeof<MemberInfo>.IsAssignableFrom t then true

            elif t.IsArray then false
            elif isCyclicType false t then false
            elif FSharpType.IsUnion(t, allMembers) then
                FSharpType.GetUnionCases(t, allMembers)
                |> Seq.collect(fun u -> u.GetFields())
                |> Seq.distinct
                |> Seq.forall(fun f -> isOfFixedSize f.PropertyType)
            else
                gatherFields t
                |> Seq.distinct
                |> Seq.forall (fun f -> isOfFixedSize f.FieldType)

    [<AutoOpen>]
    module internal Extensions =
        open System
        open System.IO
        open System.Reflection
        open System.Linq
        open System.Linq.Expressions
        open System.Text

        [<Sealed>]
        type private NonClosingStreamWrapper(s: Stream) =
            inherit Stream()

            let mutable closed = false
            let ns () = raise (NotSupportedException())

            let cc () =
                if closed then
                    failwith "NonClosingStreamWrapper has been closed or disposed"

            override w.BeginRead(a, b, c, d, e) = cc (); s.BeginRead(a, b, c, d, e)
            override w.BeginWrite(a, b, c, d, e) = cc (); s.BeginWrite(a, b, c, d, e)

            override w.Close() =
                if not closed then
                    s.Flush()
                    closed <- true

            override w.CreateObjRef(t) = ns ()
            override w.EndRead(x) = cc (); s.EndRead(x)
            override w.EndWrite(x) = cc (); s.EndWrite(x)
            override w.Flush() = cc (); s.Flush()
            override w.InitializeLifetimeService() = ns ()
            override w.Read(a, b, c) = cc (); s.Read(a, b, c)
            override w.ReadByte() = cc (); s.ReadByte()
            override w.Seek(a, b) = cc (); s.Seek(a, b)
            override w.SetLength(a) = cc (); s.SetLength(a)
            override w.Write(a, b, c) = cc (); s.Write(a, b, c)
            override w.WriteByte(a) = cc (); s.WriteByte(a)
            override w.CanRead = if closed then false else s.CanRead
            override w.CanSeek = if closed then false else s.CanSeek
            override w.CanTimeout = if closed then false else s.CanTimeout
            override w.CanWrite = if closed then false else s.CanWrite
            override w.Length = cc (); s.Length

            override w.Position
                with get () = cc (); s.Position
                and set x = cc (); s.Position <- x

            override w.ReadTimeout = cc (); s.ReadTimeout
            override w.WriteTimeout = cc (); s.WriteTimeout

        type private BinaryPortFactory<'T> =
            delegate of Stream * Encoding * bool -> 'T

        let inline private getBinaryPortFactory<'T> (make: Stream -> Encoding -> 'T) =
            let types = [| typeof<Stream>; typeof<Encoding>; typeof<bool> |]
            match typeof<'T>.GetConstructor(types) with
            | null ->
                BinaryPortFactory<'T>(fun s e o ->
                    if o then
                        make (new NonClosingStreamWrapper(s)) e
                    else
                        make s e)
            | ctor ->
                let parameters = [for t in types -> Expression.Parameter(t)]
                let ctor = typeof<'T>.GetConstructor(types)
                let body = Expression.New(ctor, [for p in parameters -> p :> Expression])
                Expression.Lambda<BinaryPortFactory<'T>>(body, parameters).Compile()

        let private binaryReaderFactory = getBinaryPortFactory (fun s e -> new BinaryReader(s, e))
        let private binaryWriterFactory = getBinaryPortFactory (fun s e -> new BinaryWriter(s, e))

        type BinaryReader with
            static member Create(stream, encoding, leaveOpen) =
                binaryReaderFactory.Invoke(stream, encoding, leaveOpen)

        type BinaryWriter with
            static member Create(stream, encoding, leaveOpen) =
                binaryWriterFactory.Invoke(stream, encoding, leaveOpen)
