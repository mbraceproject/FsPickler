namespace FsPickler

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

        /// thread-safe memo operator
        let memoize (f : 'K -> 'V) =
            let d = new ConcurrentDictionary<'K, 'V> ()

            fun (k : 'K) ->
                let found, value = d.TryGetValue k
                if found then value
                else
                    let v = f k
                    let _ = d.TryAdd(k, v)
                    v


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

        // perform a shallow copy of the contents of given reference type
        let shallowCopy (t : Type) (src : obj) (dst : obj) =
            let fields = t.GetFields(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
            for f in fields do
                let v = f.GetValue(src)
                f.SetValue(dst, v)


        let inline pickle (f : Stream -> 'T -> unit) (x : 'T) : byte [] =
            use mem = new MemoryStream()
            f mem x
            mem.ToArray()

        let inline unpickle (f : Stream -> 'T) (data : byte []) : 'T =
            use mem = new MemoryStream(data)
            f mem


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
                    t.GetFields(allFields)
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
                t.GetFields(allFields)
                |> Seq.distinct
                |> Seq.forall (fun f -> isOfFixedSize f.FieldType)