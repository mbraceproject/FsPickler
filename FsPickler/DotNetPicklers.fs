module internal FsPickler.DotNetPicklers

    open System
    open System.IO
    open System.Reflection
    open System.Threading
#if EMIT_IL
    open System.Linq.Expressions
#endif
    open System.Runtime.Serialization

    open FsPickler
    open FsPickler.Utils
    open FsPickler.PicklerUtils
    open FsPickler.BasePicklers

    // pickler rules for array types

    type ArrayPickler =

        static member CreateUntyped(t : Type, resolver : IPicklerResolver) =
            let et = t.GetElementType()
            let ef = resolver.Resolve et
            let m =
                typeof<ArrayPickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| et ; t |]

            m.GuardedInvoke(null, [| ef :> obj |]) :?> Pickler

        static member Create<'T, 'Array when 'Array :> Array> (ef : Pickler<'T>) : Pickler<'Array> =
            assert(typeof<'T> = typeof<'Array>.GetElementType())
            let rank = typeof<'Array>.GetArrayRank()

            let writer (w : Writer) (x : 'Array) =

                for d = 0 to rank - 1 do
                    w.BinaryWriter.Write(x.GetLength d)

                if ef.TypeInfo = TypeInfo.Primitive then
                    Stream.WriteArray(w.BinaryWriter.BaseStream, x)
                else
                    let isValue = ef.TypeInfo <= TypeInfo.Value
                             
                    match rank with
                    | 1 ->
                        let x = fastUnbox<'T []> x
                        for i = 0 to x.Length - 1 do
                            write isValue w ef x.[i]
                    | 2 -> 
                        let x = fastUnbox<'T [,]> x
                        for i = 0 to x.GetLength(0) - 1 do
                            for j = 0 to x.GetLength(1) - 1 do
                                write isValue w ef x.[i,j]
                    | 3 ->
                        let x = fastUnbox<'T [,,]> x
                        for i = 0 to x.GetLength(0) - 1 do
                            for j = 0 to x.GetLength(1) - 1 do
                                for k = 0 to x.GetLength(2) - 1 do
                                    write isValue w ef x.[i,j,k]
                    | 4 ->
                        let x = fastUnbox<'T [,,,]> x
                        for i = 0 to x.GetLength(0) - 1 do
                            for j = 0 to x.GetLength(1) - 1 do
                                for k = 0 to x.GetLength(2) - 1 do
                                    for l = 0 to x.GetLength(3) - 1 do
                                        write isValue w ef x.[i,j,k,l]
                    | _ -> failwith "impossible array rank"

            let reader (r : Reader) =
                let l = Array.zeroCreate<int> rank
                for i = 0 to rank - 1 do l.[i] <- r.BinaryReader.ReadInt32()

                if ef.TypeInfo = TypeInfo.Primitive then
                    let array =
                        match rank with
                        | 1 -> Array.zeroCreate<'T> l.[0] :> Array
                        | 2 -> Array2D.zeroCreate<'T> l.[0] l.[1] :> Array
                        | 3 -> Array3D.zeroCreate<'T> l.[0] l.[1] l.[2] :> Array
                        | 4 -> Array4D.zeroCreate<'T> l.[0] l.[1] l.[2] l.[3] :> Array
                        | _ -> failwith "impossible array rank"

                    r.EarlyRegisterObject array

                    Stream.CopyToArray(r.BinaryReader.BaseStream, array)

                    fastUnbox<'Array> array
                else
                    let isValue = ef.TypeInfo <= TypeInfo.Value

                    match rank with
                    | 1 -> 
                        let arr = Array.zeroCreate<'T> l.[0]
                        r.EarlyRegisterObject arr
                        for i = 0 to l.[0] - 1 do
                            arr.[i] <- read isValue r ef

                        fastUnbox<'Array> arr
                    | 2 -> 
                        let arr = Array2D.zeroCreate<'T> l.[0] l.[1]
                        r.EarlyRegisterObject arr
                        for i = 0 to l.[0] - 1 do
                            for j = 0 to l.[1] - 1 do
                                arr.[i,j] <- read isValue r ef

                        fastUnbox<'Array> arr
                    | 3 ->
                        let arr = Array3D.zeroCreate<'T> l.[0] l.[1] l.[2]
                        r.EarlyRegisterObject arr
                        for i = 0 to l.[0] - 1 do
                            for j = 0 to l.[1] - 1 do
                                for k = 0 to l.[2] - 1 do
                                    arr.[i,j,k] <- read isValue r ef

                        fastUnbox<'Array> arr
                    | 4 ->
                        let arr = Array4D.zeroCreate<'T> l.[0] l.[1] l.[2] l.[3]
                        r.EarlyRegisterObject arr
                        for i = 0 to l.[0] - 1 do
                            for j = 0 to l.[1] - 1 do
                                for k = 0 to l.[2] - 1 do
                                    for l = 0 to l.[3] - 1 do
                                        arr.[i,j,k,l] <- read isValue r ef

                        fastUnbox<'Array> arr
                    | _ -> failwith "impossible array rank"

            new Pickler<'Array>(reader, writer, PicklerInfo.Array, cacheByRef = true, useWithSubtypes = false)

    // pickler builder for ISerializable types

    type ISerializablePickler =

        static member TryCreateUntyped(t : Type, resolver : IPicklerResolver) =
            let m =
                typeof<ISerializablePickler>
                    .GetMethod("TryCreate", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Pickler option

        static member TryCreate<'T when 'T :> ISerializable>(resolver : IPicklerResolver) =
            match typeof<'T>.TryGetConstructor [| typeof<SerializationInfo> ; typeof<StreamingContext> |] with
            | None -> None
            | Some ctorInfo ->
                let allMethods = typeof<'T>.GetMethods(allMembers)
                let onSerializing = allMethods |> getSerializationMethods< OnSerializingAttribute>
                let onSerialized = allMethods |> getSerializationMethods<OnSerializedAttribute>
//                let onDeserializing = allMethods |> getSerializationMethods<OnDeserializingAttribute>
                let onDeserialized = allMethods |> getSerializationMethods<OnDeserializedAttribute>

                let isDeserializationCallback = typeof<IDeserializationCallback>.IsAssignableFrom typeof<'T>
#if EMIT_IL
                let inline run (dele : Action<StreamingContext, 'T> option) w x =
                    match dele with
                    | None -> ()
                    | Some d -> d.Invoke(getStreamingContext w, x)

                let onSerializing = Expression.preComputeSerializationMethods<'T> onSerializing
                let onSerialized = Expression.preComputeSerializationMethods<'T> onSerialized
                let onDeserialized = Expression.preComputeSerializationMethods<'T> onDeserialized

                let ctor =
                    Expression.compileFunc2<SerializationInfo, StreamingContext, 'T>(fun si sc -> 
                        Expression.New(ctorInfo, si, sc) :> _)

                let inline create si sc = ctor.Invoke(si, sc)
#else
                let inline run (ms : MethodInfo []) w o  =
                    for i = 0 to ms.Length - 1 do ms.[i].Invoke(o, [| getStreamingContext w :> obj |]) |> ignore

                let inline create (si : SerializationInfo) (sc : StreamingContext) = 
                    ctorInfo.Invoke [| si :> obj ; sc :> obj |] |> fastUnbox<'T>
#endif
                let writer (w : Writer) (x : 'T) =
                    run onSerializing w x
                    let sI = new SerializationInfo(typeof<'T>, new FormatterConverter())
                    x.GetObjectData(sI, w.StreamingContext)
                    w.BinaryWriter.Write sI.MemberCount
                    let enum = sI.GetEnumerator()
                    while enum.MoveNext() do
                        w.BinaryWriter.Write enum.Current.Name
                        w.Write<obj> enum.Current.Value

                    run onSerialized w x

                let reader (r : Reader) =
                    let sI = new SerializationInfo(typeof<'T>, new FormatterConverter())
                    let memberCount = r.BinaryReader.ReadInt32()
                    for i = 1 to memberCount do
                        let name = r.BinaryReader.ReadString()
                        let v = r.Read<obj> ()
                        sI.AddValue(name, v)

                    let x = create sI r.StreamingContext

                    run onDeserialized r x
                    if isDeserializationCallback then (fastUnbox<IDeserializationCallback> x).OnDeserialization null
                    x

                let fmt = new Pickler<'T>(reader, writer, PicklerInfo.ISerializable, cacheByRef = true, useWithSubtypes = false)
                Some(fmt :> Pickler)


    //  check if type implements a static factory method : IPicklerResolver -> Pickler<DeclaringType>

    type CustomPickler =
        static member Create(t : Type, resolver : IPicklerResolver) =
            let factoryMethod =
                match t.GetMethod("CreatePickler", BindingFlags.Public ||| BindingFlags.Static) with
                | null -> None
                | m when    not m.IsGenericMethod &&
                            m.GetParameterTypes() = [| typeof<IPicklerResolver> |] && 
                            m.ReturnType = typedefof<Pickler<_>>.MakeGenericType [| t |] -> 
                    Some m

                | _ -> None

            match factoryMethod with
            | Some m -> m.GuardedInvoke(null, [| resolver :> obj |]) :?> Pickler
            | None ->
                let msg = "marked [<CustomPickler>] but missing factory method 'CreatePickler : IPicklerResolver -> Pickler<DeclaringType>'."
                raise <| new NonSerializableTypeException(t, msg)