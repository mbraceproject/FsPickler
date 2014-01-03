module internal FsPickler.DotNetPicklers

    open System
    open System.IO
    open System.Reflection
    open System.Threading
#if EMIT_IL
    open System.Reflection.Emit
    open FsPickler.Emit
#endif
    open System.Runtime.Serialization

    open FsPickler
    open FsPickler.Utils
    open FsPickler.PicklerUtils
    open FsPickler.BasePicklers

    let isUnSupportedType (t : Type) =
        t.IsPointer 
        || t = typeof<System.Reflection.Pointer>
        || t.IsByRef
        || t.IsCOMObject
        || t.IsImport
        || t.IsMarshalByRef
        || t.IsGenericParameter
        || t.IsGenericTypeDefinition
        || t.IsPrimitive // supported primitives should be already stored in the pickler cache        
        || t = Type.GetType("System.__Canon")


    // creates a placeholder pickler instance

    type UninitializedPickler =
        static member Create<'T>() = new Pickler<'T>()
        static member CreateUntyped (t : Type) =
            if isUnSupportedType t then raise <| NonSerializableTypeException t

            let m =
                typeof<UninitializedPickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            m.GuardedInvoke(null, null) :?> Pickler

    // abstract type pickler factory

    type AbstractPickler =
        static member Create<'T> () =
            let writer _ _ = invalidOp <| sprintf "Attempting to call abstract pickler '%O'." typeof<'T>
            let reader _ = invalidOp <| sprintf "Attempting to call abstract pickler '%O'." typeof<'T>

            new Pickler<'T>(reader, writer, PicklerInfo.ReflectionDerived, true, false)

        static member CreateUntyped(t : Type) =
            let m = 
                typeof<AbstractPickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            m.GuardedInvoke(null, null) :?> Pickler

    // pickler combinator for enum types

    type EnumPickler =
        static member CreateUntyped(enum : Type, resolver : IPicklerResolver) =
            let underlying = enum.GetEnumUnderlyingType()
            // reflection call typed method
            let m = 
                typeof<EnumPickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| enum ; underlying |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Pickler

        static member Create<'Enum, 'Underlying when 'Enum : enum<'Underlying>> (resolver : IPicklerResolver) =
            let fmt = resolver.Resolve<'Underlying> ()
            let writer_func = fmt.Write
            let reader_func = fmt.Read

            let writer (w : Writer) (x : 'Enum) =
                let value = Microsoft.FSharp.Core.LanguagePrimitives.EnumToValue<'Enum, 'Underlying> x
                writer_func w value

            let reader (r : Reader) =
                let value = reader_func r
                Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<'Underlying, 'Enum> value

            new Pickler<'Enum>(reader, writer, PicklerInfo.ReflectionDerived, cacheByRef = false, useWithSubtypes = false)


    // support for nullable types

    type NullablePickler =
        static member CreateUntyped(t : Type, resolver : IPicklerResolver) =
            let underlying = t.GetGenericArguments().[0]
            // reflection call typed method
            let m = 
                typeof<NullablePickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| underlying |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Pickler

        static member Create<'T when 
                                'T : (new : unit -> 'T) and 
                                'T : struct and 
                                'T :> ValueType> (resolver : IPicklerResolver) =

            let fmt = resolver.Resolve<'T> ()
            let writer_func = fmt.Write
            let reader_func = fmt.Read

            let writer (w : Writer) (x : Nullable<'T>) =
                if x.HasValue then 
                    w.BinaryWriter.Write true
                    writer_func w x.Value
                else
                    w.BinaryWriter.Write false

            let reader (r : Reader) =
                if r.BinaryReader.ReadBoolean () then
                    let value = reader_func r
                    Nullable<'T>(value)
                else
                    Nullable<'T> ()

            new Pickler<_>(reader, writer, PicklerInfo.ReflectionDerived, cacheByRef = false, useWithSubtypes = false)

    // pickler combinator for struct types

    type StructPickler =
        static member CreateUntyped(t : Type, resolver : IPicklerResolver) =
            let m = 
                typeof<StructPickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Pickler

        static member Create<'T when 'T : struct>(resolver : IPicklerResolver) =

            let fields = gatherFields typeof<'T>
            let picklers = fields |> Array.map (fun f -> resolver.Resolve f.FieldType)

#if EMIT_IL
            let writerDele =
                DynamicMethod.compileAction3<Pickler [], Writer, 'T> "structSerializer" (fun ilGen ->
                    let picklers = EnvItem<Pickler []>.Arg0
                    let writer = EnvItem<Writer>.Arg1
                    let parent = EnvItem<'T>.Arg2

                    emitSerializeFields fields writer picklers parent ilGen

                    ilGen.Emit OpCodes.Ret
                )

            let readerDele =
                DynamicMethod.compileFunc2<Pickler [], Reader, 'T> "structDeserializer" (fun ilGen ->
                    let picklers = EnvItem<Pickler []>.Arg0
                    let reader = EnvItem<Reader>.Arg1
                    
                    // initialize empty value type
                    let value = EnvItem<'T>.InitVar ilGen
                    emitObjectInitializer typeof<'T> ilGen
                    value.Store ilGen

                    emitDeserializeFields fields reader picklers value ilGen

                    value.Load ilGen
                    ilGen.Emit OpCodes.Ret
                )

            let writer (w : Writer) (t : 'T) = writerDele.Invoke(picklers, w, t)
            let reader (r : Reader) = readerDele.Invoke(picklers, r)

#else
            let writer (w : Writer) (t : 'T) =
                for i = 0 to fields.Length - 1 do
                    let o = fields.[i].GetValue(t)
                    picklers.[i].UntypedWrite(w, o, managed = true)

            let reader (r : Reader) =
                let t = FormatterServices.GetUninitializedObject(typeof<'T>)
                for i = 0 to fields.Length - 1 do
                    let o = picklers.[i].UntypedRead(r, managed = true)
                    fields.[i].SetValue(t, o)
                
                fastUnbox<'T> t
#endif

            new Pickler<'T>(reader, writer, PicklerInfo.ReflectionDerived, cacheByRef = false, useWithSubtypes = false)
                    

    // general-purpose pickler combinator for reference types

    type ClassPickler =

        static member CreateUntyped(t : Type, resolver : IPicklerResolver) =
            let m =
                typeof<ClassPickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Pickler

        static member Create<'T when 'T : not struct>(resolver : IPicklerResolver) =
            let fields = 
                gatherFields typeof<'T>
                |> Array.filter (not << containsAttr<NonSerializedAttribute>)

            let picklers = fields |> Array.map (fun f -> resolver.Resolve f.FieldType)

            let isDeserializationCallback = typeof<IDeserializationCallback>.IsAssignableFrom typeof<'T>

            let allMethods = typeof<'T>.GetMethods(allMembers)
            let onSerializing = allMethods |> getSerializationMethods<OnSerializingAttribute>
            let onSerialized = allMethods |> getSerializationMethods<OnSerializedAttribute>
            let onDeserializing = allMethods |> getSerializationMethods<OnDeserializingAttribute>
            let onDeserialized = allMethods |> getSerializationMethods<OnDeserializedAttribute>

#if EMIT_IL
            let writer =
                if onSerializing.Length = 0 && fields.Length = 0 && onSerialized.Length = 0 then
                    fun _ _ -> ()
                else
                    let writerDele =
                        DynamicMethod.compileAction3<Pickler [], Writer, 'T> "classSerializer" (fun ilGen ->
                            let picklers = EnvItem<Pickler []>.Arg0
                            let writer = EnvItem<Writer>.Arg1
                            let value = EnvItem<'T>.Arg2

                            emitSerializationMethodCalls onSerializing (Choice1Of2 writer) value ilGen

                            emitSerializeFields fields writer picklers value ilGen

                            emitSerializationMethodCalls onSerialized (Choice1Of2 writer) value ilGen
                            
                            ilGen.Emit OpCodes.Ret)

                    fun w t -> writerDele.Invoke(picklers, w, t)

            let readerDele =
                DynamicMethod.compileFunc2<Pickler [], Reader, 'T> "classDeserializer" (fun ilGen ->
                    let picklers = EnvItem<Pickler []>.Arg0
                    let reader = EnvItem<Reader>.Arg1

                    // get uninitialized object and store locally
                    let value = EnvItem<'T>.InitVar ilGen
                    emitObjectInitializer typeof<'T> ilGen
                    value.Store ilGen

                    emitSerializationMethodCalls onDeserializing (Choice2Of2 reader) value ilGen

                    emitDeserializeFields fields reader picklers value ilGen

                    emitSerializationMethodCalls onDeserialized (Choice2Of2 reader) value ilGen

                    if isDeserializationCallback then emitDeserializationCallback value ilGen

                    value.Load ilGen
                    ilGen.Emit OpCodes.Ret
                )

            let reader r = readerDele.Invoke(picklers, r)
#else
            let inline run (ms : MethodInfo []) (x : obj) w =
                for i = 0 to ms.Length - 1 do 
                    ms.[i].Invoke(x, [| getStreamingContext w :> obj |]) |> ignore

            let writer (w : Writer) (t : 'T) =
                run onSerializing t w

                for i = 0 to fields.Length - 1 do
                    let o = fields.[i].GetValue(t)
                    picklers.[i].UntypedWrite(w, o, managed = true)

                run onSerialized t w

            let reader (r : Reader) =
                let t = FormatterServices.GetUninitializedObject(typeof<'T>) |> fastUnbox<'T>
                run onDeserializing t r

                for i = 0 to fields.Length - 1 do
                    let o = picklers.[i].UntypedRead(r, managed = true)
                    fields.[i].SetValue(t, o)

                run onDeserialized t r
                if isDeserializationCallback then (fastUnbox<IDeserializationCallback> t).OnDeserialization null
                t
#endif

            new Pickler<'T>(reader, writer, PicklerInfo.ReflectionDerived, cacheByRef = true, useWithSubtypes = false)


    // pickler implementation for delegate types

    type DelegatePickler =

        static member CreateUntyped(t : Type, resolver : IPicklerResolver) =
            let m =
                typeof<DelegatePickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Pickler

        static member Create<'Delegate when 'Delegate :> Delegate> (resolver : IPicklerResolver) =
            let objPickler = resolver.Resolve<obj> ()
            let memberInfoPickler = resolver.Resolve<MethodInfo> ()
            let delePickler = resolver.Resolve<System.Delegate> ()

            let writer (w : Writer) (dele : 'Delegate) =
                match dele.GetInvocationList() with
                | [| _ |] ->
                    w.BinaryWriter.Write true
                    w.Write(memberInfoPickler, dele.Method)
                    if not dele.Method.IsStatic then w.Write(objPickler, dele.Target)
                | deleList ->
                    w.BinaryWriter.Write false
                    w.BinaryWriter.Write deleList.Length
                    for i = 0 to deleList.Length - 1 do
                        w.Write<System.Delegate> (delePickler, deleList.[i])

            let reader (r : Reader) =
                if r.BinaryReader.ReadBoolean() then
                    let meth = r.Read memberInfoPickler
                    if not meth.IsStatic then
                        let target = r.Read objPickler
                        Delegate.CreateDelegate(typeof<'Delegate>, target, meth, throwOnBindFailure = true) |> fastUnbox<'Delegate>
                    else
                        Delegate.CreateDelegate(typeof<'Delegate>, meth, throwOnBindFailure = true) |> fastUnbox<'Delegate>
                else
                    let n = r.BinaryReader.ReadInt32()
                    let deleList = Array.zeroCreate<System.Delegate> n
                    for i = 0 to n - 1 do deleList.[i] <- r.Read delePickler
                    Delegate.Combine deleList |> fastUnbox<'Delegate>

            new Pickler<'Delegate>(reader, writer, PicklerInfo.Delegate, cacheByRef = true, useWithSubtypes = false)

    // pickler combinator for ISerializable types

    type ISerializablePickler =

        static member CreateUntyped(t : Type, resolver : IPicklerResolver) =
            let m =
                typeof<ISerializablePickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Pickler

        static member Create<'T when 'T :> ISerializable>(resolver : IPicklerResolver) =
            match typeof<'T>.TryGetConstructor [| typeof<SerializationInfo> ; typeof<StreamingContext> |] with
            | None -> raise <| new NonSerializableTypeException(typeof<'T>, "is ISerializable but does not implement constructor with parameters (SerializationInfo, StreamingContext).")
            | Some ctorInfo ->
                let allMethods = typeof<'T>.GetMethods(allMembers)
                let onSerializing = allMethods |> getSerializationMethods<OnSerializingAttribute> |> mkDelegates<'T>
                let onSerialized = allMethods |> getSerializationMethods<OnSerializedAttribute> |> mkDelegates<'T>
                let onDeserialized = allMethods |> getSerializationMethods<OnDeserializedAttribute> |> mkDelegates<'T>

                let isDeserializationCallback = typeof<IDeserializationCallback>.IsAssignableFrom typeof<'T>

                let objPickler = resolver.Resolve<obj> ()

                let inline run (dele : Action<'T, StreamingContext> []) w x =
                    for d in dele do
                        d.Invoke(x, getStreamingContext w)

#if EMIT_IL
                let ctorDele = wrapISerializableConstructor<'T> ctorInfo

                let inline create si sc = ctorDele.Invoke(si, sc)
#else
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
                        w.Write(objPickler, enum.Current.Value)

                    run onSerialized w x

                let reader (r : Reader) =
                    let sI = new SerializationInfo(typeof<'T>, new FormatterConverter())
                    let memberCount = r.BinaryReader.ReadInt32()
                    for i = 1 to memberCount do
                        let name = r.BinaryReader.ReadString()
                        let v = r.Read objPickler
                        sI.AddValue(name, v)

                    let x = create sI r.StreamingContext

                    run onDeserialized r x
                    if isDeserializationCallback then (fastUnbox<IDeserializationCallback> x).OnDeserialization null
                    x

                new Pickler<'T>(reader, writer, PicklerInfo.ISerializable, cacheByRef = true, useWithSubtypes = false)


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