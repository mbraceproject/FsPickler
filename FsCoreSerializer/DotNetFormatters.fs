module internal FsCoreSerializer.DotNetFormatters

    open System
    open System.Reflection
    open System.Runtime.Serialization
    open System.Linq.Expressions

    open FsCoreSerializer
    open FsCoreSerializer.Expression
    open FsCoreSerializer.FormatterUtils

    //
    //  general purpose .NET serialization formatters
    //

    let isUnSupportedType (t : Type) =
        t.IsPointer
        || t.IsByRef
        || t.IsCOMObject
        || t.IsImport
        || t.IsMarshalByRef
        || t.IsPrimitive // supported primitives are already stored in the formatter cache

    // dummy formatter placeholder for abstract types
    let mkAbstractFormatter (t : Type) =
        {
            Type = t
            TypeInfo = getTypeInfo t
            TypeHash = 0us

            Write = fun _ _ -> invalidOp <| sprintf "Attempting to use formatter for abstract type '%O'." t
            Read = fun _ -> invalidOp <| sprintf "Attempting to use formatter for abstract type '%O'." t

            FormatterInfo = FormatterInfo.ReflectionDerived
            UseWithSubtypes = false
            CacheObj = true
        }

    // formatter builder for ISerializable types
    let tryMkISerializableFormatter (t : Type) =
        if not <| typeof<ISerializable>.IsAssignableFrom t then None
        else
            match tryGetCtor t [| typeof<SerializationInfo> ; typeof<StreamingContext> |] with
            | None -> None
            | Some ctorInfo ->

                let allMethods = t.GetMethods(memberBindings) |> Array.filter isSerializationMethod
                let onSerializing = allMethods |> Array.filter containsAttr<OnSerializingAttribute>
                let onSerialized = allMethods |> Array.filter containsAttr<OnSerializedAttribute>
//                let onDeserializing = allMethods |> Array.filter containsAttr<OnDeserializingAttribute>
                let onDeserialized = allMethods |> Array.filter containsAttr<OnDeserializedAttribute>

                let isDeserializationCallback = typeof<IDeserializationCallback>.IsAssignableFrom t
#if EMIT_IL
                let inline run (dele : Func<obj, StreamingContext, unit> option) o sc =
                    match dele with
                    | None -> ()
                    | Some d -> d.Invoke(o, sc)

                let onSerializing = preComputeSerializationMethods t onSerializing
                let onSerialized = preComputeSerializationMethods t onSerialized
                let onDeserialized = preComputeSerializationMethods t onDeserialized

                let ctor =
                    Expression.compile2<SerializationInfo, StreamingContext, obj>(fun si sc -> 
                        Expression.New(ctorInfo, si, sc) |> Expression.box)

                let inline create si sc = ctor.Invoke(si, sc)
#else
                let inline run (ms : MethodInfo []) o (sc : StreamingContext)  =
                    for i = 0 to ms.Length - 1 do ms.[i].Invoke(o, [| sc :> obj |]) |> ignore

                let inline create (si : SerializationInfo) (sc : StreamingContext) = 
                    ctorInfo.Invoke [| si :> obj ; sc :> obj |]
#endif
                let writer (w : Writer) (o : obj) =
                    run onSerializing o w.StreamingContext
                    let s = o :?> ISerializable
                    let sI = new SerializationInfo(t, new FormatterConverter())
                    s.GetObjectData(sI, w.StreamingContext)
                    w.BW.Write sI.MemberCount
                    let enum = sI.GetEnumerator()
                    while enum.MoveNext() do
                        w.BW.Write enum.Current.Name
                        w.WriteObj enum.Current.Value

                    run onSerialized o w.StreamingContext

                let reader (r : Reader) =
                    let sI = new SerializationInfo(t, new FormatterConverter())
                    let memberCount = r.BR.ReadInt32()
                    for i = 1 to memberCount do
                        let name = r.BR.ReadString()
                        let v = r.ReadObj ()
                        sI.AddValue(name, v)

                    let o = create sI r.StreamingContext

                    run onDeserialized o r.StreamingContext
                    if isDeserializationCallback then (o :?> IDeserializationCallback).OnDeserialization null
                    o

                Some {
                    Type = t
                    TypeInfo = getTypeInfo t
                    TypeHash = 0us

                    Write = writer
                    Read = reader

                    FormatterInfo = FormatterInfo.ISerializable
                    CacheObj = true
                    UseWithSubtypes = false
                }


    // formatter builder for IFsCoreSerializable types
    let tryMkIFsCoreSerializable (t : Type) =
        if not <| typeof<IFsCoreSerializable>.IsAssignableFrom t then None
        else
            match tryGetCtor t [| typeof<Reader> |] with
            | None -> None
            | Some ctorInfo ->
#if EMIT_IL
                let reader = Expression.compile1<Reader, obj>(fun reader -> Expression.New(ctorInfo, reader) |> Expression.box).Invoke
#else
                let reader (r : Reader) = ctorInfo.Invoke [| r :> obj |]
#endif
                Some {
                    Type = t
                    TypeInfo = getTypeInfo t
                    TypeHash = 0us

                    Write = fun (w : Writer) (o : obj) -> (o :?> IFsCoreSerializable).GetObjectData(w)
                    Read = reader

                    FormatterInfo = FormatterInfo.IFsCoreSerializable
                    UseWithSubtypes = false
                    CacheObj = true
                }

    // serialization rules for struct types
    let mkStructFormatter (resolver : Type -> Lazy<Formatter>) (t : Type) =

        let fields = t.GetFields(fieldBindings)
        let formatters = fields |> Array.map (fun f -> resolver f.FieldType)

#if EMIT_IL
        let fieldReader = 
            Expression.compile1<obj, obj []>(fun boxedExpr ->
                let unboxed = Expression.unbox t boxedExpr
                Expression.readFieldsBoxed t fields unboxed :> _)

        let fieldSetter =
            Expression.compile2<obj, obj [], obj>(fun boxedExpr values ->
                let unboxed = Expression.unbox t boxedExpr
                let assignment = Expression.writeFieldsBoxed t fields unboxed values
                Expression.Block(assignment, Expression.box unboxed) :> _)

        let inline readFields o = fieldReader.Invoke o
        let inline writeFields o values = fieldSetter.Invoke(o, values)
#else
        let inline readFields (o : obj) =
            let n = fields.Length
            let values = Array.zeroCreate<obj> n
            for i = 0 to n - 1 do
                values.[i] <- fields.[i].GetValue o
            values

        let inline writeFields (o : obj) (values : obj []) =
            for i = 0 to fields.Length - 1 do
                fields.[i].SetValue(o, values.[i])
            o
            
#endif
        let writer (w : Writer) (o : obj) =
            let values = readFields o
            do zipWrite w formatters values

        let reader (r : Reader) =
            let o = FormatterServices.GetUninitializedObject(t)
            let values = zipRead r formatters
            writeFields o values

        {
            Type = t
            TypeInfo = getTypeInfo t
            TypeHash = 0us

            Write = writer
            Read = reader

            FormatterInfo = FormatterInfo.ReflectionDerived
            UseWithSubtypes = false
            CacheObj = false
        }
            

    // reflection-based serialization rules for reference types
    let mkClassFormatter (resolver : Type -> Lazy<Formatter>) (t : Type) =
        let fields = t.GetFields(fieldBindings) |> Array.filter (not << containsAttr<NonSerializedAttribute>)
        let formatters = fields |> Array.map (fun f -> resolver f.FieldType)

        let isDeserializationCallback = typeof<IDeserializationCallback>.IsAssignableFrom t

        let allMethods = t.GetMethods(memberBindings)

        let onSerializing = allMethods |> Array.filter containsAttr<OnSerializingAttribute>
        let onSerialized = allMethods |> Array.filter containsAttr<OnSerializedAttribute>
        let onDeserializing = allMethods |> Array.filter containsAttr<OnDeserializingAttribute>
        let onDeserialized = allMethods |> Array.filter containsAttr<OnDeserializedAttribute>

#if EMIT_IL

        let inline run (dele : Func<obj, StreamingContext, unit> option) o sc =
            match dele with
            | None -> ()
            | Some d -> d.Invoke(o, sc)

        let onSerializing = preComputeSerializationMethods t onSerializing
        let onSerialized = preComputeSerializationMethods t onSerialized
        let onDeserializing = preComputeSerializationMethods t onDeserializing
        let onDeserialized = preComputeSerializationMethods t onDeserialized

        let fieldGetter =
            Expression.compile1<obj, obj []>(fun instance ->
                let unboxed = Expression.unbox t instance
                Expression.readFieldsBoxed t fields unboxed :> _)

        let fieldSetter =
            Expression.compile2<obj, obj [], unit>(fun instance fieldValues ->
                let unboxed = Expression.unbox t instance
                Expression.writeFieldsBoxed t fields unboxed fieldValues |> Expression.returnUnit)

        let inline readFields (o : obj) = fieldGetter.Invoke o
        let inline writeFields (o : obj) (values : obj []) = fieldSetter.Invoke(o, values)
#else
        let inline run (ms : MethodInfo []) o (sc : StreamingContext) =
            for i = 0 to ms.Length - 1 do 
                ms.[i].Invoke(o, [| sc :> obj |]) |> ignore

        let inline readFields (o : obj) = 
            let fieldVals = Array.zeroCreate<obj> fields.Length
            for i = 0 to fields.Length - 1 do
                fieldVals.[i] <- fields.[i].GetValue o

            fieldVals

        let inline writeFields (o : obj) (values : obj []) =
            for i = 0 to fields.Length - 1 do
                fields.[i].SetValue(o, values.[i])
#endif
        let writer (w : Writer) (o : obj) =
            run onSerializing o w.StreamingContext
            let values = readFields o
            zipWrite w formatters values
            run onSerialized o w.StreamingContext

        let reader (r : Reader) =
            let o = FormatterServices.GetUninitializedObject(t)
            do r.EarlyRegisterObject o
            run onDeserializing o r.StreamingContext
            let values = zipRead r formatters
            do writeFields o values
            run onDeserialized o r.StreamingContext
            if isDeserializationCallback then (o :?> IDeserializationCallback).OnDeserialization null
            o

        {
            Type = t
            TypeInfo = getTypeInfo t
            TypeHash = 0us

            Write = writer
            Read = reader

            FormatterInfo = FormatterInfo.ReflectionDerived
            UseWithSubtypes = false
            CacheObj = true
        }

    // formatter builder for enumerator types
    let mkEnumFormatter (resolver : Type -> Lazy<Formatter>) (t : Type) =
        let ut = Enum.GetUnderlyingType t
        let uf = (resolver ut).Value

        {
            Type = t
            TypeInfo = getTypeInfo t
            TypeHash = 0us

            Write = uf.Write
            Read = uf.Read

            FormatterInfo = FormatterInfo.ReflectionDerived
            UseWithSubtypes = false
            CacheObj = false
        }

    // formatter builder for delegate types
    let mkDelegateFormatter (self : Type -> Lazy<Formatter>) (t : Type) =
        let memberInfoFormatter = (self typeof<MemberInfo>).Value
        let writer (w : Writer) (o : obj) =
            let dele = o :?> System.Delegate
            match dele.GetInvocationList() with
            | [| _ |] ->
                w.BW.Write true
                w.WriteObj(memberInfoFormatter, dele.Method)
                if not dele.Method.IsStatic then w.WriteObj dele.Target
            | deleList ->
                w.BW.Write false
                w.BW.Write deleList.Length
                for i = 0 to deleList.Length - 1 do
                    w.WriteObj(deleList.[i])

        let reader (r : Reader) =
            if r.BR.ReadBoolean() then
                let meth = r.ReadObj memberInfoFormatter :?> MethodInfo
                if not meth.IsStatic then
                    let target = r.ReadObj()
                    Delegate.CreateDelegate(t, target, meth, throwOnBindFailure = true) :> obj
                else
                    Delegate.CreateDelegate(t, meth, throwOnBindFailure = true) :> obj
            else
                let n = r.BR.ReadInt32()
                let deleList = Array.zeroCreate<System.Delegate> n
                for i = 0 to n - 1 do deleList.[i] <- r.ReadObj() :?> System.Delegate
                Delegate.Combine deleList :> obj

        {
            Type = t
            TypeInfo = getTypeInfo t
            TypeHash = 0us

            Write = writer
            Read = reader

            FormatterInfo = FormatterInfo.Custom
            UseWithSubtypes = false
            CacheObj = true
        }