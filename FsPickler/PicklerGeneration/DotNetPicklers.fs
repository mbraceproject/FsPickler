module internal Nessos.FsPickler.DotNetPicklers

    open System
    open System.IO
    open System.Reflection
    open System.Threading
    open System.Runtime.Serialization

    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection
    open Nessos.FsPickler.PicklerUtils
    open Nessos.FsPickler.TypeShape

#if EMIT_IL
    open System.Reflection.Emit
    open Nessos.FsPickler.Emit
    open Nessos.FsPickler.PicklerEmit
#endif


    // creates a placeholder pickler instance

    type PicklerInitializer private () =

        static let visitor =
            {
                new ITypeVisitor<Pickler> with
                    member __.Visit<'T> () = CompositePickler.CreateUninitialized<'T> () :> Pickler
            }

        static member Create(t : Type) =
            let typeShape = 
                try TypeShape.resolve t
                with UnSupportedShape ->
                    raise <| NonSerializableTypeException t

            let pickler = typeShape.Accept visitor
            typeShape, pickler

    // abstract type pickler factory

    type AbstractPickler =
        static member Create<'T> () =
            let writer _ _ = invalidOp <| sprintf "Attempting to call abstract pickler '%O'." typeof<'T>
            let reader _ = invalidOp <| sprintf "Attempting to call abstract pickler '%O'." typeof<'T>

            CompositePickler.Create<'T>(reader, writer, PicklerInfo.FieldSerialization, true, false)

    // pickler combinator for enum types

    type EnumPickler =

        static member Create<'Enum, 'Underlying when 'Enum : enum<'Underlying>> (resolver : IPicklerResolver) =
            let pickler = resolver.Resolve<'Underlying> ()

            let writer (w : WriteState) (x : 'Enum) =
                let value = Microsoft.FSharp.Core.LanguagePrimitives.EnumToValue<'Enum, 'Underlying> x
                pickler.Write w "value" value

            let reader (r : ReadState) =
                let value = pickler.Read r "value"
                Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<'Underlying, 'Enum> value

            CompositePickler.Create(reader, writer, PicklerInfo.FieldSerialization, cacheByRef = false, useWithSubtypes = false)


    // support for nullable types

    type NullablePickler =

        static member Create<'T when 
                                'T : (new : unit -> 'T) and 
                                'T : struct and 
                                'T :> ValueType> (resolver : IPicklerResolver) =

            let pickler = resolver.Resolve<'T> ()

            let writer (w : WriteState) (x : Nullable<'T>) =
                if x.HasValue then 
                    w.Formatter.WriteBoolean "isNull" false
                    pickler.Write w "value" x.Value
                else
                    w.Formatter.WriteBoolean "isNull" true

            let reader (r : ReadState) =
                if r.Formatter.ReadBoolean "isNull" then
                    Nullable<'T> ()
                else
                    let value = pickler.Read r "value"
                    Nullable<'T>(value)

            CompositePickler.Create(reader, writer, PicklerInfo.FieldSerialization, cacheByRef = false, useWithSubtypes = false)

    // pickler combinator for struct types

    type StructPickler =

        static member Create<'T when 'T : struct>(resolver : IPicklerResolver) =

            let fields = gatherFields typeof<'T>
            let picklers = fields |> Array.map (fun f -> resolver.Resolve f.FieldType)

#if EMIT_IL
            let writerDele =
                DynamicMethod.compileAction3<Pickler [], WriteState, 'T> "structSerializer" (fun picklers writer parent ilGen ->

                    emitSerializeFields fields writer picklers parent ilGen

                    ilGen.Emit OpCodes.Ret
                )

            let readerDele =
                DynamicMethod.compileFunc2<Pickler [], ReadState, 'T> "structDeserializer" (fun picklers reader ilGen ->
                    
                    // initialize empty value type
                    let value = EnvItem<'T>(ilGen)
                    emitObjectInitializer typeof<'T> ilGen
                    value.Store ()

                    emitDeserializeFields fields reader picklers value ilGen

                    value.Load ()
                    ilGen.Emit OpCodes.Ret
                )

            let writer (w : WriteState) (t : 'T) = writerDele.Invoke(picklers, w, t)
            let reader (r : ReadState) = readerDele.Invoke(picklers, r)

#else
            let writer (w : WriteState) (t : 'T) =
                for i = 0 to fields.Length - 1 do
                    let f = fields.[i]
                    let o = f.GetValue(t)
                    picklers.[i].UntypedWrite w (getTagFromMemberInfo f) o

            let reader (r : ReadState) =
                let t = FormatterServices.GetUninitializedObject(typeof<'T>)
                for i = 0 to fields.Length - 1 do
                    let f = fields.[i]
                    let o = picklers.[i].UntypedRead r (getTagFromMemberInfo f)
                    f.SetValue(t, o)
                
                fastUnbox<'T> t
#endif

            CompositePickler.Create(reader, writer, PicklerInfo.FieldSerialization, cacheByRef = false, useWithSubtypes = false)
                    

    // general-purpose pickler combinator for reference types

    type ClassFieldPickler =

        static member Create<'T when 'T : not struct>(resolver : IPicklerResolver) =
            if not typeof<'T>.IsSerializable then
                raise <| new NonSerializableTypeException(typeof<'T>)

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
                        DynamicMethod.compileAction3<Pickler [], WriteState, 'T> "classSerializer" (fun picklers writer value ilGen ->

                            emitSerializationMethodCalls onSerializing (Choice1Of2 writer) value ilGen

                            emitSerializeFields fields writer picklers value ilGen

                            emitSerializationMethodCalls onSerialized (Choice1Of2 writer) value ilGen
                            
                            ilGen.Emit OpCodes.Ret)

                    fun w t -> writerDele.Invoke(picklers, w, t)

            let readerDele =
                DynamicMethod.compileFunc2<Pickler [], ReadState, 'T> "classDeserializer" (fun picklers reader ilGen ->

                    // get uninitialized object and store locally
                    let value = EnvItem<'T>(ilGen)
                    emitObjectInitializer typeof<'T> ilGen
                    value.Store ()

                    emitSerializationMethodCalls onDeserializing (Choice2Of2 reader) value ilGen

                    emitDeserializeFields fields reader picklers value ilGen

                    emitSerializationMethodCalls onDeserialized (Choice2Of2 reader) value ilGen

                    if isDeserializationCallback then emitDeserializationCallback value ilGen

                    value.Load ()
                    ilGen.Emit OpCodes.Ret
                )

            let reader r = readerDele.Invoke(picklers, r)
#else
            let inline run (ms : MethodInfo []) (x : obj) w =
                for i = 0 to ms.Length - 1 do 
                    ms.[i].Invoke(x, [| getStreamingContext w :> obj |]) |> ignore

            let writer (w : WriteState) (t : 'T) =
                run onSerializing t w

                for i = 0 to fields.Length - 1 do
                    let f = fields.[i]
                    let o = f.GetValue(t)
                    picklers.[i].UntypedWrite w (getTagFromMemberInfo f) o

                run onSerialized t w

            let reader (r : ReadState) =
                let t = FormatterServices.GetUninitializedObject(typeof<'T>) |> fastUnbox<'T>
                run onDeserializing t r

                for i = 0 to fields.Length - 1 do
                    let f = fields.[i]
                    let o = picklers.[i].UntypedRead r (getTagFromMemberInfo f)
                    f.SetValue(t, o)

                run onDeserialized t r
                if isDeserializationCallback then (fastUnbox<IDeserializationCallback> t).OnDeserialization null
                t
#endif

            CompositePickler.Create(reader, writer, PicklerInfo.FieldSerialization, cacheByRef = true, useWithSubtypes = false)


    // pickler implementation for delegate types

    type DelegatePickler =

        static member Create<'Delegate when 'Delegate :> Delegate> (resolver : IPicklerResolver) =
            let objPickler = resolver.Resolve<obj> ()
            let memberInfoPickler = resolver.Resolve<MethodInfo> ()
            let delePickler = resolver.Resolve<System.Delegate> ()

            let writer (w : WriteState) (dele : 'Delegate) =
                match dele.GetInvocationList() with
                | [| _ |] ->
                    w.Formatter.WriteBoolean "isLinked" false
                    memberInfoPickler.Write w "method" dele.Method
                    if not dele.Method.IsStatic then objPickler.Write w "target" dele.Target
                | deleList ->
                    w.Formatter.WriteBoolean "isLinked" true
                    w.Formatter.WriteInt32 "length" deleList.Length
                    for i = 0 to deleList.Length - 1 do
                        delePickler.Write w "linked" deleList.[i]

            let reader (r : ReadState) =
                if not <| r.Formatter.ReadBoolean "isLinked" then
                    let meth = memberInfoPickler.Read r "method"
                    if not meth.IsStatic then
                        let target = objPickler.Read r "target"
                        Delegate.CreateDelegate(typeof<'Delegate>, target, meth, throwOnBindFailure = true) |> fastUnbox<'Delegate>
                    else
                        Delegate.CreateDelegate(typeof<'Delegate>, meth, throwOnBindFailure = true) |> fastUnbox<'Delegate>
                else
                    let n = r.Formatter.ReadInt32 "length"
                    let deleList = Array.zeroCreate<System.Delegate> n
                    for i = 0 to n - 1 do deleList.[i] <- delePickler.Read r "linked"
                    Delegate.Combine deleList |> fastUnbox<'Delegate>

            CompositePickler.Create(reader, writer, PicklerInfo.Delegate, cacheByRef = true, useWithSubtypes = false)

    // pickler combinator for ISerializable types

    type ISerializablePickler =

        static member Create<'T when 'T :> ISerializable>(resolver : IPicklerResolver) =
            match typeof<'T>.TryGetConstructor [| typeof<SerializationInfo> ; typeof<StreamingContext> |] with
            | None -> raise <| new NonSerializableTypeException(typeof<'T>, "is ISerializable but does not implement constructor with parameters (SerializationInfo, StreamingContext).")
            | Some ctorInfo ->
                let allMethods = typeof<'T>.GetMethods(allMembers)
                let onSerializing = allMethods |> getSerializationMethods<OnSerializingAttribute> |> wrapDelegate<Action<'T, StreamingContext>>
                let onSerialized = allMethods |> getSerializationMethods<OnSerializedAttribute> |> wrapDelegate<Action<'T, StreamingContext>>
                let onDeserialized = allMethods |> getSerializationMethods<OnDeserializedAttribute> |> wrapDelegate<Action<'T, StreamingContext>>

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
                let writer (w : WriteState) (x : 'T) =
                    run onSerializing w x
                    let sI = new SerializationInfo(typeof<'T>, new FormatterConverter())
                    x.GetObjectData(sI, w.StreamingContext)
                    w.Formatter.WriteInt32 "memberCount" sI.MemberCount
                    let enum = sI.GetEnumerator()
                    while enum.MoveNext() do
                        w.Formatter.WriteString "name" enum.Current.Name
                        objPickler.Write w "value" enum.Current.Value

                    run onSerialized w x

                let reader (r : ReadState) =
                    let sI = new SerializationInfo(typeof<'T>, new FormatterConverter())
                    let memberCount = r.Formatter.ReadInt32 "memberCount"
                    for i = 1 to memberCount do
                        let name = r.Formatter.ReadString "name"
                        let v = objPickler.Read r "value"
                        sI.AddValue(name, v)

                    let x = create sI r.StreamingContext

                    run onDeserialized r x
                    if isDeserializationCallback then (fastUnbox<IDeserializationCallback> x).OnDeserialization null
                    x

                CompositePickler.Create(reader, writer, PicklerInfo.ISerializable, cacheByRef = true, useWithSubtypes = false)


    //  check if type implements a static factory method : IPicklerResolver -> Pickler<DeclaringType>

    type CustomPickler =
        static member Create(t : Type, resolver : IPicklerResolver) =
            let factoryMethod =
                match t.GetMethod("CreatePickler", allStatic) with
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