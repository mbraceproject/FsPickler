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

    let isUnSupportedType (t : Type) =
        t.IsPointer 
        || t = typeof<System.Reflection.Pointer>
        || t.IsByRef
        || t.IsCOMObject
        || t.IsImport
        || t.IsMarshalByRef
        || t.IsPrimitive // supported primitives should be already stored in the pickler cache        


    // creates a placeholder pickler instance

    type UninitializedPickler =
        static member Create<'T>() = new Pickler<'T>()
        static member CreateUntyped (t : Type) =
            if isUnSupportedType t then raise <| new NonSerializableTypeException(t)

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

    // pickler combinator for struct types

    type StructPickler =
        static member CreateUntyped(t : Type, resolver : IPicklerResolver) =
            let m = 
                typeof<StructPickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Pickler

        static member Create<'T when 'T : struct>(resolver : IPicklerResolver) =
            let fields = typeof<'T>.GetFields(allFields)
            if fields |> Array.exists(fun f -> f.IsInitOnly) then
                raise <| new NonSerializableTypeException(typeof<'T>, "type is marked with read-only instance fields.")
            
            let picklers = fields |> Array.map (fun f -> resolver.Resolve f.FieldType)

#if EMIT_IL
            
            let writer =
                if fields.Length = 0 then (fun _ _ -> ())
                else
                    let action =
                        Expression.compileAction2<Writer, 'T>(fun writer instance ->
                            Expression.zipWriteFields fields picklers writer instance |> Expression.Block :> _)

                    fun w t -> action.Invoke(w,t)

            let reader =
                Expression.compileFunc1<Reader, 'T>(fun reader ->

                    let instance = Expression.Variable(typeof<'T>, "instance")

                    let body =
                        seq {
                            yield Expression.Assign(instance, Expression.initializeObject<'T> ()) :> Expression

                            yield! Expression.zipReadFields fields picklers reader instance

                            yield instance :> _
                        }

                    Expression.Block([| instance |], body) :> _).Invoke

#else
            let writer (w : Writer) (t : 'T) =
                for i = 0 to fields.Length - 1 do
                    let o = fields.[i].GetValue(t)
                    picklers.[i].ManagedWrite w o

            let reader (r : Reader) =
                let t = FormatterServices.GetUninitializedObject(typeof<'T>)
                for i = 0 to fields.Length - 1 do
                    let o = picklers.[i].ManagedRead r
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
                typeof<'T>.GetFields(allFields) 
                |> Array.filter (fun f -> not (containsAttr<NonSerializedAttribute> f))

            if fields |> Array.exists(fun f -> f.IsInitOnly) then
                raise <| new NonSerializableTypeException(typeof<'T>, "type is marked with read-only instance fields.") 

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
                    let writer =
                        Expression.compileAction2<Writer, 'T>(fun writer instance ->
                            seq {
                                yield! Expression.runSerializationActions onSerializing writer instance

                                yield! Expression.zipWriteFields fields picklers writer instance

                                yield! Expression.runSerializationActions onSerialized writer instance

                            } |> Expression.Block :> Expression)

                    fun w t -> writer.Invoke(w,t)

            let reader =
                Expression.compileFunc1<Reader, 'T>(fun reader ->

                    let instance = Expression.Variable(typeof<'T>, "instance")

                    let body =
                        seq {
                            yield Expression.Assign(instance, Expression.initializeObject<'T> ()) :> Expression

                            yield! Expression.runDeserializationActions onDeserializing reader instance

                            yield! Expression.zipReadFields fields picklers reader instance

                            yield! Expression.runDeserializationActions onDeserialized reader instance

                            if isDeserializationCallback then
                                yield Expression.runDeserializationCallback instance

                            yield instance :> _
                        } 

                    Expression.Block([| instance |], body) :> Expression).Invoke

#else
            let inline run (ms : MethodInfo []) (x : obj) w =
                for i = 0 to ms.Length - 1 do 
                    ms.[i].Invoke(x, [| getStreamingContext w :> obj |]) |> ignore

            let writer (w : Writer) (t : 'T) =
                run onSerializing t w

                for i = 0 to fields.Length - 1 do
                    let o = fields.[i].GetValue(t)
                    picklers.[i].ManagedWrite w o

                run onSerialized t w

            let reader (r : Reader) =
                let t = FormatterServices.GetUninitializedObject(typeof<'T>) |> fastUnbox<'T>
                run onDeserializing t r

                for i = 0 to fields.Length - 1 do
                    let o = picklers.[i].ManagedRead r
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
                let onDeserialized = allMethods |> getSerializationMethods<OnDeserializedAttribute>

                let isDeserializationCallback = typeof<IDeserializationCallback>.IsAssignableFrom typeof<'T>

                let objPickler = resolver.Resolve<obj> ()
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