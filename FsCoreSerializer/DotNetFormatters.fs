module internal FsCoreSerializer.DotNetFormatters

    open System
    open System.Reflection
    open System.Runtime.Serialization
    open System.Linq.Expressions

    open Microsoft.FSharp.Reflection

    open FsCoreSerializer
    open FsCoreSerializer.Reflection
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


    type UninitializedFormatter =
        static member Create<'T>() = new Formatter<'T>()
        static member CreateUntyped (t : Type) =
            if isUnSupportedType t then raise <| new NonSerializableTypeException(t)
                
            let m =
                typeof<UninitializedFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            try m.Invoke(null, null) :?> Formatter
            with :? TargetInvocationException as e -> raise e.InnerException

    type AbstractFormatter =
        static member Create<'T> () =
            let writer = fun _ _ -> invalidOp <| sprintf "Attempting to use formatter for abstract type '%O'." typeof<'T>
            let reader = fun _ -> invalidOp <| sprintf "Attempting to use formatter for abstract type '%O'." typeof<'T>

            new Formatter<'T>(reader, writer, FormatterInfo.ReflectionDerived, true, false)

        static member CreateUntyped(t : Type) =
            let m = 
                typeof<AbstractFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            try m.Invoke(null, null) :?> Formatter
            with :? TargetInvocationException as e -> raise e.InnerException

    // formatter builder for ISerializable types
    type SerializableFormatter =
        static member TryCreate<'T when 'T :> ISerializable>(resolver : IFormatterResolver) =
            match tryGetCtor typeof<'T> [| typeof<SerializationInfo> ; typeof<StreamingContext> |] with
            | None -> None
            | Some ctorInfo ->
                let allMethods = typeof<'T>.GetMethods(memberBindings)
                let onSerializing = allMethods |> getSerializationMethods<'T, OnSerializingAttribute>
                let onSerialized = allMethods |> getSerializationMethods<'T, OnSerializedAttribute>
//                let onDeserializing = allMethods |> getSerializationMethods<'T, OnDeserializingAttribute>
                let onDeserialized = allMethods |> getSerializationMethods<'T, OnDeserializedAttribute>

                let isDeserializationCallback = typeof<IDeserializationCallback>.IsAssignableFrom typeof<'T>
#if EMIT_IL
                let inline runW (dele : Action<StreamingContext, 'T> option) (w : Writer) x =
                    match dele with
                    | None -> ()
                    | Some d -> d.Invoke(w.StreamingContext, x)

                let inline runR (dele : Action<StreamingContext, 'T> option) (r : Reader) x =
                    match dele with
                    | None -> ()
                    | Some d -> d.Invoke(r.StreamingContext, x)

                let onSerializing = preComputeSerializationMethods<'T> onSerializing
                let onSerialized = preComputeSerializationMethods<'T> onSerialized
                let onDeserialized = preComputeSerializationMethods<'T> onDeserialized

                let ctor =
                    Expression.compileFunc2<SerializationInfo, StreamingContext, 'T>(fun si sc -> 
                        Expression.New(ctorInfo, si, sc) :> _)

                let inline create si sc = ctor.Invoke(si, sc)
#else
                let inline run (ms : MethodInfo []) o (sc : StreamingContext)  =
                    for i = 0 to ms.Length - 1 do ms.[i].Invoke(o, [| sc :> obj |]) |> ignore

                let inline create (si : SerializationInfo) (sc : StreamingContext) = 
                    ctorInfo.Invoke [| si :> obj ; sc :> obj |]
#endif
                let writer (w : Writer) (x : 'T) =
                    runW onSerializing w x
                    let sI = new SerializationInfo(typeof<'T>, new FormatterConverter())
                    x.GetObjectData(sI, w.StreamingContext)
                    w.BW.Write sI.MemberCount
                    let enum = sI.GetEnumerator()
                    while enum.MoveNext() do
                        w.BW.Write enum.Current.Name
                        w.Write<obj> enum.Current.Value

                    runW onSerialized w x

                let reader (r : Reader) =
                    let sI = new SerializationInfo(typeof<'T>, new FormatterConverter())
                    let memberCount = r.BR.ReadInt32()
                    for i = 1 to memberCount do
                        let name = r.BR.ReadString()
                        let v = r.Read<obj> ()
                        sI.AddValue(name, v)

                    let x = create sI r.StreamingContext

                    runR onDeserialized r x
                    if isDeserializationCallback then (x :> obj :?> IDeserializationCallback).OnDeserialization null
                    x

                let fmt = new Formatter<'T>(reader, writer, FormatterInfo.ISerializable, true, false)
                Some(fmt :> Formatter)

        static member TryCreateUntyped(t : Type, resolver : IFormatterResolver) =
            let m =
                typeof<SerializableFormatter>
                    .GetMethod("TryCreate", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            try m.Invoke(null, [| resolver :> obj |]) :?> Formatter option
            with :? TargetInvocationException as e -> raise e.InnerException


    // formatter builder for IFsCoreSerializable types
    type FsCoreSerialibleFormatter =
        static member Create<'T when 'T :> IFsCoreSerializable>(resolver : IFormatterResolver) =
            match tryGetCtor typeof<'T> [| typeof<Reader> |] with
            | None ->
                let msg = "declared IFsCoreSerializable but missing constructor Reader -> T."
                raise <| new NonSerializableTypeException(typeof<'T>, msg)
            | Some ctorInfo ->
#if EMIT_IL
                let reader = Expression.compileFunc1<Reader, 'T>(fun reader -> Expression.New(ctorInfo, reader) :>_).Invoke
#else
                let reader (r : Reader) = ctorInfo.Invoke [| r :> obj |] :?> 'T
#endif
                let writer (w : Writer) (x : 'T) = x.GetObjectData(w)

                new Formatter<'T>(reader, writer, FormatterInfo.IFsCoreSerializable, true, false)

        static member CreateUntyped(t : Type, resolver : IFormatterResolver) =
            let m =
                typeof<FsCoreSerialibleFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            try m.Invoke(null, [| resolver :> obj |]) :?> Formatter
            with :? TargetInvocationException as e -> raise e.InnerException


    type StructFormatter =
        static member Create<'T when 'T : struct>(resolver : IFormatterResolver) =
            let fields = typeof<'T>.GetFields(fieldBindings)
            if fields |> Array.exists(fun f -> f.IsInitOnly) then
                raise <| new NonSerializableTypeException(typeof<'T>, "type is marked with read-only instance fields.")
            
            let formatters = fields |> Array.map (fun f -> resolver.Resolve f.FieldType)

#if EMIT_IL
            
            let writer =
                if fields.Length = 0 then (fun _ _ -> ())
                else
                    let action =
                        Expression.compileAction2<Writer, 'T>(fun writer instance ->
                            zipWriter fields formatters writer instance |> Expression.Block :> _)

                    fun w t -> action.Invoke(w,t)

            let reader =
                Expression.compileFunc1<Reader, 'T>(fun reader ->
                    let initializer = 
                        Expression.Call(objInitializer, Expression.constant typeof<'T>) 
                        |> Expression.unbox typeof<'T>

                    let instance = Expression.Variable(typeof<'T>, "instance")

                    let body =
                        seq {
                            yield Expression.Assign(instance, initializer) :> Expression

                            yield! zipReader fields formatters reader instance

                            yield instance :> _
                        }

                    Expression.Block([| instance |], body) :> _)

#else
#endif

            new Formatter<'T>(reader.Invoke, writer, FormatterInfo.ReflectionDerived, false, false)

        static member CreateUntyped(t : Type, resolver : IFormatterResolver) =
            let m = 
                typeof<StructFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            try m.Invoke(null, [| resolver :> obj |]) :?> Formatter
            with :? TargetInvocationException as e -> raise e.InnerException
                    
                    

    // reflection-based serialization rules for reference types

    type ClassFormatter =
        static member Create<'T when 'T : not struct>(resolver : IFormatterResolver) =

            let fields = 
                typeof<'T>.GetFields(fieldBindings) 
                |> Array.filter (fun f -> not (containsAttr<NonSerializedAttribute> f))

            if fields |> Array.exists(fun f -> f.IsInitOnly) then
                raise <| new NonSerializableTypeException(typeof<'T>, "type is marked with read-only instance fields.") 

            let formatters = fields |> Array.map (fun f -> resolver.Resolve f.FieldType)

            let isDeserializationCallback = typeof<IDeserializationCallback>.IsAssignableFrom typeof<'T>

            let allMethods = typeof<'T>.GetMethods(memberBindings)
            let onSerializing = allMethods |> getSerializationMethods<'T, OnSerializingAttribute>
            let onSerialized = allMethods |> getSerializationMethods<'T, OnSerializedAttribute>
            let onDeserializing = allMethods |> getSerializationMethods<'T, OnDeserializingAttribute>
            let onDeserialized = allMethods |> getSerializationMethods<'T, OnDeserializedAttribute>

#if EMIT_IL

            let writer =
                if onSerializing.Length = 0 && fields.Length = 0 && onSerialized.Length = 0 then
                    fun _ _ -> ()
                else
                    let writer =
                        Expression.compileAction2<Writer, 'T>(fun writer instance ->
                            seq {
                                yield! runSerializationActions onSerializing writer instance

                                yield! zipWriter fields formatters writer instance

                                yield! runSerializationActions onSerialized writer instance

                                yield Expression.constant ()

                            } |> Expression.Block :> Expression)

                    fun w t -> writer.Invoke(w,t)

            let reader =
                Expression.compileFunc1<Reader, 'T>(fun reader ->

                    let instance = Expression.Variable(typeof<'T>, "instance")

                    let body =
                        seq {
                            let objInitializer = 
                                Expression.Call(objInitializer, Expression.constant typeof<'T>) 
                                |> Expression.unbox typeof<'T>

                            yield Expression.Assign(instance, objInitializer) :> Expression
                
                            yield Expression.Call(reader, readerInitializer, instance) :> _

                            yield! runDeserializationActions onDeserializing reader instance

                            yield! zipReader fields formatters reader instance

                            yield! runDeserializationActions onDeserialized reader instance

                            if isDeserializationCallback then
                                yield runDeserializationCallback instance :> _

                            yield instance :> _
                        } 

                    Expression.Block([| instance |], body) :> Expression).Invoke


            new Formatter<'T>(reader, writer, FormatterInfo.ReflectionDerived, cacheObj = true, useWithSubtypes = false)

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


        static member CreateUntyped(t : Type, resolver : IFormatterResolver) =
            let m =
                typeof<ClassFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            try m.Invoke(null, [| resolver :> obj |]) :?> Formatter
            with :? TargetInvocationException as e -> raise e.InnerException


    type EnumFormatter =
        static member Create<'Enum, 'Underlying when 'Enum : enum<'Underlying>> (resolver : IFormatterResolver) =
            let fmt = resolver.Resolve<'Underlying> ()
            let writer_func = fmt.Write
            let reader_func = fmt.Read

            let writer (w : Writer) (x : 'Enum) =
                let value = Microsoft.FSharp.Core.LanguagePrimitives.EnumToValue<'Enum, 'Underlying> x
                writer_func w value

            let reader (r : Reader) =
                let value = reader_func r
                Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<'Underlying, 'Enum> value

            new Formatter<'Enum>(reader, writer, FormatterInfo.ReflectionDerived, false, false)

        static member CreateUntyped(enum : Type, resolver : IFormatterResolver) =
            let underlying = enum.GetEnumUnderlyingType()
            // reflection call typed method
            let m = 
                typeof<EnumFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| enum ; underlying |]

            m.Invoke(null, [| resolver :> obj |]) :?> Formatter


    type DelegateFormatter =
        static member Create<'Delegate when 'Delegate :> Delegate> (resolver : IFormatterResolver) =
            let memberInfoFormatter = resolver.Resolve<MethodInfo> ()
            let writer (w : Writer) (dele : 'Delegate) =
                match dele.GetInvocationList() with
                | [| _ |] ->
                    w.BW.Write true
                    w.Write(memberInfoFormatter, dele.Method)
                    if not dele.Method.IsStatic then w.Write<obj> dele.Target
                | deleList ->
                    w.BW.Write false
                    w.BW.Write deleList.Length
                    for i = 0 to deleList.Length - 1 do
                        w.Write<System.Delegate> (deleList.[i])

            let reader (r : Reader) =
                if r.BR.ReadBoolean() then
                    let meth = r.Read memberInfoFormatter
                    if not meth.IsStatic then
                        let target = r.Read<obj> ()
                        Delegate.CreateDelegate(typeof<'Delegate>, target, meth, throwOnBindFailure = true) :?> 'Delegate
                    else
                        Delegate.CreateDelegate(typeof<'Delegate>, meth, throwOnBindFailure = true) :?> 'Delegate
                else
                    let n = r.BR.ReadInt32()
                    let deleList = Array.zeroCreate<System.Delegate> n
                    for i = 0 to n - 1 do deleList.[i] <- r.Read<System.Delegate> ()
                    Delegate.Combine deleList :?> 'Delegate

            new Formatter<'Delegate>(reader, writer, FormatterInfo.Custom, true, false)

        static member CreateUntyped(t : Type, resolver : IFormatterResolver) =
            let m =
                typeof<DelegateFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            try m.Invoke(null, [| resolver :> obj |]) :?> Formatter
            with :? TargetInvocationException as e -> raise e.InnerException


    // special treatment of F# unions

    type FsUnionFormatter =
        static member CreateUntyped(unionType : Type, resolver : IFormatterResolver) =
            let m =
                typeof<FsUnionFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| unionType |]

            try m.Invoke(null, [| resolver :> obj |]) :?> Formatter
            with :? TargetInvocationException as e -> raise e.InnerException

        static member Create<'Union> (resolver : IFormatterResolver) =

            let unionInfo =
                FSharpType.GetUnionCases(typeof<'Union>, memberBindings) 
                |> Array.map(fun uci ->
                    let fields = uci.GetFields()
                    let ctor = FSharpValue.PreComputeUnionConstructorInfo(uci, memberBindings)
                    let formatters = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                    uci, ctor, fields, formatters)

#if EMIT_IL
            let callUnionTagReader (union : Expression) =
                match FSharpValue.PreComputeUnionTagMemberInfo(typeof<'Union>, memberBindings) with
                | null -> invalidOp "unexpected error"
                | :? PropertyInfo as p -> Expression.Property(union, p) :> Expression
                | :? MethodInfo as m when m.IsStatic -> Expression.Call(m, union) :> Expression
                | :? MethodInfo as m -> Expression.Call(union, m) :> Expression
                | _ -> invalidOp "unexpected error"


            let tagWriter (writer : Expression) (tag : Expression) =
                let bw = typeof<Writer>.GetProperty("BW", BindingFlags.NonPublic ||| BindingFlags.Instance)
                let intWriter = typeof<System.IO.BinaryWriter>.GetMethod("Write", [| typeof<int> |])
                let bwExpr = Expression.Property(writer, bw)
                Expression.Call(bwExpr, intWriter, tag) :> Expression

            let tagReader (reader : Expression) =
                let br = typeof<Reader>.GetProperty("BR", BindingFlags.NonPublic ||| BindingFlags.Instance)
                let intReader = typeof<System.IO.BinaryReader>.GetMethod("ReadInt32")
                let brExpr = Expression.Property(reader, br)
                Expression.Call(brExpr, intReader) :> Expression


            let callCaseWriter (uci : UnionCaseInfo) (fields : PropertyInfo []) (formatters : Formatter []) 
                                    (writer : Expression) (instance : Expression) =

                let writeField instance (field : PropertyInfo) (formatter : Formatter) =
                    let value = Expression.Property(instance, field)
                    serializeValue writer formatter value

                let body =
                    if fields.Length = 0 then Expression.Empty() :> Expression
                    else
                        let instance = 
                            match fields.[0].DeclaringType with
                            | t when t <> typeof<'Union> -> Expression.TypeAs(instance, t) :> Expression
                            | _ -> instance

                        Seq.map2 (writeField instance) fields formatters |> Expression.Block :> _

                Expression.SwitchCase(body, Expression.constant uci.Tag)


            let callCaseReader (uci : UnionCaseInfo) (fields : PropertyInfo []) (formatters : Formatter []) 
                                            (ctor : MethodInfo) (reader : Expression) =
                    
                let cParams = Seq.map (deserializeValue reader) formatters
                let unionCase = Expression.Call(ctor, cParams) :> Expression
                // upcast to union type
                let unionCase =
                    if fields.Length > 0 then 
                        match fields.[0].DeclaringType with
                        | t when t <> typeof<'Union> ->
                            Expression.TypeAs(unionCase, typeof<'Union>) :> Expression
                        | _ -> unionCase
                    else unionCase

                Expression.SwitchCase(unionCase, Expression.constant uci.Tag)

            let writer =
                Expression.compileAction2<Writer, 'Union>(fun writer union ->
                    let tag = Expression.Variable(typeof<int>, "tag")
                    let cases = 
                        unionInfo 
                        |> Array.map (fun (uci,_,fields,formatters) -> callCaseWriter uci fields formatters writer union)

                    let defCase = Expression.failwith<InvalidOperationException, SynVoid> "Invalid F# union tag."

                    let body =
                        [|
                            Expression.Assign(tag, callUnionTagReader union) :> Expression
                            tagWriter writer tag
                            Expression.Switch(tag, defCase, cases) :> _
                        |]

                    Expression.Block([| tag |] , body) :> _)

            let reader =
                Expression.compileFunc1<Reader, 'Union>(fun reader ->
                    let tag = tagReader reader
                    let cases =
                        unionInfo
                        |> Array.map (fun (uci,ctor,fields,formatters) -> callCaseReader uci fields formatters ctor reader)

                    let defCase = Expression.failwith<InvalidOperationException, 'Union> "Invalid F# union tag."

                    Expression.Switch(tag, defCase, cases) :> _)


            new Formatter<'Union>(reader.Invoke, (fun w t -> writer.Invoke(w,t)), FormatterInfo.FSharpValue, true, true)
#else
#endif


    type TupleFormatter =

        static member CreateUntyped(tupleType : Type, resolver : IFormatterResolver) =
            let m =
                typeof<TupleFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| tupleType |]

            try m.Invoke(null, [| resolver :> obj |]) :?> Formatter
            with :? TargetInvocationException as e -> raise e.InnerException
        
        static member Create<'Tuple>(resolver : IFormatterResolver) =
            let ctor,_ = FSharpValue.PreComputeTupleConstructorInfo typeof<'Tuple>
            let elements = 
                typeof<'Tuple>.GetProperties()  
                |> Seq.filter (fun p -> p.Name.StartsWith("Item")) 
                |> Seq.sortBy (fun p -> p.Name)
                |> Seq.map (fun p -> p, resolver.Resolve p.PropertyType)
                |> Seq.toArray

            let nestedTuple =
                match typeof<'Tuple>.GetProperty("Rest") with
                | null -> None
                | rest ->
                    let fmt = resolver.Resolve rest.PropertyType
                    Some(rest, fmt)

#if EMIT_IL 
            let writer =
                if elements.Length = 0 then fun _ _ -> ()
                else
                    let writer =
                        Expression.compileAction2<Writer, 'Tuple>(fun writer tuple ->
                            seq {
                                let writeElement (element : PropertyInfo, f : Formatter) =
                                    let value = Expression.Property(tuple, element)
                                    serializeValue writer f value

                                yield! elements |> Seq.map writeElement

                                match nestedTuple with
                                | None -> ()
                                | Some (prop, f) ->
                                    let nested = Expression.Property(tuple, prop)
                                    yield serializeValue writer f nested
                            } |> Expression.Block :> _)

                    fun w t -> writer.Invoke(w,t)

            let reader =
                Expression.compileFunc1<Reader, 'Tuple>(fun reader ->
                    let elements = elements |> Seq.map (fun (_,f) -> deserializeValue reader f)

                    match nestedTuple with
                    | None -> Expression.New(ctor, elements) :> _
                    | Some (_, fmt) ->
                        let nestedValue = deserializeValue reader fmt
                        Expression.New(ctor, seq { yield! elements ; yield nestedValue }) :> _).Invoke        
#else
#endif

#if OPTIMIZE_FSHARP
            new Formatter<'Tuple>(reader, writer, FormatterInfo.FSharpValue, false, true)
#else
            new Formatter<'Tuple>(reader, writer, FormatterInfo.Custom, true, false)
#endif


    type FsRecordFormatter =

        static member CreateUntyped(t : Type, resolver : IFormatterResolver, isExceptionType) =
            let m =
                typeof<FsRecordFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            try m.Invoke(null, [| resolver :> obj ; isExceptionType :> obj |]) :?> Formatter
            with :? TargetInvocationException as e -> raise e.InnerException
        
        static member Create<'Record>(resolver : IFormatterResolver, isExceptionType) =
            let fields, ctor =
                if isExceptionType then
                    let fields = FSharpType.GetExceptionFields(typeof<'Record>, memberBindings)
                    let signature = fields |> Array.map(fun p -> p.PropertyType)
                    let ctor = 
                        typeof<'Record>.GetConstructors(memberBindings)
                        |> Array.find(fun c -> c.GetParameters () |> Array.map(fun p -> p.ParameterType) = signature)
                    fields, ctor
                else
                    let fields = FSharpType.GetRecordFields typeof<'Record>
                    let ctor = FSharpValue.PreComputeRecordConstructorInfo(typeof<'Record>, memberBindings)
                    fields, ctor

            let formatters = fields |> Array.map (fun f -> f, resolver.Resolve f.PropertyType)

#if EMIT_IL
            let writer =
                if fields.Length = 0 then fun _ _ -> ()
                else
                    let writer =
                        Expression.compileAction2<Writer, 'Record>(fun writer record ->
                            let writeField (p : PropertyInfo, f : Formatter) =
                                let value = Expression.Property(record, p)
                                serializeValue writer f value
                            
                            formatters |> Seq.map writeField |> Expression.Block :> _)

                    fun w t -> writer.Invoke(w,t)

            let reader =
                Expression.compileFunc1<Reader, 'Record>(fun reader ->
                    let values = formatters |> Seq.map(fun (_,f) -> deserializeValue reader f)

                    Expression.New(ctor, values) :> _).Invoke
#else
#endif
            new Formatter<'Record>(reader, writer, FormatterInfo.FSharpValue, cacheObj = true, useWithSubtypes = false)