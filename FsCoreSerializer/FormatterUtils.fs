namespace FsCoreSerializer

    open System
    open System.Reflection
    open System.Linq.Expressions
    open System.Runtime.CompilerServices
    open System.Runtime.Serialization
    
    open FsCoreSerializer
    open FsCoreSerializer.Utils

    module internal FormatterUtils =

        let containsAttr<'T when 'T :> Attribute> (m : MemberInfo) =
            m.GetCustomAttributes(typeof<'T>, true) |> Seq.isEmpty |> not

        let fieldBindings = 
            BindingFlags.NonPublic ||| BindingFlags.Public ||| 
                BindingFlags.Instance ||| BindingFlags.FlattenHierarchy 

        let memberBindings =
            BindingFlags.NonPublic ||| BindingFlags.Public |||
                BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.FlattenHierarchy

        let ctorBindings = BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public

        let tryGetCtor (t : Type) (args : Type []) = denull <| t.GetConstructor(ctorBindings,null,args, [||]) 

//        // builds type info enumeration out of reflection info
//        let getTypeInfo (t : Type) =
//            if t.IsPrimitive then TypeInfo.Primitive
//            elif t.IsEnum then TypeInfo.Enum
//            elif t.IsValueType then TypeInfo.Value
//            elif t.IsArray then TypeInfo.Array
//            elif t.IsSealed then TypeInfo.Sealed
//            elif t.IsAbstract then TypeInfo.Abstract
//            else TypeInfo.NonSealed

        // initialize a formatter from a typed set of lambdas
        let inline mkFormatter<'T> (info:FormatterInfo) (useWithSubtypes:bool) (cache:bool) 
                                            (reader : Reader -> 'T) (writer : Writer -> 'T -> unit) =

            new Formatter<'T>(reader, writer, info, cache, useWithSubtypes)


        // compilation provision for methods that carry the OnSerializing, OnSerialized, etc attributes
        let preComputeSerializationMethods<'T> (ms : MethodInfo []) =
            match ms with
            | [||] -> None
            | methods ->
                let dele =
                    Expression.compileAction2<StreamingContext,'T>(fun sc value ->
                        methods 
                        |> Array.map (fun m -> Expression.Call(value, m, sc) :> Expression)
                        |> Expression.Block :> _)

                Some dele

        let writerCtx = typeof<Writer>.GetProperty("StreamingContext")
        let readerCtx = typeof<Reader>.GetProperty("StreamingContext")
        let readerInitializer = typeof<Reader>.GetMethod("EarlyRegisterObject", BindingFlags.NonPublic ||| BindingFlags.Instance)
        let objInitializer = typeof<FormatterServices>.GetMethod("GetUninitializedObject")
        let deserializationCallBack = typeof<IDeserializationCallback>.GetMethod("OnDeserialization")

        let runSerializationActions (ms : MethodInfo []) (writerInstance : Expression) (instance : Expression) =
            let sctx = Expression.Property(writerInstance, writerCtx)
            ms |> Seq.map (fun m -> Expression.Call(instance, m, sctx) :> Expression)
                
        let runDeserializationActions (ms : MethodInfo []) (readerInstance : Expression) (instance : Expression) =
            let sctx = Expression.Property(readerInstance, readerCtx)
            ms |> Seq.map (fun m -> Expression.Call(instance, m, sctx) :> Expression)

        let runDeserializationCallback (instance : Expression) =
            let dc = Expression.TypeAs(instance, typeof<IDeserializationCallback>)
            Expression.Call(dc, deserializationCallBack, Expression.constant null)

        let getSerializationMethods<'T, 'Attr when 'Attr :> Attribute> (ms : MethodInfo []) =
            let isSerializationMethod(m : MethodInfo) =
                not m.IsStatic && 
                containsAttr<'Attr> m &&
                m.ReturnType = typeof<System.Void> &&

                    match m.GetParameters() with
                    | [| p |] when p.ParameterType = typeof<StreamingContext> -> true
                    | _ -> false

            let ms = ms |> Array.filter isSerializationMethod

            if ms.Length > 0 && typeof<'T>.IsValueType then
                let message = sprintf "is value type but contains methods marked with '%s'." typeof<'Attr>.Name
                raise <| new NonSerializableTypeException(typeof<'T>, message)
            else
                ms

        //
        //  internal read/write combinators
        //

        let inline isValue (f : Formatter) = f.TypeInfo <= TypeInfo.Value

        let inline write bypass (w : Writer) (f : Formatter<'T>) (x : 'T) =
            if bypass then f.Write w x
            else w.Write(f, x)

        let inline read bypass (r : Reader) (f : Formatter<'T>) =
            if bypass then f.Read r
            else r.Read f
                
//            if f.TypeInfo <= TypeInfo.Value then f.Write w x
//            elif not f.CacheObj then
//                if obj.ReferenceEquals(x, null) then w.BW.Write true
//                else
//                    w.BW.Write false ; f.Write w x
//            elif f.FormatterInfo = FormatterInfo.FSharpValue then
//                if obj.ReferenceEquals(x, null) then w.BW.Write true
//                else
//                    do RuntimeHelpers.EnsureSufficientExecutionStack()
//
//                    w.BW.Write false
//                    f.Write w x
//            else
//                w.Write(f, x)

//        let inline read (r : Reader) (f : Formatter<'T>) =
//            if f.TypeInfo <= TypeInfo.Value then f.Read r
//            elif not f.CacheObj || f.FormatterInfo = FormatterInfo.FSharpValue then
//                if r.BR.ReadBoolean() then Unchecked.defaultof<'T>
//                else f.Read r
//            else
//                r.Read f

        // length passed as argument to avoid unecessary evaluations of sequence
        let inline writeSeq (w : Writer) (ef : Formatter<'T>) (length : int) (xs : seq<'T>) =
            let isValue = ef.TypeInfo <= TypeInfo.Value
            w.BW.Write length
            for x in xs do write isValue w ef x

        // TODO : value types should probably be block deserialized
        let inline readSeq<'T> (r : Reader) (ef : Formatter<'T>) =
            let isValue = ef.TypeInfo <= TypeInfo.Value
            let length = r.BR.ReadInt32()
            let xs = Array.zeroCreate<'T> length
            for i = 0 to length - 1 do
                xs.[i] <- read isValue r ef
            xs

        // length passed as argument to avoid unecessary evaluations of sequence
        let inline writeKVPair (w : Writer) (kf : Formatter<'K>) (vf : Formatter<'V>) (length : int) (xs : ('K * 'V) seq) =
            let kIsValue = kf.TypeInfo <= TypeInfo.Value
            let vIsValue = vf.TypeInfo <= TypeInfo.Value
            w.BW.Write length
            for k,v in xs do
                write kIsValue w kf k
                write vIsValue w vf v

        let inline readKVPair<'K,'V> (r : Reader) (kf : Formatter<'K>) (vf : Formatter<'V>) =
            let kIsValue = kf.TypeInfo <= TypeInfo.Value
            let vIsValue = vf.TypeInfo <= TypeInfo.Value
            let length = r.BR.ReadInt32()
            let xs = Array.zeroCreate<'K * 'V> length
            for i = 0 to length - 1 do
                let k = read kIsValue r kf
                let v = read vIsValue r vf
                xs.[i] <- k,v

            xs

        type Type with
            member t.GetGenericMethod(isStatic, name : string, genericArgCount : int, paramCount : int) =
                t.GetMethods(memberBindings)
                |> Array.find(fun m ->
                    m.Name = name && m.IsStatic = isStatic 
                        && genericArgCount = m.GetGenericArguments().Length
                        && paramCount = m.GetParameters().Length)


        let writerM = typeof<Writer>.GetGenericMethod(false, "Write", 1, 2)
        let readerM = typeof<Reader>.GetGenericMethod(false, "Read", 1, 1)

        let inline serializeValue (writer : Expression) (formatter : Formatter) (value : Expression) =
            let ft = typedefof<Formatter<_>>.MakeGenericType [| formatter.Type |]
            let fExpr = Expression.Constant(formatter, ft)
            Expression.Call(writer, writerM.MakeGenericMethod [| formatter.Type |], fExpr, value) :> Expression

        let inline deserializeValue (reader : Expression) (formatter : Formatter) =
            let ft = typedefof<Formatter<_>>.MakeGenericType [| formatter.Type |]
            let fExpr = Expression.Constant(formatter, ft)
            Expression.Call(reader, readerM.MakeGenericMethod [| formatter.Type |], fExpr) :> Expression

        let zipWriter (fields : FieldInfo []) (formatters : Formatter []) (writer : Expression) (instance : Expression) =
            let writeValue (formatter : Formatter, field : FieldInfo) =
                let value = Expression.Field(instance, field)
                serializeValue writer formatter value

            Seq.zip formatters fields |> Seq.map writeValue


        let zipReader (fields : FieldInfo []) (formatters : Formatter []) (reader : Expression) (instance : Expression) =
            let readValue (formatter : Formatter, field : FieldInfo) =
                let value = deserializeValue reader formatter
                let field = Expression.Field(instance, field)
                Expression.Assign(field, value) :> Expression

            Seq.zip formatters fields |> Seq.map readValue


//        // TODO: use IL emit
//        let inline zipWrite (w : Writer) (formatters : Formatter []) (objs : obj []) : unit =
//            for i = 0 to formatters.Length - 1 do
//                write w formatters.[i].Value objs.[i]
//
//        let inline zipRead (r : Reader) (formatters : Formatter []) : obj [] =
//            let objs = Array.zeroCreate formatters.Length
//            for i = 0 to formatters.Length - 1 do
//                objs.[i] <- read r formatters.[i].Value
//
//            objs