namespace FsPickler

    // LINQ expression utilities

#if EMIT_IL

    open System
    open System.Reflection
    open System.Linq.Expressions
    open System.Runtime.Serialization

    open FsPickler
    open FsPickler.Utils
    open FsPickler.FormatterUtils

    module internal Expression =

        /// F# does not allow using Void in type arguments
        type SynVoid = class end
        with 
            static member Replace(t : System.Type) =
                if t = typeof<SynVoid> then typeof<System.Void>
                else t

        let inline compileFunc1<'U, 'V>(f : Expression -> Expression) =
            let parameter = Expression.Parameter(typeof<'U>)
            let lambda = Expression.Lambda<Func<'U,'V>>(f parameter, parameter)
            lambda.Compile()

        let inline compileFunc2<'U1, 'U2, 'V>(f : Expression -> Expression -> Expression) =
            let p1 = Expression.Parameter typeof<'U1>
            let p2 = Expression.Parameter typeof<'U2>
            let lambda = Expression.Lambda<Func<'U1,'U2,'V>>(f p1 p2, p1, p2)
            lambda.Compile()

        let inline compileAction1<'U1>(f : Expression -> Expression) =
            let p1 = Expression.Parameter typeof<'U1>
            let lambda = Expression.Lambda<Action<'U1>>(f p1, p1)
            lambda.Compile()

        let inline compileAction2<'U1, 'U2>(f : Expression -> Expression -> Expression) =
            let p1 = Expression.Parameter typeof<'U1>
            let p2 = Expression.Parameter typeof<'U2>
            let lambda = Expression.Lambda<Action<'U1, 'U2>>(f p1 p2, p1, p2)
            lambda.Compile()

        let pair<'T, 'U>(e1 : Expression, e2 : Expression) =
            let ctor = typeof<System.Tuple<'T,'U>>.GetConstructor([| SynVoid.Replace typeof<'T> ;  SynVoid.Replace typeof<'U> |])
            Expression.New(ctor , [| e1 ; e2 |])

        let failwith<'exn, 'T when 'exn :> exn> (message : string) =
            let ctor = typeof<'exn>.GetConstructor [| typeof<string> |]
            let exn = Expression.New(ctor, Expression.Constant message)
            Expression.Throw(exn, SynVoid.Replace typeof<'T>)

        let unbox (t : Type) (e : Expression) =
            if t.IsValueType then Expression.Unbox(e, t) :> Expression
            else Expression.Convert(e, t) :> Expression

        let box (e : Expression) = Expression.TypeAs(e, typeof<obj>) :> Expression

        let constant (t : 'T) = Expression.Constant(t, typeof<'T>) :> Expression


        //
        //  Serialization - specific routines
        //

        [<AutoOpen>]
        module private ReflectedAPI =

            let writerCtx = typeof<Writer>.GetProperty("StreamingContext")
            let readerCtx = typeof<Reader>.GetProperty("StreamingContext")
            let readerInitializer = typeof<Reader>.GetMethod("EarlyRegisterObject", BindingFlags.NonPublic ||| BindingFlags.Instance)
            let objInitializer = typeof<FormatterServices>.GetMethod("GetUninitializedObject")
            let deserializationCallBack = typeof<IDeserializationCallback>.GetMethod("OnDeserialization")
            let writerM = typeof<Writer>.GetGenericMethod(false, "Write", 1, 2)
            let readerM = typeof<Reader>.GetGenericMethod(false, "Read", 1, 1)
            let bw = typeof<Writer>.GetProperty("BW", BindingFlags.NonPublic ||| BindingFlags.Instance)
            let br = typeof<Reader>.GetProperty("BR", BindingFlags.NonPublic ||| BindingFlags.Instance)
            let bwIntWriter = typeof<System.IO.BinaryWriter>.GetMethod("Write", [| typeof<int> |])
            let brIntReader = typeof<System.IO.BinaryReader>.GetMethod("ReadInt32")
            
        let writeInt (writer : Expression) (intExpr : Expression) =
            let bwExpr = Expression.Property(writer, bw)
            Expression.Call(bwExpr, bwIntWriter, intExpr) :> Expression

        let readInt (reader : Expression) =
            let brExpr = Expression.Property(reader, br)
            Expression.Call(brExpr, brIntReader) :> Expression

        /// initializes an empty object instance
        let initializeObject<'T> () =
            let obj = Expression.Call(objInitializer, constant typeof<'T>) 
            unbox typeof<'T> obj

        /// early register object to reader cache
        let earlyRegisterObject (reader : Expression) (instance : Expression) =
            Expression.Call(reader, readerInitializer, instance) :> Expression

        /// execute a collection of serialization actions
        let runSerializationActions (ms : MethodInfo []) (writerInstance : Expression) (instance : Expression) =
            let sctx = Expression.Property(writerInstance, writerCtx)
            ms |> Seq.map (fun m -> Expression.Call(instance, m, sctx) :> Expression)
             
        /// execute a collection of deserialization actions   
        let runDeserializationActions (ms : MethodInfo []) (readerInstance : Expression) (instance : Expression) =
            let sctx = Expression.Property(readerInstance, readerCtx)
            ms |> Seq.map (fun m -> Expression.Call(instance, m, sctx) :> Expression)

        /// execute a deserialization callback actions 
        let runDeserializationCallback (instance : Expression) =
            let dc = Expression.TypeAs(instance, typeof<IDeserializationCallback>)
            Expression.Call(dc, deserializationCallBack, constant null) :> Expression

        /// serialize a given value
        let write (writer : Expression) (formatter : Formatter) (value : Expression) =
            let ft = typedefof<Formatter<_>>.MakeGenericType [| formatter.Type |]
            let fExpr = Expression.Constant(formatter, ft)
            Expression.Call(writer, writerM.MakeGenericMethod [| formatter.Type |], fExpr, value) :> Expression

        /// deserialize a given value
        let read (reader : Expression) (formatter : Formatter) =
            let ft = typedefof<Formatter<_>>.MakeGenericType [| formatter.Type |]
            let fExpr = Expression.Constant(formatter, ft)
            Expression.Call(reader, readerM.MakeGenericMethod [| formatter.Type |], fExpr) :> Expression

        /// write a collection of formatter from a corresponding collection of properties
        let zipWriteProperties (properties : PropertyInfo []) (formatters : Formatter []) 
                                                    (writer : Expression) (instance : Expression) =
            let writeValue (formatter : Formatter, property : PropertyInfo) =
                let value = Expression.Property(instance, property)
                write writer formatter value

            Seq.zip formatters properties |> Seq.map writeValue

        /// read from a collection of formatters and pass to given constructor
        let callConstructor (ctor : ConstructorInfo) (formatters : Formatter []) (reader : Expression) =
            let values = Seq.map (read reader) formatters
            Expression.New(ctor, values) :> Expression

        /// read from a collection of formatters and pass to given static method
        let callMethod (methodInfo : MethodInfo) (formatters : Formatter []) (reader : Expression) =
            let values = Seq.map (read reader) formatters
            Expression.Call(methodInfo, values) :> Expression

        /// zip write a collection of fields to corresponding collection of formatters
        let zipWriteFields (fields : FieldInfo []) (formatters : Formatter []) 
                                            (writer : Expression) (instance : Expression) =

            let writeValue (formatter : Formatter, field : FieldInfo) =
                let value = Expression.Field(instance, field)
                write writer formatter value

            Seq.zip formatters fields |> Seq.map writeValue

        /// zip read a collection of formatters to corresponding collection of fields
        let zipReadFields (fields : FieldInfo []) (formatters : Formatter []) (reader : Expression) (instance : Expression) =
            let readValue (formatter : Formatter, field : FieldInfo) =
                let value = read reader formatter
                let field = Expression.Field(instance, field)
                Expression.Assign(field, value) :> Expression

            Seq.zip formatters fields |> Seq.map readValue

        // compilation provision for methods that carry the OnSerializing, OnSerialized, etc attributes
        let preComputeSerializationMethods<'T> (ms : MethodInfo []) =
            match ms with
            | [||] -> None
            | methods ->
                let dele =
                    compileAction2<StreamingContext,'T>(fun sc value ->
                        methods 
                        |> Array.map (fun m -> Expression.Call(value, m, sc) :> Expression)
                        |> Expression.Block :> _)

                Some dele

#endif