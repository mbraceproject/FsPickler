namespace FsPickler

    // LINQ expression utilities

#if EMIT_IL

    open System
    open System.Reflection
    open System.Reflection.Emit
    open System.Linq.Expressions
    open System.Runtime.Serialization

    open FsPickler
    open FsPickler.Utils
    open FsPickler.PicklerUtils

#if COMPILETOMETHOD
    type DynamicAssembly private () =
        
        static let assemblyName = AssemblyName("FsPicklerDynamicAssembly")
        static let getUUID () = Guid.NewGuid().ToString("N")

        static let moduleBuilder =
            let dynAsmb = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run)
            dynAsmb.DefineDynamicModule("PicklerModule")

        static member CompileExpr(expr : LambdaExpression) =
            let methodName = getUUID()
            let typeBuilder = moduleBuilder.DefineType(getUUID (), TypeAttributes.Public)
            let methodBuilder = typeBuilder.DefineMethod(methodName, MethodAttributes.Public ||| MethodAttributes.Static)
            expr.CompileToMethod(methodBuilder)
            let ty = typeBuilder.CreateType()
            ty.GetMethod(methodName)
#endif

    module internal Expression =

        /// F# does not allow using Void in type arguments
        type SynVoid = class end
        with 
            static member Replace(t : System.Type) =
                if t = typeof<SynVoid> then typeof<System.Void>
                else t

        let compileFunc1<'U, 'V>(f : Expression -> Expression) =
            let parameter = Expression.Parameter(typeof<'U>)
            let lambda = Expression.Lambda<Func<'U,'V>>(f parameter, parameter)
#if COMPILETOMETHOD
            let meth = DynamicAssembly.CompileExpr lambda
            Func<'U,'V>(fun u -> meth.Invoke(null, [| box u |]) :?> 'V)
#else
            lambda.Compile()
#endif

        let compileFunc2<'U1, 'U2, 'V>(f : Expression -> Expression -> Expression) =
            let p1 = Expression.Parameter typeof<'U1>
            let p2 = Expression.Parameter typeof<'U2>
            let lambda = Expression.Lambda<Func<'U1,'U2,'V>>(f p1 p2, p1, p2)
#if COMPILETOMETHOD
            let meth = DynamicAssembly.CompileExpr lambda
            Func<'U1, 'U2, 'V>(fun u1 u2 -> meth.Invoke(null, [| box u1; box u2 |]) :?> 'V)
#else
            lambda.Compile()
#endif

        let compileFunc3<'U1, 'U2, 'U3, 'V>(f : Expression -> Expression -> Expression -> Expression) =
            let p1 = Expression.Parameter typeof<'U1>
            let p2 = Expression.Parameter typeof<'U2>
            let p3 = Expression.Parameter typeof<'U3>
            let lambda = Expression.Lambda<Func<'U1,'U2,'V>>(f p1 p2 p3, p1, p2, p3)
#if COMPILETOMETHOD
            let meth = DynamicAssembly.CompileExpr lambda
            Func<'U1, 'U2, 'U3, 'V>(fun u1 u2 u3 -> meth.Invoke(null, [| box u1; box u2; box u3 |]) :?> 'V)
#else
            lambda.Compile()
#endif

        let compileAction1<'U1>(f : Expression -> Expression) =
            let p1 = Expression.Parameter typeof<'U1>
            let lambda = Expression.Lambda<Action<'U1>>(f p1, p1)
#if COMPILETOMETHOD
            let meth = DynamicAssembly.CompileExpr lambda
            Action<'U1>(fun u -> meth.Invoke(null, [| box u |]) |> ignore)
#else
            lambda.Compile()
#endif

        let compileAction2<'U1, 'U2>(f : Expression -> Expression -> Expression) =
            let p1 = Expression.Parameter typeof<'U1>
            let p2 = Expression.Parameter typeof<'U2>
            let lambda = Expression.Lambda<Action<'U1, 'U2>>(f p1 p2, p1, p2)
#if COMPILETOMETHOD
            let meth = DynamicAssembly.CompileExpr lambda
            Action<'U1, 'U2>(fun u1 u2 -> meth.Invoke(null, [| box u1; box u2 |]) |> ignore)
#else
            lambda.Compile()
#endif
        let compileAction3<'U1, 'U2, 'U3>(f : Expression -> Expression -> Expression -> Expression) =
            let p1 = Expression.Parameter typeof<'U1>
            let p2 = Expression.Parameter typeof<'U2>
            let p3 = Expression.Parameter typeof<'U3>
            let lambda = Expression.Lambda<Action<'U1, 'U2, 'U3>>(f p1 p2 p3, p1, p2, p3)
#if COMPILETOMETHOD
            let meth = DynamicAssembly.CompileExpr lambda
            Action<'U1, 'U2, 'U3>(fun u1 u2 u3 -> meth.Invoke(null, [| box u1; box u2; box u3 |]) |> ignore)
#else
            lambda.Compile()
#endif

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

            type ObjInitializer =
                static member Init<'T> () = FormatterServices.GetUninitializedObject typeof<'T>

            let writerCtx = typeof<Writer>.GetProperty("StreamingContext")
            let readerCtx = typeof<Reader>.GetProperty("StreamingContext")
            let objInitializer = typeof<ObjInitializer>.GetMethod("Init", BindingFlags.NonPublic ||| BindingFlags.Static)
            let deserializationCallBack = typeof<IDeserializationCallback>.GetMethod("OnDeserialization")
            let writerM = typeof<Writer>.GetGenericMethod(false, "Write", 1, 2)
            let readerM = typeof<Reader>.GetGenericMethod(false, "Read", 1, 1)
            let bw = typeof<Writer>.GetProperty("BinaryWriter", BindingFlags.Public ||| BindingFlags.Instance)
            let br = typeof<Reader>.GetProperty("BinaryReader", BindingFlags.Public ||| BindingFlags.Instance)
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
            let obj = Expression.Call(objInitializer.MakeGenericMethod typeof<'T>) 
            unbox typeof<'T> obj

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
        let write (writer : Expression) (picklers : Expression) (picklerType : Type) (picklerIdx : int) (value : Expression) =
            let ft = typedefof<Pickler<_>>.MakeGenericType [| picklerType |]
            let pickler = unbox ft <| Expression.ArrayIndex(picklers, constant picklerIdx)
            Expression.Call(writer, writerM.MakeGenericMethod [| picklerType |], pickler, value) :> Expression

        /// deserialize a given value
        let read (reader : Expression) (picklers : Expression) (picklerType : Type) (picklerIdx : int) =
            let ft = typedefof<Pickler<_>>.MakeGenericType [| picklerType |]
            let pickler = unbox ft <| Expression.ArrayIndex(picklers, constant picklerIdx)
            Expression.Call(reader, readerM.MakeGenericMethod [| picklerType |], pickler) :> Expression

        /// write a collection of pickler from a corresponding collection of properties
        let zipWriteProperties (properties : PropertyInfo []) (picklers : Expression) 
                                                    (writer : Expression) (instance : Expression) =

            let writeValue (idx : int) (property : PropertyInfo) =
                let value = Expression.Property(instance, property)
                write writer picklers property.PropertyType idx value

            Seq.mapi writeValue properties

        let zipReadProperties (properties : PropertyInfo []) (picklers : Expression) (reader : Expression) =
            properties |> Seq.mapi (fun i p -> read reader picklers p.PropertyType i)

        /// read from a collection of picklers and pass to given constructor
        let callConstructor (ctor : ConstructorInfo) (ctorParams : Type []) (picklers : Expression) (reader : Expression) =
            let values = ctorParams |> Seq.mapi (fun i t -> read reader picklers t i)
            Expression.New(ctor, values) :> Expression

        /// read from a collection of picklers and pass to given static method
        let callMethod (methodInfo : MethodInfo) (methodParams : Type []) (picklers : Expression) (reader : Expression) =
            let values = methodParams |> Seq.mapi (fun i t -> read reader picklers t i)
            Expression.Call(methodInfo, values) :> Expression

        /// zip write a collection of fields to corresponding collection of picklers
        let zipWriteFields (fields : FieldInfo []) (picklers : Expression) (writer : Expression) (instance : Expression) =

            let writeValue (idx : int) (field : FieldInfo) =
                let value = Expression.Field(instance, field)
                write writer picklers field.FieldType idx value

            Seq.mapi writeValue fields

        /// zip read a collection of picklers to corresponding collection of fields
        let zipReadFields (fields : FieldInfo []) (picklers : Expression) (reader : Expression) (instance : Expression) =
            let readValue (idx : int) (field : FieldInfo) =
                let value = read reader picklers field.FieldType idx
                let field = Expression.Field(instance, field)
                Expression.Assign(field, value) :> Expression

            Seq.mapi readValue fields

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