namespace FsCoreSerializer

    /// F# does not allow using Void in type arguments
    type internal SynVoid = class end

    module internal Expression =

        // LINQ expression utilities

        open System
        open System.Reflection
        open System.Linq.Expressions

        let inline insert (t : Type) =
            if t = typeof<SynVoid> then typeof<Void>
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
            let ctor = typeof<System.Tuple<'T,'U>>.GetConstructor([| insert typeof<'T> ; insert typeof<'U> |])
            Expression.New(ctor , [| e1 ; e2 |])

        let failwith<'exn, 'T when 'exn :> exn> (message : string) =
            let ctor = typeof<'exn>.GetConstructor [| typeof<string> |]
            let exn = Expression.New(ctor, Expression.Constant message)
            Expression.Throw(exn, insert typeof<'T>)

        let unbox (t : Type) (e : Expression) =
            if t.IsValueType then Expression.Unbox(e, t) :> Expression
            else Expression.Convert(e, t) :> Expression

        let box (e : Expression) = Expression.TypeAs(e, typeof<obj>) :> Expression

        let constant (t : 'T) = Expression.Constant(t, typeof<'T>) :> Expression

        let unboxElement (objArray : Expression) (idx : int) (t : Type) =
            unbox t <| Expression.ArrayIndex(objArray, Expression.Constant idx)

        /// calls constructor with arguments boxed in object array
        let callConstructorBoxed (ctorInfo : ConstructorInfo) (objArray : Expression) =
            let unboxedParams = 
                ctorInfo.GetParameters() 
                |> Array.mapi (fun i p -> unboxElement objArray i p.ParameterType)
            Expression.New(ctorInfo, unboxedParams)

        /// calls method with arguments boxed in object array
        let callMethodBoxed (methodInfo : MethodInfo) (instance : Expression option) (objArray : Expression) =
            let unboxedParams =
                methodInfo.GetParameters() 
                |> Array.mapi (fun i p -> unboxElement objArray i p.ParameterType)

            match instance with
            | None when methodInfo.IsStatic -> Expression.Call(methodInfo, unboxedParams)
            | Some instance when not methodInfo.IsStatic -> Expression.Call(instance, methodInfo, unboxedParams)
            | None -> invalidArg methodInfo.Name "Expected static method."
            | Some _ -> invalidArg methodInfo.Name "Expected non-static method."

        /// calls collection of property getters on given instance expression
        /// and returns an array expression of boxed results
        let callPropertyGettersBoxed (declaringType : Type) (properties : PropertyInfo []) (instance : Expression) =
            assert(properties |> Array.forall (fun p -> p.DeclaringType = declaringType))

            let calls = properties |> Seq.map (fun p -> Expression.Property(instance, p) |> box)
            Expression.NewArrayInit(typeof<obj>, calls)

        /// reads a collection of fields from given instance expression
        /// and returns an array expression of boxed results
        let readFieldsBoxed (declaringType : Type) (fields : FieldInfo []) (instance : Expression) =
            let isValidField (f : FieldInfo) = f.DeclaringType.IsAssignableFrom declaringType
            assert(Array.forall isValidField fields)

            let reads = fields |> Array.map (fun f -> Expression.Field(instance, f) |> box)
            Expression.NewArrayInit(typeof<obj>, reads)

        /// writes a collection of values to given fields on instance expression
        let writeFieldsBoxed (declaringType : Type) (fields : FieldInfo []) 
                                (instance : Expression) (boxedParamArray : Expression) =

            let isValidField (f : FieldInfo) = f.DeclaringType.IsAssignableFrom declaringType
            assert(Array.forall isValidField fields)

            if fields.Length = 0 then Expression.Empty() :> Expression
            else

                let assignExprs = 
                    fields |> Array.mapi (fun i f -> 
                        let unboxedElem = unboxElement boxedParamArray i f.FieldType
                        let fieldExpr = Expression.Field(instance, f)
                        Expression.Assign(fieldExpr, unboxedElem) :> Expression)

                Expression.Block assignExprs :> Expression

        let returnUnit (block : Expression) =
            Expression.Block(block, constant ()) :> Expression