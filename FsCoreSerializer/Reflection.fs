namespace FsCoreSerializer

    module internal Reflection =

        open System
        open System.Reflection
        open System.Linq.Expressions

        open Microsoft.FSharp.Reflection
    
        let inline isPrivateRepresentation (t : Type) =
            let msg = sprintf "The type '%s' has private representation." t.Name 
            invalidArg "bindingFlags" (msg + " You must specify BindingFlags.NonPublic to access private type representations.")
    
        type FsTuple(tupleType : Type) =

            static let preComputeTupleReader (tupleType : Type) =
                let rec traverseTuple (tupleType : Type) (tupleExpr : Expression) =
                    let fieldExprs = 
                        tupleType.GetProperties()  
                        |> Seq.filter (fun p -> p.Name.StartsWith("Item")) 
                        |> Seq.sortBy (fun p -> p.Name)
                        |> Seq.map (fun p -> Expression.Property(tupleExpr, p) |> Expression.box)

                    let restExprs =
                        match tupleType.GetProperty("Rest") with
                        | null -> Seq.empty
                        | rest ->
                            let restExpr = Expression.Property(tupleExpr, rest)
                            traverseTuple rest.PropertyType restExpr

                    Seq.append fieldExprs restExprs

                if not <| FSharpType.IsTuple tupleType then
                    invalidArg "tupleType" <| sprintf "Type '%s' is not a tuple type." tupleType.Name


                Expression.compile1<obj, obj []>(fun param ->
                    let fieldExprs = traverseTuple tupleType (param |> Expression.unbox tupleType)
                    Expression.NewArrayInit(typeof<obj>, fieldExprs) :> _)

            static let preComputeTupleConstructor (tupleType : Type) =
                let rec composeTuple (objArray : Expression) (tupleType : Type) offset =
                    let ctorInfo, nested = FSharpValue.PreComputeTupleConstructorInfo tupleType

                    let unboxElem i (p : ParameterInfo) = 
                        Expression.unboxElement objArray (i+offset) p.ParameterType

                    match nested with
                    | None ->
                        let paramExprs = ctorInfo.GetParameters() |> Seq.mapi unboxElem
                        Expression.New(ctorInfo, paramExprs)
                    | Some restType ->
                        let ctorParams = ctorInfo.GetParameters()
                        let n = ctorParams.Length
                        let fieldExprs = ctorParams |> Seq.take (n - 1) |> Seq.mapi unboxElem
                        let restExprs = composeTuple objArray restType (offset + n - 1)
                        Expression.New(ctorInfo, Seq.append fieldExprs [| restExprs |])

                Expression.compile1<obj [], obj>(fun boxedParams -> 
                    composeTuple boxedParams tupleType 0 |> Expression.box)

            let elements = FSharpType.GetTupleElements tupleType

#if EMIT_IL
            let reader = preComputeTupleReader tupleType
            let ctor = preComputeTupleConstructor tupleType

            member __.Compose(values : obj []) = ctor.Invoke values
            member __.Decompose(tuple : obj) = reader.Invoke tuple
#else
            let reader = FSharpValue.PreComputeTupleReader tupleType
            let ctor = FSharpValue.PreComputeTupleConstructor tupleType

            member __.Compose(values : obj []) = ctor values
            member __.Decompose(tuple : obj) = reader tuple
#endif
            member __.Elements = elements

        //
        //  F# Discriminated Unions
        //

        type FsUnion(unionType : Type, ?bindingFlags) =

            static let preComputeUnionReader (unionType : Type) (ucis : UnionCaseInfo []) bindingFlags =
                let callUnionTagReader (union : Type) bindingFlags (instance : Expression) =
                    match FSharpValue.PreComputeUnionTagMemberInfo(union, ?bindingFlags = bindingFlags) with
                    | null -> isPrivateRepresentation union
                    | :? PropertyInfo as p -> Expression.Property(instance, p) :> Expression
                    | :? MethodInfo as m when m.IsStatic -> Expression.Call(m, instance) :> Expression
                    | :? MethodInfo as m -> Expression.Call(instance, m) :> Expression
                    | _ -> invalidOp "unexpected error"

                let defaultBody = 
                    Expression.failwith<InvalidOperationException, int * obj []> "Invalid F# union tag."

                let getBranchCase (instance : Expression) (uci : UnionCaseInfo) =
                    let fields = uci.GetFields()
                    let branchType = if fields.Length = 0 then uci.DeclaringType else fields.[0].DeclaringType
                    let unboxedInstance = Expression.unbox branchType instance
                    let values = Expression.callPropertyGettersBoxed branchType fields unboxedInstance :> Expression
                    let result = Expression.pair<int, obj []>(Expression.Constant uci.Tag, values)
                    Expression.SwitchCase(result, Expression.Constant uci.Tag)

                Expression.compile1<obj, int * obj []>(fun boxedInstance ->
                    let unboxedInstance = Expression.unbox unionType boxedInstance
                    let tag = callUnionTagReader unionType bindingFlags unboxedInstance
                    let cases = ucis |> Array.map (getBranchCase unboxedInstance)
                    Expression.Switch(tag, defaultBody, cases) :> _)

            static let preComputeUnionConstructor (union : Type) (ucis : UnionCaseInfo []) bindingFlags =
                let defaultBody = Expression.failwith<ArgumentException, obj>("Supplied F# union tag is out of range.")
                let getBranchCtor (boxedArgs : Expression) (uci : UnionCaseInfo) =
                    let ctor = FSharpValue.PreComputeUnionConstructorInfo(uci, ?bindingFlags = bindingFlags)
                    let result = Expression.callMethodBoxed ctor None boxedArgs |> Expression.box
                    Expression.SwitchCase(result, Expression.Constant uci.Tag)

                Expression.compile2<int, obj [], obj>(fun tag args ->
                    let branchCtors = ucis |> Array.map (getBranchCtor args)
                    Expression.Switch(tag, defaultBody, branchCtors) :> _)


            let ucis = FSharpType.GetUnionCases(unionType, ?bindingFlags = bindingFlags)
            // resolve the actual union type, not a subtype
            let unionType = ucis.[0].DeclaringType

#if EMIT_IL
            let reader = preComputeUnionReader unionType ucis bindingFlags
            let ctor = preComputeUnionConstructor unionType ucis bindingFlags

            member __.Decompose(union : obj) = reader.Invoke union
            member __.Compose(tag : int, values : obj []) = ctor.Invoke(tag, values)
#else
            let tagReader = FSharpValue.PreComputeUnionTagReader(unionType, ?bindingFlags = bindingFlags)
            let tagMap = 
                ucis    |> Seq.map (fun u -> 
                                let reader = FSharpValue.PreComputeUnionReader(u, ?bindingFlags = bindingFlags)
                                let ctor = FSharpValue.PreComputeUnionConstructor(u, ?bindingFlags = bindingFlags)
                                u.Tag, (u, reader, ctor))

                        |> Map.ofSeq

            member __.Decompose(union : obj) = let tag = tagReader union in let (_,reader,_) = tagMap.[tag] in tag, reader union
            member __.Compose(tag : int, values : obj []) = let (_,_,ctor) = tagMap.[tag] in ctor values
#endif
            member __.Type = unionType
            member __.UCIs = ucis


        //
        //  F# records
        //

        type FsRecord(recordType : Type, ?bindingFlags) =
            static let preComputeRecordConstructor (record : Type) bindingFlags =
                let ctor = FSharpValue.PreComputeRecordConstructorInfo(record, ?bindingFlags = bindingFlags)

                Expression.compile1<obj [], obj>(fun e -> Expression.callConstructorBoxed ctor e |> Expression.box)

            static let preComputeRecordReader (record : Type) (fields : PropertyInfo []) =
                Expression.compile1<obj, obj []>(fun e -> 
                    let ue = Expression.unbox record e
                    Expression.callPropertyGettersBoxed record fields ue :> _)

        
            let fields = FSharpType.GetRecordFields(recordType, ?bindingFlags = bindingFlags)
        
#if EMIT_IL
            let reader = preComputeRecordReader recordType fields
            let ctor = preComputeRecordConstructor recordType bindingFlags

            member __.Decompose(record : obj) = reader.Invoke record
            member __.Compose(fields : obj []) = ctor.Invoke fields
#else
            let reader = FSharpValue.PreComputeRecordReader(recordType, ?bindingFlags = bindingFlags)
            let ctor = FSharpValue.PreComputeRecordConstructor(recordType, ?bindingFlags = bindingFlags)

            member __.Decompose(record : obj) = reader record
            member __.Compose(fields : obj []) = ctor fields
#endif
            member __.Fields = fields

#if EMIT_IL

        //
        //  F# exceptions
        //

        type FsException(exceptionType : Type, ?bindingFlags) =

            // an implementation that curiously does not exist in Microsoft.FSharp.Reflection
            static let preComputeExceptionConstructorInfo (exceptionType : Type) bindingFlags : ConstructorInfo =
                let signature = 
                    FSharpType.GetExceptionFields(exceptionType, ?bindingFlags = bindingFlags) 
                    |> Array.map(fun f -> f.PropertyType)

                let ctors = 
                    match bindingFlags with 
                    | Some f -> exceptionType.GetConstructors (f ||| BindingFlags.Instance) 
                    | None -> exceptionType.GetConstructors()

                let testCtor (ctor : ConstructorInfo) = 
                    ctor.GetParameters() |> Array.map (fun p -> p.ParameterType) = signature

                match Array.tryFind testCtor ctors with
                | None -> isPrivateRepresentation exceptionType
                | Some ctorInfo -> ctorInfo

            static let preComputeExceptionConstructor (exceptionType : Type) bindingFlags =
                let ctor = preComputeExceptionConstructorInfo exceptionType bindingFlags

                Expression.compile1<obj [], obj>(fun e -> Expression.callConstructorBoxed ctor e |> Expression.box)

            static let preComputeExceptionReader (exceptionType : Type) (fields : PropertyInfo []) bindingFlags =
                Expression.compile1<obj, obj []>(fun e -> 
                    let ue = Expression.unbox exceptionType e
                    Expression.callPropertyGettersBoxed exceptionType fields ue :> _)


            let fields = FSharpType.GetExceptionFields(exceptionType, ?bindingFlags = bindingFlags)
            let reader = preComputeExceptionReader exceptionType fields bindingFlags
            let ctor = preComputeExceptionConstructor exceptionType bindingFlags

            member __.Decompose(exn : obj) = reader.Invoke exn
            member __.Compose(fields : obj []) = ctor.Invoke fields
            member __.Fields = fields
#endif