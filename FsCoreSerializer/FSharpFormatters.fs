module internal FsCoreSerializer.FSharpFormatters

    open System
    open System.IO
    open System.Reflection
    open System.Threading
#if EMIT_IL
    open System.Linq.Expressions
#endif
    open System.Runtime.Serialization

    open Microsoft.FSharp.Reflection

    open FsCoreSerializer
    open FsCoreSerializer.FormatterUtils


    // F# union types

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
                    let caseType = if fields.Length = 0 then None else Some <| fields.[0].DeclaringType
                    uci, caseType, ctor, fields, formatters)

#if EMIT_IL
            let callUnionTagReader (union : Expression) =
                match FSharpValue.PreComputeUnionTagMemberInfo(typeof<'Union>, memberBindings) with
                | null -> invalidOp "unexpected error"
                | :? PropertyInfo as p -> Expression.Property(union, p) :> Expression
                | :? MethodInfo as m when m.IsStatic -> Expression.Call(m, union) :> Expression
                | :? MethodInfo as m -> Expression.Call(union, m) :> Expression
                | _ -> invalidOp "unexpected error"

            let callCaseWriter (uci : UnionCaseInfo) (caseType : Type option) (fields : PropertyInfo []) 
                                (formatters : Formatter []) (writer : Expression) (instance : Expression) =

                let body =
                    if fields.Length = 0 then Expression.Empty() :> Expression
                    else
                        let instance =
                            match caseType with
                            | Some t -> Expression.TypeAs(instance, t) :> Expression
                            | None -> instance

                        Expression.zipWriteProperties fields formatters writer instance |> Expression.Block :> _

                Expression.SwitchCase(body, Expression.constant uci.Tag)


            let callCaseReader (uci : UnionCaseInfo) (formatters : Formatter []) (ctor : MethodInfo) (reader : Expression) =
                let unionCase = Expression.callMethod ctor formatters reader
                Expression.SwitchCase(unionCase, Expression.constant uci.Tag)

            let writer =
                Expression.compileAction2<Writer, 'Union>(fun writer union ->
                    let tag = Expression.Variable(typeof<int>, "tag")
                    let cases = 
                        unionInfo 
                        |> Array.map (fun (uci,caseType,_,fields,formatters) -> 
                            callCaseWriter uci caseType fields formatters writer union)

                    let defCase = Expression.failwith<InvalidOperationException, Expression.SynVoid> "Invalid F# union tag."

                    let body =
                        [|
                            Expression.Assign(tag, callUnionTagReader union) :> Expression
                            Expression.writeInt writer tag
                            Expression.Switch(tag, defCase, cases) :> _
                        |]

                    Expression.Block([| tag |] , body) :> _)

            let reader =
                Expression.compileFunc1<Reader, 'Union>(fun reader ->
                    let tag = Expression.readInt reader
                    let cases =
                        unionInfo
                        |> Array.map (fun (uci,_,ctor,_,formatters) -> callCaseReader uci formatters ctor reader)

                    let defCase = Expression.failwith<InvalidOperationException, 'Union> "Invalid F# union tag."

                    Expression.Switch(tag, defCase, cases) :> _)


            new Formatter<'Union>(reader.Invoke, (fun w t -> writer.Invoke(w,t)), FormatterInfo.FSharpValue, true, true)
#else
#endif

    // System.Tuple<...> types

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
                |> Seq.toArray

            let formatters = elements |> Array.map (fun e -> resolver.Resolve e.PropertyType)

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
                                yield! Expression.zipWriteProperties elements formatters writer tuple

                                match nestedTuple with
                                | None -> ()
                                | Some (prop, f) ->
                                    let nested = Expression.Property(tuple, prop)
                                    yield Expression.write writer f nested
                            } |> Expression.Block :> _)

                    fun w t -> writer.Invoke(w,t)

            let reader =
                Expression.compileFunc1<Reader, 'Tuple>(fun reader ->
                    let values = formatters |> Seq.map (Expression.read reader)

                    match nestedTuple with
                    | None -> Expression.New(ctor, values) :> _
                    | Some (_, fmt) ->
                        let nestedValue = Expression.read reader fmt
                        Expression.New(ctor, seq { yield! values ; yield nestedValue }) :> _).Invoke        
#else
#endif

#if OPTIMIZE_FSHARP
            new Formatter<'Tuple>(reader, writer, FormatterInfo.FSharpValue, false, true)
#else
            new Formatter<'Tuple>(reader, writer, FormatterInfo.Custom, true, false)
#endif

    // F# record/exception types

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

            let formatters = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)

#if EMIT_IL
            let writer =
                if fields.Length = 0 then fun _ _ -> ()
                else
                    let writer =
                        Expression.compileAction2<Writer, 'Record>(fun writer record ->
                            Expression.zipWriteProperties fields formatters writer record |> Expression.Block :> _)

                    fun w t -> writer.Invoke(w,t)

            let reader =
                Expression.compileFunc1<Reader, 'Record>(fun reader ->
                    let values = formatters |> Seq.map (Expression.read reader)

                    Expression.New(ctor, values) :> _).Invoke
#else
#endif
            new Formatter<'Record>(reader, writer, FormatterInfo.FSharpValue, cacheObj = true, useWithSubtypes = false)