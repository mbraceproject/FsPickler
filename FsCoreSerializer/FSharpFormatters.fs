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
    open FsCoreSerializer.Utils
    open FsCoreSerializer.FormatterUtils


    // F# union types

    type FsUnionFormatter =
        static member CreateUntyped(unionType : Type, resolver : IFormatterResolver) =
            let m =
                typeof<FsUnionFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| unionType |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Formatter

        static member Create<'Union> (resolver : IFormatterResolver) =

#if EMIT_IL
            let unionInfo =
                FSharpType.GetUnionCases(typeof<'Union>, allMembers) 
                |> Array.map(fun uci ->
                    let fields = uci.GetFields()
                    let ctor = FSharpValue.PreComputeUnionConstructorInfo(uci, allMembers)
                    let formatters = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                    let caseType = if fields.Length = 0 then None else Some <| fields.[0].DeclaringType
                    uci, caseType, ctor, fields, formatters)

            let callUnionTagReader (union : Expression) =
                match FSharpValue.PreComputeUnionTagMemberInfo(typeof<'Union>, allMembers) with
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

            let writerFunc =
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

            let readerFunc =
                Expression.compileFunc1<Reader, 'Union>(fun reader ->
                    let tag = Expression.readInt reader
                    let cases =
                        unionInfo
                        |> Array.map (fun (uci,_,ctor,_,formatters) -> callCaseReader uci formatters ctor reader)

                    let defCase = Expression.failwith<InvalidOperationException, 'Union> "Invalid F# union tag."

                    Expression.Switch(tag, defCase, cases) :> _)

            let writer (w : Writer) (u : 'Union) = writerFunc.Invoke(w, u)
            let reader = readerFunc.Invoke
#else
            let tagReader = FSharpValue.PreComputeUnionTagReader(typeof<'Union>, memberBindings)
            
            let unionCases =
                FSharpType.GetUnionCases(typeof<'Union>, memberBindings) 
                |> Array.map (fun uci ->
                    let ctor = FSharpValue.PreComputeUnionConstructor(uci, memberBindings)
                    let reader = FSharpValue.PreComputeUnionReader(uci, memberBindings)
                    let formatters = uci.GetFields() |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                    ctor, reader, formatters)

            let writer (w : Writer) (x : 'Union) =
                let tag = tagReader x
                w.BW.Write tag
                let _,reader,formatters = unionCases.[tag]
                let values = reader x
                for i = 0 to values.Length - 1 do
                    formatters.[i].ManagedWrite w values.[i]

            let reader (r : Reader) =
                let tag = r.BR.ReadInt32()
                let ctor,_,formatters = unionCases.[tag]
                let values = Array.zeroCreate<obj> formatters.Length
                for i = 0 to formatters.Length - 1 do
                    values.[i] <- formatters.[i].ManagedRead r

                ctor values |> fastUnbox<'Union>
#endif

            new Formatter<'Union>(reader, writer, FormatterInfo.FSharpValue, cacheObj = false, useWithSubtypes = true)


    // System.Tuple<...> types

    type TupleFormatter =

        static member CreateUntyped(tupleType : Type, resolver : IFormatterResolver) =
            let m =
                typeof<TupleFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| tupleType |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Formatter
        
        static member Create<'Tuple>(resolver : IFormatterResolver) =

            let ctor,_ = FSharpValue.PreComputeTupleConstructorInfo typeof<'Tuple>

            let items = 
                typeof<'Tuple>.GetProperties()  
                |> Seq.filter (fun p -> p.Name.StartsWith("Item")) 
                |> Seq.sortBy (fun p -> p.Name)

            let nestedTuple = 
                match typeof<'Tuple>.GetProperty("Rest") with
                | null -> None
                | rest -> Some rest

            let elements = Seq.append items (Option.toList nestedTuple) |> Seq.toArray
            let formatters = elements |> Array.map (fun e -> resolver.Resolve e.PropertyType)

#if EMIT_IL 
            let writer =
                if elements.Length = 0 then fun _ _ -> ()
                else
                    let writer =
                        Expression.compileAction2<Writer, 'Tuple>(fun writer tuple ->
                            Expression.zipWriteProperties elements formatters writer tuple
                            |> Expression.Block :> _)

                    fun w t -> writer.Invoke(w,t)

            let reader =
                Expression.compileFunc1<Reader, 'Tuple>(fun reader ->
                    let values = formatters |> Seq.map (Expression.read reader)

                    Expression.New(ctor, values) :> _ ).Invoke 
#else
            let writer (w : Writer) (x : 'Tuple) =
                for i = 0 to elements.Length - 1 do
                    let o = elements.[i].GetValue(x)
                    formatters.[i].ManagedWrite w o

            let reader (r : Reader) =
                let values = Array.zeroCreate<obj> formatters.Length
                for i = 0 to values.Length - 1 do
                    values.[i] <- formatters.[i].ManagedRead r

                ctor.Invoke values |> fastUnbox<'Tuple>
#endif

#if OPTIMIZE_FSHARP
            // do not cache or perform subtype resolution for performance
            new Formatter<'Tuple>(reader, writer, FormatterInfo.FSharpValue, cacheObj = false, useWithSubtypes = true)
#else
            new Formatter<'Tuple>(reader, writer, FormatterInfo.Custom, cacheObj = true, useWithSubtypes = false)
#endif

    // F# record/exception types

    type FsRecordFormatter =

        static member CreateUntyped(t : Type, resolver : IFormatterResolver, isExceptionType) =
            let m =
                typeof<FsRecordFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            m.GuardedInvoke(null, [| resolver :> obj ; isExceptionType :> obj |]) :?> Formatter
        
        static member Create<'Record>(resolver : IFormatterResolver, isExceptionType) =

            let fields, ctor =
                if isExceptionType then
                    let fields = FSharpType.GetExceptionFields(typeof<'Record>, allMembers)
                    let signature = fields |> Array.map(fun p -> p.PropertyType)
                    let ctor = 
                        typeof<'Record>.GetConstructors(allMembers)
                        |> Array.find(fun c -> c.GetParameters () |> Array.map(fun p -> p.ParameterType) = signature)
                    fields, ctor
                else
                    let fields = FSharpType.GetRecordFields typeof<'Record>
                    let ctor = FSharpValue.PreComputeRecordConstructorInfo(typeof<'Record>, allMembers)
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
            let writer (w : Writer) (x : 'Record) =
                for i = 0 to fields.Length - 1 do
                    let o = fields.[i].GetValue x
                    formatters.[i].ManagedWrite w o
            
            let reader (r : Reader) =
                let values = Array.zeroCreate<obj> fields.Length
                for i = 0 to fields.Length - 1 do
                    values.[i] <- formatters.[i].ManagedRead r

                ctor.Invoke values |> fastUnbox<'Record>
#endif

            new Formatter<'Record>(reader, writer, FormatterInfo.FSharpValue, cacheObj = false, useWithSubtypes = false)