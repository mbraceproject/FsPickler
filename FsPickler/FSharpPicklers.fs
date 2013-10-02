module internal FsPickler.FSharpPicklers

    open System
    open System.IO
    open System.Reflection
    open System.Threading
#if EMIT_IL
    open System.Linq.Expressions
#endif
    open System.Runtime.Serialization

    open Microsoft.FSharp.Reflection

    open FsPickler
    open FsPickler.Utils
    open FsPickler.PicklerUtils


    // F# union types

    type FsUnionPickler =
        static member CreateUntyped(unionType : Type, resolver : IPicklerResolver) =
            let m =
                typeof<FsUnionPickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| unionType |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Pickler

        static member Create<'Union> (resolver : IPicklerResolver) =

#if EMIT_IL
            let unionInfo =
                FSharpType.GetUnionCases(typeof<'Union>, allMembers) 
                |> Array.map(fun uci ->
                    let fields = uci.GetFields()
                    let ctor = FSharpValue.PreComputeUnionConstructorInfo(uci, allMembers)
                    let picklers = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                    let caseType = if fields.Length = 0 then None else Some <| fields.[0].DeclaringType
                    uci, caseType, ctor, fields, picklers)

            let callUnionTagReader (union : Expression) =
                match FSharpValue.PreComputeUnionTagMemberInfo(typeof<'Union>, allMembers) with
                | null -> invalidOp "unexpected error"
                | :? PropertyInfo as p -> Expression.Property(union, p) :> Expression
                | :? MethodInfo as m when m.IsStatic -> Expression.Call(m, union) :> Expression
                | :? MethodInfo as m -> Expression.Call(union, m) :> Expression
                | _ -> invalidOp "unexpected error"

            let callCaseWriter (uci : UnionCaseInfo) (caseType : Type option) (fields : PropertyInfo []) 
                                (picklers : Pickler []) (writer : Expression) (instance : Expression) =

                let body =
                    if fields.Length = 0 then Expression.Empty() :> Expression
                    else
                        let instance =
                            match caseType with
                            | Some t -> Expression.TypeAs(instance, t) :> Expression
                            | None -> instance

                        Expression.zipWriteProperties fields picklers writer instance |> Expression.Block :> _

                Expression.SwitchCase(body, Expression.constant uci.Tag)


            let callCaseReader (uci : UnionCaseInfo) (picklers : Pickler []) (ctor : MethodInfo) (reader : Expression) =
                let unionCase = Expression.callMethod ctor picklers reader
                Expression.SwitchCase(unionCase, Expression.constant uci.Tag)

            let writerFunc =
                Expression.compileAction2<Writer, 'Union>(fun writer union ->
                    let tag = Expression.Variable(typeof<int>, "tag")
                    let cases = 
                        unionInfo 
                        |> Array.map (fun (uci,caseType,_,fields,picklers) -> 
                            callCaseWriter uci caseType fields picklers writer union)

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
                        |> Array.map (fun (uci,_,ctor,_,picklers) -> callCaseReader uci picklers ctor reader)

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
                    let picklers = uci.GetFields() |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                    ctor, reader, picklers)

            let writer (w : Writer) (x : 'Union) =
                let tag = tagReader x
                w.BinaryWriter.Write tag
                let _,reader,picklers = unionCases.[tag]
                let values = reader x
                for i = 0 to values.Length - 1 do
                    picklers.[i].ManagedWrite w values.[i]

            let reader (r : Reader) =
                let tag = r.BinaryReader.ReadInt32()
                let ctor,_,picklers = unionCases.[tag]
                let values = Array.zeroCreate<obj> picklers.Length
                for i = 0 to picklers.Length - 1 do
                    values.[i] <- picklers.[i].ManagedRead r

                ctor values |> fastUnbox<'Union>
#endif

            new Pickler<'Union>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)


    // System.Tuple<...> types

    type TuplePickler =

        static member CreateUntyped(tupleType : Type, resolver : IPicklerResolver) =
            let m =
                typeof<TuplePickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| tupleType |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Pickler
        
        static member Create<'Tuple>(resolver : IPicklerResolver) =

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
            let picklers = elements |> Array.map (fun e -> resolver.Resolve e.PropertyType)

#if EMIT_IL 
            let writer =
                if elements.Length = 0 then fun _ _ -> ()
                else
                    let writer =
                        Expression.compileAction2<Writer, 'Tuple>(fun writer tuple ->
                            Expression.zipWriteProperties elements picklers writer tuple
                            |> Expression.Block :> _)

                    fun w t -> writer.Invoke(w,t)

            let reader =
                Expression.compileFunc1<Reader, 'Tuple>(fun reader ->
                    let values = picklers |> Seq.map (Expression.read reader)

                    Expression.New(ctor, values) :> _ ).Invoke 
#else
            let writer (w : Writer) (x : 'Tuple) =
                for i = 0 to elements.Length - 1 do
                    let o = elements.[i].GetValue(x)
                    picklers.[i].ManagedWrite w o

            let reader (r : Reader) =
                let values = Array.zeroCreate<obj> picklers.Length
                for i = 0 to values.Length - 1 do
                    values.[i] <- picklers.[i].ManagedRead r

                ctor.Invoke values |> fastUnbox<'Tuple>
#endif

#if OPTIMIZE_FSHARP
            // do not cache or perform subtype resolution for performance
            new Pickler<'Tuple>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            new Pickler<'Tuple>(reader, writer, PicklerInfo.Custom, cacheByRef = true, useWithSubtypes = false)
#endif

    // F# record/exception types

    type FsRecordPickler =

        static member CreateUntyped(t : Type, resolver : IPicklerResolver, isExceptionType) =
            let m =
                typeof<FsRecordPickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            m.GuardedInvoke(null, [| resolver :> obj ; isExceptionType :> obj |]) :?> Pickler
        
        static member Create<'Record>(resolver : IPicklerResolver, isExceptionType) =

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

            let picklers = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)

#if EMIT_IL
            let writer =
                if fields.Length = 0 then fun _ _ -> ()
                else
                    let writer =
                        Expression.compileAction2<Writer, 'Record>(fun writer record ->
                            Expression.zipWriteProperties fields picklers writer record |> Expression.Block :> _)

                    fun w t -> writer.Invoke(w,t)

            let reader =
                Expression.compileFunc1<Reader, 'Record>(fun reader ->
                    let values = picklers |> Seq.map (Expression.read reader)

                    Expression.New(ctor, values) :> _).Invoke
#else
            let writer (w : Writer) (x : 'Record) =
                for i = 0 to fields.Length - 1 do
                    let o = fields.[i].GetValue x
                    picklers.[i].ManagedWrite w o
            
            let reader (r : Reader) =
                let values = Array.zeroCreate<obj> fields.Length
                for i = 0 to fields.Length - 1 do
                    values.[i] <- picklers.[i].ManagedRead r

                ctor.Invoke values |> fastUnbox<'Record>
#endif

            new Pickler<'Record>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = false)
