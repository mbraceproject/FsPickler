module internal FsPickler.FSharpPicklers

    open System
    open System.IO
    open System.Reflection
    open System.Threading
#if EMIT_IL
    open System.Reflection.Emit
    open FsPickler.Emit
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
            // resolve tag reader methodInfo
            let tagReaderMethod =
                match FSharpValue.PreComputeUnionTagMemberInfo(typeof<'Union>, allMembers) with
                | null -> invalidOp "unexpected error"
                | :? PropertyInfo as p -> p.GetGetMethod(true)
                | :? MethodInfo as m -> m
                | _ -> invalidOp "unexpected error"

            let tagReader = Delegate.CreateDelegate<Func<'Union,int>> tagReaderMethod

#if EMIT_IL
            let buildCaseWriter (caseType : Type) fields =
                
                DynamicMethod.compileAction3<Pickler [], Writer, 'Union> "unionSerializer" (fun ilGen ->
                    let picklers = EnvItem<Pickler []>.Arg0
                    let writer = EnvItem<Writer>.Arg1
                    let union = EnvItem<'Union>.InitVar(ilGen, reflectedType = caseType)

                    // push union and downcast
                    ilGen.Emit(OpCodes.Ldarg_2)
                    ilGen.Emit(OpCodes.Castclass, caseType)
                    union.Store ilGen

                    emitSerializeProperties fields writer picklers union ilGen)

            let buildCaseReader ctor =
                DynamicMethod.compileFunc2<Pickler [], Reader, 'Union> "unionDeserializer" (fun ilGen ->
                    let picklers = EnvItem<Pickler []>.Arg0
                    let reader = EnvItem<Reader>.Arg1

                    emitDeserializeAndConstruct (Choice1Of2 ctor) (ctor.GetParameterTypes()) reader picklers ilGen
                    
                    ilGen.Emit(OpCodes.Ret))

            let caseSerializers =
                FSharpType.GetUnionCases(typeof<'Union>, allMembers)
                |> Array.sortBy(fun uci -> uci.Tag)
                |> Array.map (fun uci ->
                    let fields = uci.GetFields()
                    let ctor = FSharpValue.PreComputeUnionConstructorInfo(uci, allMembers)
                    let picklers = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                    let caseWriter = 
                        if fields.Length = 0 then None
                        else
                            let caseType = fields.[0].DeclaringType
                            Some <| buildCaseWriter caseType fields

                    let caseReader = buildCaseReader ctor
                    picklers, caseWriter, caseReader)


            let writer (w : Writer) (u : 'Union) =
                let tag = tagReader.Invoke u

                let picklers,caseWriter,_ = caseSerializers.[tag]

                w.BinaryWriter.Write (byte tag)

                match caseWriter with
                | None -> ()
                | Some cw -> cw.Invoke(picklers, w, u)

            let reader (r : Reader) =
                let tag = int (r.BinaryReader.ReadByte())

                let picklers,_,caseReader = caseSerializers.[tag]
                caseReader.Invoke(picklers, r)

#else
            let unionCases =
                FSharpType.GetUnionCases(typeof<'Union>, allMembers) 
                |> Array.map (fun uci ->
                    let ctor = FSharpValue.PreComputeUnionConstructor(uci, allMembers)
                    let reader = FSharpValue.PreComputeUnionReader(uci, allMembers)
                    let picklers = uci.GetFields() |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                    ctor, reader, picklers)

            let writer (w : Writer) (x : 'Union) =
                let tag = tagReader.Invoke x
                w.BinaryWriter.Write (byte tag)
                let _,reader,picklers = unionCases.[tag]
                let values = reader x
                for i = 0 to values.Length - 1 do
                    picklers.[i].ManagedWrite w values.[i]

            let reader (r : Reader) =
                let tag = int (r.BinaryReader.ReadByte())
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
                    let writerDele =
                        DynamicMethod.compileAction3<Pickler [], Writer, 'Tuple> "tupleSerializer" (fun ilGen ->
                            let picklers = EnvItem<Pickler []>.Arg0
                            let writer = EnvItem<Writer>.Arg1
                            let tuple = EnvItem<'Tuple>.Arg2

                            emitSerializeProperties elements writer picklers tuple ilGen)

                    fun w t -> writerDele.Invoke(picklers, w, t)

            let readerDele =
                DynamicMethod.compileFunc2<Pickler [], Reader, 'Tuple> "tupleDeserializer" (fun ilGen ->
                    let picklers = EnvItem<Pickler []>.Arg0
                    let reader = EnvItem<Reader>.Arg1

                    emitDeserializeAndConstruct (Choice2Of2 ctor) (ctor.GetParameterTypes()) reader picklers ilGen
                    ilGen.Emit OpCodes.Ret)

            let reader r = readerDele.Invoke(picklers, r)
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
                    let fields = FSharpType.GetRecordFields(typeof<'Record>, allMembers)
                    let ctor = FSharpValue.PreComputeRecordConstructorInfo(typeof<'Record>, allMembers)
                    fields, ctor

            let picklers = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)

#if EMIT_IL
            let writer =
                if fields.Length = 0 then fun _ _ -> ()
                else
                    let writerDele =
                        DynamicMethod.compileAction3<Pickler [], Writer, 'Record> "recordSerializer" (fun ilGen ->
                            let picklers = EnvItem<Pickler []>.Arg0
                            let writer = EnvItem<Writer>.Arg1
                            let record = EnvItem<'Record>.Arg2

                            emitSerializeProperties fields writer picklers record ilGen)

                    fun w t -> writerDele.Invoke(picklers, w,t)

            let readerDele =
                DynamicMethod.compileFunc2<Pickler [], Reader, 'Record> "recordDeserializer" (fun ilGen ->
                    let picklers = EnvItem<Pickler []>.Arg0
                    let reader = EnvItem<Reader>.Arg1

                    emitDeserializeAndConstruct (Choice2Of2 ctor) (ctor.GetParameterTypes()) reader picklers ilGen

                    ilGen.Emit OpCodes.Ret)
            
            let reader r = readerDele.Invoke(picklers, r)
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
