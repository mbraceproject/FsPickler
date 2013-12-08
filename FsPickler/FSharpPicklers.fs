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

#if EMIT_IL
            let caseInfo =
                FSharpType.GetUnionCases(typeof<'Union>, allMembers) 
                |> Array.sortBy (fun uci -> uci.Tag)
                |> Array.map (fun uci ->
                    let fields = uci.GetFields()
                    let caseType = 
                        if fields.Length = 0 then None
                        else
                            match fields.[0].DeclaringType with
                            | dt when dt <> uci.DeclaringType -> Some dt
                            | _ -> None
                    let ctor = FSharpValue.PreComputeUnionConstructorInfo(uci, allMembers)
                    let picklers = uci.GetFields() |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                    caseType, ctor, fields, picklers)

            let picklerss = caseInfo |> Array.map (fun (_,_,_,picklers) -> picklers)

            let writerDele =
                DynamicMethod.compileAction3<Pickler [] [], Writer, 'Union> "unionSerializer" (fun ilGen ->
                    let picklerss = EnvItem<Pickler [] []>.Arg0
                    let writer = EnvItem<Writer>.Arg1
                    let union = EnvItem<'Union>.Arg2
                    let tag = EnvItem<int>.InitVar ilGen
                    let picklers = EnvItem<Pickler []>.InitVar ilGen

                    let labels = Array.init caseInfo.Length (fun _ -> ilGen.DefineLabel())

                    // read union tag & store to local value
                    union.Load ilGen
                    ilGen.EmitCall(OpCodes.Call, tagReaderMethod, null)
                    tag.Store ilGen

                    // select appropriate picklers & store
                    picklerss.Load ilGen
                    tag.Load ilGen
                    ilGen.Emit OpCodes.Ldelem_Ref
                    picklers.Store ilGen

                    // write tag to stream
                    writeInt writer tag ilGen

                    // make jump table
                    tag.Load ilGen
                    ilGen.Emit(OpCodes.Switch, labels)

                    // emit cases
                    for i = 0 to caseInfo.Length - 1 do
                        let label = labels.[i]
                        let caseType,_,fields,_ = caseInfo.[i]

                        ilGen.MarkLabel label
                        emitSerializeProperties fields writer picklers union ilGen
                        ilGen.Emit OpCodes.Ret
                )

            let readerDele =
                DynamicMethod.compileFunc2<Pickler [] [], Reader, 'Union> "unionDeserializer" (fun ilGen ->
                    let picklerss = EnvItem<Pickler [] []>.Arg0
                    let reader = EnvItem<Reader>.Arg1
                    let tag = EnvItem<int>.InitVar ilGen
                    let picklers = EnvItem<Pickler []>.InitVar ilGen

                    let labels = Array.init caseInfo.Length (fun _ -> ilGen.DefineLabel())

                    // read tag from stream & store
                    readInt reader ilGen
                    tag.Store ilGen

                    // select appropriate picklers & store
                    picklerss.Load ilGen
                    tag.Load ilGen
                    ilGen.Emit OpCodes.Ldelem_Ref
                    picklers.Store ilGen

                    // make jump table
                    tag.Load ilGen
                    ilGen.Emit(OpCodes.Switch, labels)

                    // emit cases
                    for i = 0 to caseInfo.Length - 1 do
                        let label = labels.[i]
                        let _,ctor,_,_ = caseInfo.[i]

                        ilGen.MarkLabel label
                        emitDeserializeAndConstruct (Choice1Of2 ctor) (ctor.GetParameterTypes()) reader picklers ilGen
                        ilGen.Emit OpCodes.Ret
                )

            let writer w u = writerDele.Invoke(picklerss, w, u)
            let reader r = readerDele.Invoke(picklerss, r)
#else
            let tagReader = Delegate.CreateDelegate<Func<'Union,int>> tagReaderMethod

            let caseInfo =
                FSharpType.GetUnionCases(typeof<'Union>, allMembers) 
                |> Array.map (fun uci ->
                    let ctor = FSharpValue.PreComputeUnionConstructor(uci, allMembers)
                    let reader = FSharpValue.PreComputeUnionReader(uci, allMembers)
                    let picklers = uci.GetFields() |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                    ctor, reader, picklers)

            let writer (w : Writer) (x : 'Union) =
                let tag = tagReader.Invoke x
                w.BinaryWriter.Write (byte tag)
                let _,reader,picklers = caseInfo.[tag]
                let values = reader x
                for i = 0 to values.Length - 1 do
                    picklers.[i].ManagedWrite w values.[i]

            let reader (r : Reader) =
                let tag = int (r.BinaryReader.ReadByte())
                let ctor,_,picklers = caseInfo.[tag]
                let values = Array.zeroCreate<obj> picklers.Length
                for i = 0 to picklers.Length - 1 do
                    values.[i] <- picklers.[i].ManagedRead r

                ctor values |> fastUnbox<'Union>
#endif
            new Pickler<'Union>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

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

                            emitSerializeProperties fields writer picklers record ilGen
                            
                            ilGen.Emit OpCodes.Ret)

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
