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
                    picklers.[i].UntypedWrite(w, values.[i], managed = true)

            let reader (r : Reader) =
                let tag = int (r.BinaryReader.ReadByte())
                let ctor,_,picklers = caseInfo.[tag]
                let values = Array.zeroCreate<obj> picklers.Length
                for i = 0 to picklers.Length - 1 do
                    values.[i] <- picklers.[i].UntypedRead(r, managed = true)

                ctor values |> fastUnbox<'Union>
#endif
            new Pickler<'Union>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

    // F# record types

    type FsRecordPickler =

        static member CreateUntyped(t : Type, resolver : IPicklerResolver) =
            let m =
                typeof<FsRecordPickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Pickler
        
        static member Create<'Record>(resolver : IPicklerResolver) =

            let fields = FSharpType.GetRecordFields(typeof<'Record>, allMembers)
            let ctor = FSharpValue.PreComputeRecordConstructorInfo(typeof<'Record>, allMembers)

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
                    picklers.[i].UntypedWrite(w, o, managed = true)
            
            let reader (r : Reader) =
                let values = Array.zeroCreate<obj> fields.Length
                for i = 0 to fields.Length - 1 do
                    values.[i] <- picklers.[i].UntypedRead(r, managed = true)

                ctor.Invoke values |> fastUnbox<'Record>
#endif

            new Pickler<'Record>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = false)


    // F# exception types
    // Exception serialization is broken in F#; while types are ISerializable, added fields will not be serialized properly
    // Use a combination of ISerializable resolution and reflection to derive correct logic
    type FsExceptionPickler =

        static member CreateUntyped(t : Type, resolver : IPicklerResolver) =
            let m =
                typeof<FsExceptionPickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Pickler
        
        static member Create<'Exception when 'Exception :> exn>(resolver : IPicklerResolver) =
            // the default ISerializable pickler that handles exception metadata serialization
            let defPickler = DotNetPicklers.ISerializablePickler.Create<'Exception>(resolver)
            // separately serialize exception fields
            let fields = gatherFields typeof<'Exception> |> Array.filter(fun f -> f.DeclaringType = typeof<'Exception>)
            let fpicklers = fields |> Array.map (fun f -> resolver.Resolve f.FieldType)

#if EMIT_IL
            let writerDele = 
                if fields.Length = 0 then None
                else
                    DynamicMethod.compileAction3<Pickler [], Writer, 'Exception> "exceptionSerializer" (fun ilGen ->
                        let picklers = EnvItem<Pickler []>.Arg0
                        let writer = EnvItem<Writer>.Arg1
                        let value = EnvItem<'Exception>.Arg2

                        emitSerializeFields fields writer picklers value ilGen

                        ilGen.Emit OpCodes.Ret
                    ) |> Some

            let readerDele =
                if fields.Length = 0 then None
                else
                    DynamicMethod.compileAction3<Pickler [], Reader, 'Exception> "exceptionDeserializer" (fun ilGen ->
                        let picklers = EnvItem<Pickler []>.Arg0
                        let reader = EnvItem<Reader>.Arg1
                        let value = EnvItem<'Exception>.Arg2
                        
                        emitDeserializeFields fields reader picklers value ilGen

                        ilGen.Emit OpCodes.Ret
                    ) |> Some


            let writer (w : Writer) (e : 'Exception) =
                defPickler.Write w e
                match writerDele with
                | None -> ()
                | Some d -> d.Invoke(fpicklers, w, e)

            let reader (r : Reader) =
                let e = defPickler.Read r
                match readerDele with
                | None -> e
                | Some d -> d.Invoke(fpicklers, r, e) ; e
#else
            let writer (w : Writer) (e : 'Exception) =
                defPickler.Write w e
                for i = 0 to fields.Length - 1 do
                    let o = fields.[i].GetValue e
                    fpicklers.[i].UntypedWrite(w, o, managed = true)

            let reader (r : Reader) =
                let e = defPickler.Read r
                for i = 0 to fields.Length - 1 do
                    let o = fpicklers.[i].UntypedRead(r, managed = true)
                    fields.[i].SetValue(e, o)
                e
#endif
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = true)