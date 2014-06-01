module internal Nessos.FsPickler.FSharpPicklers

    open System
    open System.IO
    open System.Reflection
    open System.Threading
    open System.Runtime.Serialization

    open Microsoft.FSharp.Reflection

    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection
    open Nessos.FsPickler.PicklerUtils

#if EMIT_IL
    open System.Reflection.Emit
    open Nessos.FsPickler.Emit
    open Nessos.FsPickler.PicklerEmit
#endif

    // F# union types

    type FsUnionPickler =

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
                    let picklers = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                    caseType, ctor, fields, picklers)

            let picklerss = caseInfo |> Array.map (fun (_,_,_,picklers) -> picklers)

            let writerDele =
                DynamicMethod.compileAction3<Pickler [] [], WriteState, 'Union> "unionSerializer" (fun picklerss writer union ilGen ->
                    let tag = EnvItem<int>(ilGen)
                    let picklers = EnvItem<Pickler []>(ilGen)

                    let labels = Array.init caseInfo.Length (fun _ -> ilGen.DefineLabel())

                    // read union tag & store to local value
                    union.Load ()
                    ilGen.EmitCall(OpCodes.Call, tagReaderMethod, null)
                    tag.Store ()

                    // select appropriate picklers & store
                    picklerss.Load ()
                    tag.Load ()
                    ilGen.Emit OpCodes.Ldelem_Ref
                    picklers.Store ()

                    // write tag to stream
                    writeInt writer "case" tag ilGen

                    // make jump table
                    tag.Load ()
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
                DynamicMethod.compileFunc2<Pickler [] [], ReadState, 'Union> "unionDeserializer" (fun picklerss reader ilGen ->

                    let tag = EnvItem<int>(ilGen)
                    let picklers = EnvItem<Pickler []>(ilGen)

                    let labels = Array.init caseInfo.Length (fun _ -> ilGen.DefineLabel())

                    // read tag from stream & store
                    readInt reader "case" ilGen
                    tag.Store ()

                    // select appropriate picklers & store
                    picklerss.Load ()
                    tag.Load ()
                    ilGen.Emit OpCodes.Ldelem_Ref
                    picklers.Store ()

                    // make jump table
                    tag.Load ()
                    ilGen.Emit(OpCodes.Switch, labels)

                    // emit cases
                    for i = 0 to caseInfo.Length - 1 do
                        let label = labels.[i]
                        let _,ctor,fields,_ = caseInfo.[i]

                        let ctorParams = fields |> Array.map (fun f -> f.PropertyType, getTagFromMemberInfo f)

                        ilGen.MarkLabel label
                        emitDeserializeAndConstruct (Choice1Of2 ctor) ctorParams reader picklers ilGen
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
                    let fields = uci.GetFields()
                    let picklers = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                    ctor, reader, fields, picklers)

            let writer (w : WriteState) (x : 'Union) =
                let tag = tagReader.Invoke x
                w.Formatter.WriteByte "case" (byte tag)
                let _,reader,fields,picklers = caseInfo.[tag]
                let values = reader x
                for i = 0 to values.Length - 1 do
                    picklers.[i].UntypedWrite w (getTagFromMemberInfo fields.[i]) (values.[i])

            let reader (r : ReadState) =
                let tag = int (r.Formatter.ReadByte "case")
                let ctor,_,fields,picklers = caseInfo.[tag]
                let values = Array.zeroCreate<obj> picklers.Length
                for i = 0 to picklers.Length - 1 do
                    values.[i] <- picklers.[i].UntypedRead r (getTagFromMemberInfo fields.[i])

                ctor values |> fastUnbox<'Union>
#endif
            CompositePickler.Create(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

    // F# record types

    type FsRecordPickler =
        
        static member Create<'Record>(resolver : IPicklerResolver) =

            let fields = FSharpType.GetRecordFields(typeof<'Record>, allMembers)
            let ctor = FSharpValue.PreComputeRecordConstructorInfo(typeof<'Record>, allMembers)

            let picklers = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)

#if EMIT_IL
            let writer =
                if fields.Length = 0 then fun _ _ -> ()
                else
                    let writerDele =
                        DynamicMethod.compileAction3<Pickler [], WriteState, 'Record> "recordSerializer" (fun picklers writer record ilGen ->

                            emitSerializeProperties fields writer picklers record ilGen
                            
                            ilGen.Emit OpCodes.Ret)

                    fun w t -> writerDele.Invoke(picklers, w,t)

            let readerDele =
                DynamicMethod.compileFunc2<Pickler [], ReadState, 'Record> "recordDeserializer" (fun picklers reader ilGen ->

                    let ctorParams = fields |> Array.map (fun f -> f.PropertyType, getTagFromMemberInfo f)

                    emitDeserializeAndConstruct (Choice2Of2 ctor) ctorParams reader picklers ilGen

                    ilGen.Emit OpCodes.Ret)
            
            let reader r = readerDele.Invoke(picklers, r)
#else
            let writer (w : WriteState) (x : 'Record) =
                for i = 0 to fields.Length - 1 do
                    let f = fields.[i]
                    let o = f.GetValue x
                    picklers.[i].UntypedWrite w (getTagFromMemberInfo f) o
            
            let reader (r : ReadState) =
                let values = Array.zeroCreate<obj> fields.Length
                for i = 0 to fields.Length - 1 do
                    values.[i] <- picklers.[i].UntypedRead r (getTagFromMemberInfo fields.[i])

                ctor.Invoke values |> fastUnbox<'Record>
#endif

            CompositePickler.Create(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = false)


    // F# exception types
    // Exception serialization is broken in F#; while types are ISerializable, added fields will not be serialized properly
    // Use a combination of ISerializable resolution and reflection to derive correct logic
    type FsExceptionPickler =
        
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
                    DynamicMethod.compileAction3<Pickler [], WriteState, 'Exception> "exceptionSerializer" (fun picklers writer value ilGen ->

                        emitSerializeFields fields writer picklers value ilGen

                        ilGen.Emit OpCodes.Ret
                    ) |> Some

            let readerDele =
                if fields.Length = 0 then None
                else
                    DynamicMethod.compileAction3<Pickler [], ReadState, 'Exception> "exceptionDeserializer" (fun picklers reader value ilGen ->

                        emitDeserializeFields fields reader picklers value ilGen

                        ilGen.Emit OpCodes.Ret
                    ) |> Some


            let writer (w : WriteState) (e : 'Exception) =
                defPickler.Write w "exceptionBase" e
                match writerDele with
                | None -> ()
                | Some d -> d.Invoke(fpicklers, w, e)

            let reader (r : ReadState) =
                let e = defPickler.Read r "exceptionBase"
                match readerDele with
                | None -> e
                | Some d -> d.Invoke(fpicklers, r, e) ; e
#else
            let writer (w : WriteState) (e : 'Exception) =
                defPickler.Write w "exceptionBase" e
                for i = 0 to fields.Length - 1 do
                    let f = fields.[i]
                    let o = f.GetValue e
                    fpicklers.[i].UntypedWrite w (getTagFromMemberInfo f) o

            let reader (r : ReadState) =
                let e = defPickler.Read r "exceptionBase"
                for i = 0 to fields.Length - 1 do
                    let f = fields.[i]
                    let o = fpicklers.[i].UntypedRead r (getTagFromMemberInfo f)
                    f.SetValue(e, o)
                e
#endif
            CompositePickler.Create(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = true)
