namespace Nessos.FsPickler

    open System
    open System.IO
    open System.Reflection
    open System.Threading
    open System.Collections.Generic
    open System.Runtime.Serialization

    open Microsoft.FSharp.Reflection

    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection

#if EMIT_IL
    open System.Reflection.Emit
    open Nessos.FsPickler.Emit
    open Nessos.FsPickler.PicklerEmit
#endif

    // F# union types

    type internal FsUnionPickler =

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
                    let ctor = FSharpValue.PreComputeUnionConstructorInfo(uci, allMembers)
                    let picklers = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                    let tags = fields |> Array.map (fun f -> f.NormalizedName)
                    uci.Name, ctor, fields, tags, picklers)

            let picklerss = caseInfo |> Array.map (fun (_,_,_,_,picklers) -> picklers)
            let tagResolver = UnionTagResolver(caseInfo |> Array.map (fun (name,_,_,_,_) -> name))

            let writerDele =
                DynamicMethod.compileAction3<Pickler [] [], WriteState, 'Union> "unionSerializer" (fun picklerss writer union ilGen ->
                    let tag = EnvItem<int>(ilGen)
                    let picklers = EnvItem<Pickler []>(ilGen)

                    let labels = Array.init caseInfo.Length (fun _ -> ilGen.DefineLabel())

                    // read union tag
                    union.Load ()
                    ilGen.EmitCall(OpCodes.Call, tagReaderMethod, null)
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
                        let name,_,fields,tags,_ = caseInfo.[i]

                        ilGen.MarkLabel label
                        writeString writer "Case" name ilGen
                        emitSerializeProperties fields tags writer picklers union ilGen
                        ilGen.Emit OpCodes.Ret
                )

            let readerDele =
                DynamicMethod.compileFunc3<Pickler [] [], UnionTagResolver, ReadState, 'Union> "unionDeserializer" (fun picklerss tagResolver reader ilGen ->

                    let tag = EnvItem<int>(ilGen)
                    let picklers = EnvItem<Pickler []>(ilGen)

                    let labels = Array.init caseInfo.Length (fun _ -> ilGen.DefineLabel())

                    // resolve union case tag
                    tagResolver.Load() // load resolver to stack
                    readString reader "Case" ilGen // read case name from stream
                    UnionTagResolver.InvokeResolver ilGen // call the resolver method
                    tag.Store() // store to local var

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
                        let _,ctor,fields,tags,_ = caseInfo.[i]

                        let ctorParams = fields |> Array.map (fun f -> f.PropertyType)

                        ilGen.MarkLabel label
                        emitDeserializeAndConstruct (Choice1Of2 ctor) ctorParams tags reader picklers ilGen
                        ilGen.Emit OpCodes.Ret
                )

            let writer w tag u = writerDele.Invoke(picklerss, w, u)
            let reader r tag = readerDele.Invoke(picklerss, tagResolver, r)
#else
            let tagReader = Delegate.CreateDelegate<Func<'Union,int>> tagReaderMethod

            let caseInfo =
                FSharpType.GetUnionCases(typeof<'Union>, allMembers) 
                |> Array.map (fun uci ->
                    let ctor = FSharpValue.PreComputeUnionConstructor(uci, allMembers)
                    let reader = FSharpValue.PreComputeUnionReader(uci, allMembers)
                    let fields = uci.GetFields()
                    let picklers = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                    let tags = fields |> Array.map (fun f -> f.NormalizedName)
                    uci.Name,ctor, reader, fields, tags, picklers)

            let tagResolver = UnionTagResolver(caseInfo |> Array.map (fun (name,_,_,_,_,_) -> name))

            let writer (w : WriteState) (tag : string) (u : 'Union) =
                let tag = tagReader.Invoke u
                let name,_,reader,fields,tags,picklers = caseInfo.[tag]
                w.Formatter.WriteString "Case" name
                let values = reader u
                for i = 0 to values.Length - 1 do
                    picklers.[i].UntypedWrite w tags.[i] (values.[i])

            let reader (r : ReadState) (tag : string) =
                let case = r.Formatter.ReadString "Case"
                let tag = tagResolver.ResolveTag case
                let _,ctor,_,fields,tags,picklers = caseInfo.[tag]
                let values = Array.zeroCreate<obj> picklers.Length
                for i = 0 to picklers.Length - 1 do
                    values.[i] <- picklers.[i].UntypedRead r tags.[i]

                ctor values |> fastUnbox<'Union>
#endif
            CompositePickler.Create(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

    and private UnionTagResolver(caseNames : string []) =
        let dict = new Dictionary<string, int>()
        do 
            for i = 0 to caseNames.Length - 1 do
                dict.Add(caseNames.[i], i)

        member __.ResolveTag(case : string) =
            if obj.ReferenceEquals(case, null) then
                raise <| new FormatException("invalid union case 'null'.")
            else
                let ok, tag = dict.TryGetValue case
                if ok then tag
                else
                    raise <| new FormatException(sprintf "invalid union case '%s'." case)

#if EMIT_IL
        static member InvokeResolver (ilGen : ILGenerator) =
            let m = typeof<UnionTagResolver>.GetMethod("ResolveTag", allMembers)
            ilGen.EmitCall(OpCodes.Call, m, null)
#endif

    // F# record types

    type internal FsRecordPickler =
        
        static member Create<'Record>(resolver : IPicklerResolver) =

            let fields = FSharpType.GetRecordFields(typeof<'Record>, allMembers)
            let ctor = FSharpValue.PreComputeRecordConstructorInfo(typeof<'Record>, allMembers)

            let picklers = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)
            let tags = fields |> Array.map (fun f -> f.NormalizedName)

#if EMIT_IL
            let writer =
                if fields.Length = 0 then fun _ _ _ -> ()
                else
                    let writerDele =
                        DynamicMethod.compileAction3<Pickler [], WriteState, 'Record> "recordSerializer" (fun picklers writer record ilGen ->

                            emitSerializeProperties fields tags writer picklers record ilGen
                            
                            ilGen.Emit OpCodes.Ret)

                    fun w tag t -> writerDele.Invoke(picklers, w,t)

            let readerDele =
                DynamicMethod.compileFunc2<Pickler [], ReadState, 'Record> "recordDeserializer" (fun picklers reader ilGen ->

                    let ctorParams = fields |> Array.map (fun f -> f.PropertyType)

                    emitDeserializeAndConstruct (Choice2Of2 ctor) ctorParams tags reader picklers ilGen

                    ilGen.Emit OpCodes.Ret)
            
            let reader r tag = readerDele.Invoke(picklers, r)
#else
            let writer (w : WriteState) (tag : string) (r : 'Record) =
                for i = 0 to fields.Length - 1 do
                    let f = fields.[i]
                    let o = f.GetValue r
                    picklers.[i].UntypedWrite w tags.[i] o
            
            let reader (r : ReadState) (tag : string) =
                let values = Array.zeroCreate<obj> fields.Length
                for i = 0 to fields.Length - 1 do
                    values.[i] <- picklers.[i].UntypedRead r tags.[i]

                ctor.Invoke values |> fastUnbox<'Record>
#endif

            CompositePickler.Create(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = false)


    // F# exception types
    // Exception serialization is broken in F#; while types are ISerializable, added fields will not be serialized properly
    // Use a combination of ISerializable resolution and reflection to derive correct logic
    type internal FsExceptionPickler =
        
        static member Create<'Exception when 'Exception :> exn>(resolver : IPicklerResolver) =
            // the default ISerializable pickler that handles exception metadata serialization
            let defPickler = ISerializablePickler.Create<'Exception>(resolver) :?> CompositePickler<'Exception>
            // separately serialize exception fields
            let fields = gatherFields typeof<'Exception> |> Array.filter(fun f -> f.DeclaringType = typeof<'Exception>)
            let fpicklers = fields |> Array.map (fun f -> resolver.Resolve f.FieldType)
            // extract tag names from properties, not fields
            let tags = FSharpType.GetExceptionFields(typeof<'Exception>, allMembers) |> Array.map (fun p -> p.Name)

#if EMIT_IL
            let writerDele = 
                if fields.Length = 0 then None
                else
                    DynamicMethod.compileAction3<Pickler [], WriteState, 'Exception> "exceptionSerializer" (fun picklers writer value ilGen ->

                        emitSerializeFields fields tags writer picklers value ilGen

                        ilGen.Emit OpCodes.Ret
                    ) |> Some

            let readerDele =
                if fields.Length = 0 then None
                else
                    DynamicMethod.compileAction3<Pickler [], ReadState, 'Exception> "exceptionDeserializer" (fun picklers reader value ilGen ->

                        emitDeserializeFields fields tags reader picklers value ilGen

                        ilGen.Emit OpCodes.Ret
                    ) |> Some


            let writer (w : WriteState) (tag : string) (e : 'Exception) =
                do defPickler.Writer w tag e
 
                match writerDele with
                | None -> ()
                | Some d -> d.Invoke(fpicklers, w, e)

            let reader (r : ReadState) (tag : string) =
                let e = defPickler.Reader r tag

                match readerDele with
                | None -> e
                | Some d -> d.Invoke(fpicklers, r, e) ; e
#else
            let writer (w : WriteState) (tag : string) (e : 'Exception) =
                do defPickler.Writer w tag e

                for i = 0 to fields.Length - 1 do
                    let f = fields.[i]
                    let o = f.GetValue e
                    fpicklers.[i].UntypedWrite w tags.[i] o

            let reader (r : ReadState) (tag : string) =
                let e = defPickler.Reader r tag

                for i = 0 to fields.Length - 1 do
                    let f = fields.[i]
                    let o = fpicklers.[i].UntypedRead r tags.[i]
                    f.SetValue(e, o)
                e
#endif
            CompositePickler.Create(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = true)
