namespace MBrace.FsPickler

open System
open System.IO
open System.Reflection
open System.Threading
open System.Collections.Generic
open System.Runtime.Serialization

open Microsoft.FSharp.Reflection

open MBrace.FsPickler
open MBrace.FsPickler.Reflection

#if EMIT_IL
open System.Reflection.Emit
open MBrace.FsPickler.Emit
open MBrace.FsPickler.PicklerEmit
#endif

// F# union types

type internal FsUnionPickler =

    static member Create<'Union> (registry : ICustomPicklerRegistry) (resolver : IPicklerResolver) =
        let ty = typeof<'Union>
        if not (isReflectionSerializable ty || registry.IsDeclaredSerializable ty) then
            raise <| new NonSerializableTypeException(ty)

        let isStructUnion = typeof<'Union>.IsValueType

        // Only cache by reference if typedef introduces custom or reference equality semantics
        let isCacheByRef =
            if isStructUnion then false 
            else
                containsAttr<CustomEqualityAttribute> ty 
                || containsAttr<ReferenceEqualityAttribute> ty

        let ucis = FSharpType.GetUnionCases(ty, allMembers)
        let tagSerializer = new UnionCaseSerializationHelper(ucis |> Array.map (fun u -> u.Name))

#if EMIT_IL
        // resolve tag reader methodInfo
        let tagReaderMethod =
            match FSharpValue.PreComputeUnionTagMemberInfo(ty, allMembers) with
            | null -> invalidOp "unexpected error"
            | :? PropertyInfo as p -> p.GetGetMethod(true)
            | :? MethodInfo as m -> m
            | _ -> invalidOp "unexpected error"

        let caseInfo =
            ucis
            |> Array.sortBy (fun uci -> uci.Tag)
            |> Array.map (fun uci ->
                let fields = uci.GetFields()
                let ctor = FSharpValue.PreComputeUnionConstructorInfo(uci, allMembers)
                let picklers = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                let tags = fields |> Array.mapi (fun i f -> getNormalizedFieldName i f.Name)
                ctor, fields, tags, picklers)

        let picklerss = caseInfo |> Array.map (fun (_,_,_,picklers) -> picklers)

        let writerDele =
            DynamicMethod.compileAction4<Pickler [] [], UnionCaseSerializationHelper, WriteState, 'Union> "unionSerializer" (fun picklerss cs writer union ilGen ->
                let tag = EnvItem<int>(ilGen)
                let picklers = EnvItem<Pickler []>(ilGen)

                let labels = Array.init caseInfo.Length (fun _ -> ilGen.DefineLabel())

                // read union tag
                if isStructUnion then union.LoadAddress() else union.Load()
                ilGen.EmitCall(OpCodes.Call, tagReaderMethod, null)
                tag.Store ()

                // select appropriate picklers & store
                picklerss.Load ()
                tag.Load ()
                ilGen.Emit OpCodes.Ldelem_Ref
                picklers.Store ()

                // serialize union case tag
                UnionCaseSerializationHelper.InvokeTagWriter cs writer tag ilGen

                // make jump table
                tag.Load ()
                ilGen.Emit(OpCodes.Switch, labels)

                // emit cases
                for i = 0 to caseInfo.Length - 1 do
                    let label = labels.[i]
                    let _,fields,tags,_ = caseInfo.[i]

                    ilGen.MarkLabel label
                    emitSerializeMembers fields tags writer picklers union ilGen
                    ilGen.Emit OpCodes.Ret
            )

        let readerDele =
            DynamicMethod.compileFunc3<Pickler [] [], UnionCaseSerializationHelper, ReadState, 'Union> "unionDeserializer" (fun picklerss cs reader ilGen ->

                let tag = EnvItem<int>(ilGen)
                let picklers = EnvItem<Pickler []>(ilGen)

                let labels = Array.init caseInfo.Length (fun _ -> ilGen.DefineLabel())

                // resolve union case tag
                UnionCaseSerializationHelper.InvokeTagReader cs reader ilGen
                tag.Store()

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
                    let ctor,fields,tags,_ = caseInfo.[i]

                    let ctorParams = fields |> Array.map (fun f -> f.PropertyType)

                    ilGen.MarkLabel label
                    emitDeserializeAndConstruct (Choice1Of2 ctor) ctorParams tags reader picklers ilGen
                    ilGen.Emit OpCodes.Ret
            )

        let clonerDele =
            DynamicMethod.compileFunc3<Pickler [] [], CloneState, 'Union, 'Union> "unionCloner" (fun picklerss state source ilGen ->
                let tag = EnvItem<int>(ilGen)
                let picklers = EnvItem<Pickler []>(ilGen)

                let labels = Array.init caseInfo.Length (fun _ -> ilGen.DefineLabel())

                // read union tag
                if isStructUnion then source.LoadAddress() else source.Load ()
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
                    let ctor,fields,_,_ = caseInfo.[i]

                    ilGen.MarkLabel label
                    emitCloneAndConstruct (Choice1Of2 ctor) fields state picklers source ilGen
                    ilGen.Emit OpCodes.Ret
            )

        let accepterDele =
            DynamicMethod.compileAction3<Pickler [] [], VisitState, 'Union> "unionAccepter" (fun picklerss state union ilGen ->
                let tag = EnvItem<int>(ilGen)
                let picklers = EnvItem<Pickler []>(ilGen)

                let labels = Array.init caseInfo.Length (fun _ -> ilGen.DefineLabel())

                // read union tag
                if isStructUnion then union.LoadAddress() else union.Load ()
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
                    let _,fields,_,_ = caseInfo.[i]

                    ilGen.MarkLabel label
                    emitAcceptMembers fields state picklers union ilGen
                    ilGen.Emit OpCodes.Ret
            )

        let writer w _ u = writerDele.Invoke(picklerss, tagSerializer, w, u)
        let reader r _ = readerDele.Invoke(picklerss, tagSerializer, r)
        let cloner c t = clonerDele.Invoke(picklerss, c, t)
        let accepter v t = accepterDele.Invoke(picklerss, v, t)
#else
        let tagReader = FSharpValue.PreComputeUnionTagReader(ty, allMembers)

        let caseInfo =
            ucis
            |> Array.map (fun uci ->
                let ctor = FSharpValue.PreComputeUnionConstructor(uci, allMembers)
                let reader = FSharpValue.PreComputeUnionReader(uci, allMembers)
                let fields = uci.GetFields()
                let picklers = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                let tags = fields |> Array.mapi (fun i f -> getNormalizedFieldName i f.Name)
                ctor, reader, fields, tags, picklers)

        let writer (w : WriteState) (_ : string) (u : 'Union) =
            let tag = tagReader u
            tagSerializer.WriteTag(w.Formatter, tag)
            let _,reader,_,tags,picklers = caseInfo.[tag]
            let values = reader u
            for i = 0 to values.Length - 1 do
                picklers.[i].UntypedWrite w tags.[i] (values.[i])

        let reader (r : ReadState) (_ : string) =
            let tag = tagSerializer.ReadTag r.Formatter
            let ctor,_,_,tags,picklers = caseInfo.[tag]
            let values = Array.zeroCreate<obj> picklers.Length
            for i = 0 to picklers.Length - 1 do
                values.[i] <- picklers.[i].UntypedRead r tags.[i]

            ctor values |> fastUnbox<'Union>

        let cloner (c : CloneState) (u : 'Union) =
            let tag = tagReader u
            let ctor,reader,_,_,picklers = caseInfo.[tag]
            let values = reader u
            let values' = Array.zeroCreate<obj> values.Length
            for i = 0 to picklers.Length - 1 do
                values'.[i] <- picklers.[i].UntypedClone c values.[i]

            ctor values' |> fastUnbox<'Union>

        let accepter (v : VisitState) (u : 'Union) =
            let tag = tagReader u
            let _,reader,_,_,picklers = caseInfo.[tag]
            let values = reader u
            for i = 0 to picklers.Length - 1 do
                picklers.[i].UntypedAccept v values.[i]
            
#endif
        CompositePickler.Create(reader, writer, cloner, accepter, PicklerInfo.FSharpValue, cacheByRef = isCacheByRef, useWithSubtypes = true)

// F# record types

type internal FsRecordPickler =
        
    static member Create<'Record> (registry : ICustomPicklerRegistry) (resolver : IPicklerResolver) =
        let ty = typeof<'Record>
        if not (isReflectionSerializable ty || registry.IsDeclaredSerializable ty) then
            raise <| new NonSerializableTypeException(ty)

        let isStructRecord = ty.IsValueType
        let fields = FSharpType.GetRecordFields(ty, allMembers)
        let ctor = FSharpValue.PreComputeRecordConstructorInfo(ty, allMembers)

        let picklers = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)
        let tags = fields |> Array.mapi (fun i f -> getNormalizedFieldName i f.Name)

        // Only cache by reference if typedef introduces custom or reference equality semantics
        let isCacheByRef = 
            if isStructRecord then false 
            else
                containsAttr<CustomEqualityAttribute> ty ||
                containsAttr<ReferenceEqualityAttribute> ty

#if EMIT_IL

        let writer =
            if fields.Length = 0 then fun _ _ _ -> ()
            else
                let writerDele =
                    DynamicMethod.compileAction3<Pickler [], WriteState, 'Record> "recordSerializer" (fun picklers writer record ilGen ->

                        emitSerializeMembers fields tags writer picklers record ilGen
                            
                        ilGen.Emit OpCodes.Ret)

                fun w _ t -> writerDele.Invoke(picklers, w,t)

        let readerDele =
            DynamicMethod.compileFunc2<Pickler [], ReadState, 'Record> "recordDeserializer" (fun picklers reader ilGen ->

                let ctorParams = fields |> Array.map (fun f -> f.PropertyType)

                emitDeserializeAndConstruct (Choice2Of2 ctor) ctorParams tags reader picklers ilGen

                ilGen.Emit OpCodes.Ret)

        let clonerDele =
            DynamicMethod.compileFunc3<Pickler [], CloneState, 'Record, 'Record> "recordCloner" (fun picklers state source ilGen ->

                emitCloneAndConstruct (Choice2Of2 ctor) fields state picklers source ilGen

                ilGen.Emit OpCodes.Ret)

        let accepter =
            if fields.Length = 0 then ignore2
            else
                let accepterDele =
                    DynamicMethod.compileAction3<Pickler [], VisitState, 'Record> "recordAccepter" (fun picklers state record ilGen ->
                        emitAcceptMembers fields state picklers record ilGen
                            
                        ilGen.Emit OpCodes.Ret)

                fun v t -> accepterDele.Invoke(picklers, v, t)
            
        let reader r _ = readerDele.Invoke(picklers, r)
        let cloner c t = clonerDele.Invoke(picklers, c, t)
#else
        let writer (w : WriteState) (_ : string) (r : 'Record) =
            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = f.GetValue r
                picklers.[i].UntypedWrite w tags.[i] o
            
        let reader (r : ReadState) (_ : string) =
            let values = Array.zeroCreate<obj> fields.Length
            for i = 0 to fields.Length - 1 do
                values.[i] <- picklers.[i].UntypedRead r tags.[i]

            ctor.Invoke values |> fastUnbox<'Record>

        let cloner (c : CloneState) (r : 'Record) =
            let values = Array.zeroCreate<obj> picklers.Length
            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = f.GetValue r
                values.[i] <- picklers.[i].UntypedClone c o

            ctor.Invoke values |> fastUnbox<'Record>

        let accepter (v : VisitState) (r : 'Record) =
            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = f.GetValue r
                picklers.[i].UntypedAccept v o
                
#endif

        CompositePickler.Create(reader, writer, cloner, accepter, PicklerInfo.FSharpValue, cacheByRef = isCacheByRef)


// F# exception types
// Exception serialization is broken in F#; while types are ISerializable, added fields will not be serialized properly
// Use a combination of ISerializable resolution and reflection to derive correct logic
type internal FsExceptionPickler =
        
    static member Create<'Exception when 'Exception :> exn> (registry : ICustomPicklerRegistry) (resolver : IPicklerResolver) =
        let ty = typeof<'Exception>
        if not (isReflectionSerializable ty || registry.IsDeclaredSerializable ty) then
            raise <| new NonSerializableTypeException(ty)

        // the default ISerializable pickler that handles exception metadata serialization
        let defPickler = ISerializablePickler.Create<'Exception>() :?> CompositePickler<'Exception>
        // separately serialize exception fields
        let fields = gatherSerializedFields ty |> Array.filter(fun f -> f.DeclaringType = ty)
        let fpicklers = fields |> Array.map (fun f -> resolver.Resolve f.FieldType)
        // extract tag names from properties, not fields
        let tags = FSharpType.GetExceptionFields(ty, allMembers) |> Array.map (fun p -> p.Name)

#if EMIT_IL
        let writerDele = 
            if fields.Length = 0 then None
            else
                DynamicMethod.compileAction3<Pickler [], WriteState, 'Exception> "exceptionSerializer" (fun picklers writer value ilGen ->

                    emitSerializeMembers fields tags writer picklers value ilGen

                    ilGen.Emit OpCodes.Ret
                ) |> Some

        let readerDele =
            if fields.Length = 0 then None
            else
                DynamicMethod.compileAction3<Pickler [], ReadState, 'Exception> "exceptionDeserializer" (fun picklers reader value ilGen ->

                    emitDeserializeMembers fields tags reader picklers value ilGen

                    ilGen.Emit OpCodes.Ret
                ) |> Some

        let clonerDele =
            if fields.Length = 0 then None
            else
                DynamicMethod.compileAction4<Pickler [], CloneState, 'Exception, 'Exception> "exceptionCloner" (fun picklers state source target ilGen ->
                    emitCloneMembers fields state picklers source target ilGen

                    ilGen.Emit OpCodes.Ret
                
                ) |> Some

        let accepterDele =
            if fields.Length = 0 then None
            else
                DynamicMethod.compileAction3<Pickler [], VisitState, 'Exception> "exceptionAccepter" (fun picklers state source ilGen ->
                    emitAcceptMembers fields state picklers source ilGen
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

        let cloner (c : CloneState) (e : 'Exception) =
            let e' = defPickler.Cloner c e
            match clonerDele with
            | None -> e'
            | Some d -> d.Invoke(fpicklers, c, e, e') ; e'

        let accepter (v : VisitState) (e : 'Exception) =
            defPickler.Accepter v e
            match accepterDele with
            | None -> ()
            | Some d -> d.Invoke(fpicklers, v, e)

#else
        let writer (w : WriteState) (_ : string) (e : 'Exception) =
            do defPickler.Writer w "exceptionData" e

            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = f.GetValue e
                fpicklers.[i].UntypedWrite w tags.[i] o

        let reader (r : ReadState) (_ : string) =
            let e = defPickler.Reader r "exceptionData"

            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = fpicklers.[i].UntypedRead r tags.[i]
                f.SetValue(e, o)
            e

        let cloner (c : CloneState) (e : 'Exception) =
            let e' = defPickler.Cloner c e

            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = f.GetValue e
                let o' = fpicklers.[i].UntypedClone c o
                f.SetValue(e', o')

            e'

        let accepter (v : VisitState) (e : 'Exception) =
            defPickler.Accept v e

            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = f.GetValue e
                fpicklers.[i].UntypedAccept v o

#endif
        CompositePickler.Create(reader, writer, cloner, accepter, PicklerInfo.FSharpValue, useWithSubtypes = true)