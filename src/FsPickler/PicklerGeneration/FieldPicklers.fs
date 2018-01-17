namespace MBrace.FsPickler

open System
open System.IO
open System.Reflection
open System.Threading
open System.Runtime.Serialization

open MBrace.FsPickler
open MBrace.FsPickler.Reflection

#if EMIT_IL
open System.Reflection.Emit
open MBrace.FsPickler.Emit
open MBrace.FsPickler.PicklerEmit
#endif

// pickler combinator for struct types

type internal StructFieldPickler =

    static member Create<'T when 'T : struct>(resolver : IPicklerResolver) =
        let t = typeof<'T>
        let fields = gatherSerializedFields t
        let picklers = fields |> Array.map (fun f -> resolver.Resolve f.FieldType)
        let tags = fields |> Array.mapi (fun i f -> getNormalizedFieldName i f.Name)

#if EMIT_IL
        let writerDele =
            DynamicMethod.compileAction3<Pickler [], WriteState, 'T> "structSerializer" (fun picklers writer parent ilGen ->

                emitSerializeMembers fields tags writer picklers parent ilGen

                ilGen.Emit OpCodes.Ret
            )

        let readerDele =
            DynamicMethod.compileFunc2<Pickler [], ReadState, 'T> "structDeserializer" (fun picklers reader ilGen ->
                // initialize empty value type
                let value = EnvItem<'T>(ilGen)
                emitObjectInitializer value ilGen

                emitDeserializeMembers fields tags reader picklers value ilGen

                value.Load ()
                ilGen.Emit OpCodes.Ret
            )

        let clonerDele =
            DynamicMethod.compileFunc3<Pickler [], CloneState, 'T, 'T> "structCloner" (fun picklers state value ilGen ->
                // initialize empty value type
                let value' = EnvItem<'T>(ilGen)
                emitObjectInitializer value' ilGen
            
                emitCloneMembers fields state picklers value value' ilGen

                value'.Load()
                ilGen.Emit OpCodes.Ret
            )

        let accepter =
            if fields.Length = 0 then ignore2
            else
                let accepterDele =
                    DynamicMethod.compileAction3<Pickler [], VisitState, 'T> "structAccepter" (fun picklers state value ilGen ->
                        emitAcceptMembers fields state picklers value ilGen
                        ilGen.Emit OpCodes.Ret
                    )

                fun v t -> accepterDele.Invoke(picklers, v, t)

        let writer (w : WriteState) (_ : string) (t : 'T) = writerDele.Invoke(picklers, w, t)
        let reader (r : ReadState) (_ : string) = readerDele.Invoke(picklers, r)
        let cloner (c : CloneState) (t : 'T) = clonerDele.Invoke(picklers, c, t)

#else
        let writer (w : WriteState) (_ : string) (t : 'T) =
            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = f.GetValue(t)
                picklers.[i].UntypedWrite w tags.[i] o

        let reader (r : ReadState) (_ : string) =
            let t = box Unchecked.defaultof<'T>
            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = picklers.[i].UntypedRead r tags.[i]
                f.SetValue(t, o)
                
            fastUnbox<'T> t

        let cloner (c : CloneState) (t : 'T) =
            let t' = box Unchecked.defaultof<'T>
            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = f.GetValue t
                let o' = picklers.[i].UntypedClone c o
                f.SetValue(t', o')

            fastUnbox<'T> t'

        let accepter (v : VisitState) (t : 'T) =
            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = f.GetValue t
                picklers.[i].UntypedAccept v o
#endif

        CompositePickler.Create(reader, writer, cloner, accepter, PicklerInfo.FieldSerialization)

// general-purpose pickler combinator for reference types

type internal ClassFieldPickler =

    static member Create<'T when 'T : not struct> (picklerRegistry : ICustomPicklerRegistry) (resolver : IPicklerResolver) =
        let ty = typeof<'T>
        // ExceptionDispatchInfo serialization not supported in mono.
        let isEDI = not runsOnMono && isExceptionDispatchInfo ty
        // Exception field-based serialization.
        let isException = isAssignableFrom typeof<Exception> ty
        // we need to be capable of serializing Roslyn submission types
        let isRoslynReplSubmissionType = isRoslynReplSubmissionType ty

        let isSerializable = 
            isReflectionSerializable ty
            || isRoslynReplSubmissionType
            || isLinqEnumerable ty
            || isException
            || isEDI
            || picklerRegistry.IsDeclaredSerializable ty
            // compiler generated types in C# are not marked as serializable, but should in principle be treated as such.
            || containsAttr<System.Runtime.CompilerServices.CompilerGeneratedAttribute> ty
            // certain types in Microsoft's CRM SDK contain the CollectionDataContractAttribute but do not
            // supply a DataContract for its data. Support field-based serialization as a last-ditch effort
            // to support those types.
            || containsAttr<System.Runtime.Serialization.CollectionDataContractAttribute> ty
            || containsAttr<System.Runtime.Serialization.KnownTypeAttribute> ty

        if not isSerializable then raise <| new NonSerializableTypeException(ty)

        let fields = 
            gatherSerializedFields ty
            |> Array.filter (not << containsAttr<NonSerializedAttribute>)

        let fields =
            // if ExceptionDispatchInfo, do not serialize Watson metadata.
            if isEDI then fields |> Array.filter (fun f -> not <| f.Name.Contains "Watson")
            // if Exception, ignore fields specified by System.Exception.
            elif isException then fields |> Array.filter (fun f -> f.DeclaringType <> typeof<exn>)
            else fields

        let fields, picklers =
            if isRoslynReplSubmissionType then
                // in Roslyn submission objects, ignore any fields that are not serializable
                fields
                |> Array.choose (fun f ->
                    try Some (f, resolver.Resolve f.FieldType)
                    with :? NonSerializableTypeException -> None)
                |> Array.unzip

            else
                let picklers = fields |> Array.map (fun f -> resolver.Resolve f.FieldType)
                fields, picklers


        let tags = fields |> Array.mapi (fun i f -> getNormalizedFieldName i f.Name)

        let isDeserializationCallback = isAssignableFrom typeof<IDeserializationCallback> ty
        let isObjectReference = isAssignableFrom typeof<IObjectReference> typeof<'T>

        let allMethods = ty.GetMethods(allMembers)
        let onSerializing = allMethods |> getSerializationMethods<OnSerializingAttribute>
        let onSerialized = allMethods |> getSerializationMethods<OnSerializedAttribute>
        let onDeserializing = allMethods |> getSerializationMethods<OnDeserializingAttribute>
        let onDeserialized = allMethods |> getSerializationMethods<OnDeserializedAttribute>

#if EMIT_IL
        let writer =
            if onSerializing.Length = 0 && fields.Length = 0 && onSerialized.Length = 0 then
                fun _ _ _ -> ()
            else
                let writerDele =
                    DynamicMethod.compileAction3<Pickler [], WriteState, 'T> "classSerializer" (fun picklers writer value ilGen ->

                        emitSerializationMethodCalls onSerializing (Choice1Of4 writer) value ilGen

                        emitSerializeMembers fields tags writer picklers value ilGen

                        emitSerializationMethodCalls onSerialized (Choice1Of4 writer) value ilGen
                            
                        ilGen.Emit OpCodes.Ret)

                fun w _ t -> writerDele.Invoke(picklers, w, t)

        let readerDele =
            DynamicMethod.compileFunc2<Pickler [], ReadState, 'T> "classDeserializer" (fun picklers reader ilGen ->

                // get uninitialized object and store locally
                let value = EnvItem<'T>(ilGen)
                emitObjectInitializer value ilGen

                emitSerializationMethodCalls onDeserializing (Choice2Of4 reader) value ilGen

                emitDeserializeMembers fields tags reader picklers value ilGen

                emitSerializationMethodCalls onDeserialized (Choice2Of4 reader) value ilGen

                if isDeserializationCallback then emitDeserializationCallback value ilGen

                if isObjectReference then 
                    emitObjectReferenceResolver<'T, 'T> value (Choice1Of2 reader) ilGen
                else
                    value.Load ()

                ilGen.Emit OpCodes.Ret
            )

        let clonerDele =
            DynamicMethod.compileFunc3<Pickler [], CloneState, 'T, 'T> "classCloner" (fun picklers state value ilGen ->
                // get uninitialized object and store locally
                let value' = EnvItem<'T>(ilGen)
                emitObjectInitializer value' ilGen

                emitSerializationMethodCalls onSerializing (Choice3Of4 state) value ilGen
                emitSerializationMethodCalls onDeserializing (Choice3Of4 state) value' ilGen

                emitCloneMembers fields state picklers value value' ilGen

                emitSerializationMethodCalls onSerialized (Choice3Of4 state) value ilGen
                emitSerializationMethodCalls onDeserialized (Choice3Of4 state) value' ilGen

                if isDeserializationCallback then emitDeserializationCallback value' ilGen

                if isObjectReference then 
                    emitObjectReferenceResolver<'T, 'T> value' (Choice2Of2 state) ilGen
                else
                    value'.Load ()

                ilGen.Emit OpCodes.Ret

            )

        let accepter =
            if fields.Length = 0 then ignore2
            else
                let accepterDele =
                    DynamicMethod.compileAction3<Pickler [], VisitState, 'T> "classAccepter" (fun picklers state value ilGen ->
                        emitSerializationMethodCalls onSerializing (Choice4Of4 state) value ilGen
                        emitAcceptMembers fields state picklers value ilGen
                        emitSerializationMethodCalls onSerialized (Choice4Of4 state) value ilGen
                        ilGen.Emit OpCodes.Ret
                    )

                fun v t -> accepterDele.Invoke(picklers, v,t)

        let reader r (_ : string) = readerDele.Invoke(picklers, r)
        let cloner c t = clonerDele.Invoke(picklers, c, t)
#else
        let inline run (ms : MethodInfo []) (x : obj) w =
            for i = 0 to ms.Length - 1 do 
                ms.[i].Invoke(x, [| getStreamingContext w :> obj |]) |> ignore

        let writer (w : WriteState) (_ : string) (t : 'T) =
            run onSerializing t w

            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = f.GetValue(t)
                picklers.[i].UntypedWrite w tags.[i] o

            run onSerialized t w

        let reader (r : ReadState) (_ : string) =
            let t = FormatterServices.GetUninitializedObject(typeof<'T>) |> fastUnbox<'T>
            run onDeserializing t r

            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = picklers.[i].UntypedRead r tags.[i]
                f.SetValue(t, o)

            run onDeserialized t r
            if isDeserializationCallback then (fastUnbox<IDeserializationCallback> t).OnDeserialization null
            if isObjectReference then 
                (fastUnbox<IObjectReference> t).GetRealObject r.StreamingContext :?> 'T
            else
                t

        let cloner (c : CloneState) (t : 'T) =
            let t' = FormatterServices.GetUninitializedObject(typeof<'T>) |> fastUnbox<'T>
            run onSerializing t c
            run onDeserializing t' c
            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = f.GetValue(t)
                let o' = picklers.[i].UntypedClone c o
                f.SetValue(t', o')

            run onSerialized t c
            run onDeserialized t' c
            if isDeserializationCallback then (fastUnbox<IDeserializationCallback> t').OnDeserialization null
            if isObjectReference then 
                (fastUnbox<IObjectReference> t').GetRealObject c.StreamingContext :?> 'T
            else
                t'

        let accepter (v : VisitState) (t : 'T) =
            run onSerializing t v
            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = f.GetValue(t)
                picklers.[i].UntypedAccept v o
            run onSerialized t v
#endif

        CompositePickler.Create(reader, writer, cloner, accepter, PicklerInfo.FieldSerialization)