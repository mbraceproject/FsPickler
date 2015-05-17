namespace Nessos.FsPickler

open System
open System.IO
open System.Reflection
open System.Threading
open System.Runtime.Serialization

open Nessos.FsPickler
open Nessos.FsPickler.Reflection

#if EMIT_IL
open System.Reflection.Emit
open Nessos.FsPickler.Emit
open Nessos.FsPickler.PicklerEmit
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
                emitObjectInitializer typeof<'T> ilGen
                value.Store ()

                emitDeserializeMembers fields tags reader picklers value ilGen

                value.Load ()
                ilGen.Emit OpCodes.Ret
            )

        let clonerDele =
            DynamicMethod.compileFunc3<Pickler [], CloneState, 'T, 'T> "structCloner" (fun picklers state value ilGen ->
                // initialize empty value type
                let value' = EnvItem<'T>(ilGen)
                emitObjectInitializer typeof<'T> ilGen
                value'.Store ()
            
                emitCloneMembers fields state picklers value value' ilGen

                value'.Load()
                ilGen.Emit OpCodes.Ret
            )

        let writer (w : WriteState) (tag : string) (t : 'T) = writerDele.Invoke(picklers, w, t)
        let reader (r : ReadState) (tag : string) = readerDele.Invoke(picklers, r)
        let cloner (c : CloneState) (t : 'T) = clonerDele.Invoke(picklers, c, t)

#else
        let writer (w : WriteState) (tag : string) (t : 'T) =
            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = f.GetValue(t)
                picklers.[i].UntypedWrite w tags.[i] o

        let reader (r : ReadState) (tag : string) =
            let t = FormatterServices.GetUninitializedObject(typeof<'T>)
            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = picklers.[i].UntypedRead r tags.[i]
                f.SetValue(t, o)
                
            fastUnbox<'T> t

        let cloner (c : CloneState) (t : 'T) =
            let t' = FormatterServices.GetUninitializedObject(typeof<'T>) |> fastUnbox<'T>
            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = f.GetValue t
                let o' = picklers.[i].UntypedClone c o
                f.SetValue(t', o')
            t'
#endif

        CompositePickler.Create(reader, writer, cloner, PicklerInfo.FieldSerialization, cacheByRef = false, useWithSubtypes = false)

// general-purpose pickler combinator for reference types

type internal ClassFieldPickler =

    static member Create<'T when 'T : not struct>(resolver : IPicklerResolver) =
        let ty = typeof<'T>
        let isEDI = not runsOnMono.Value && isExceptionDispatchInfo ty // ExceptionDispatchInfo serialization not supported in mono.
        let isSerializable =
            ty.IsSerializable
            // compiler generated types in C# are not marked as serializable, but should in principle be treated as such.
            || containsAttr<System.Runtime.CompilerServices.CompilerGeneratedAttribute> ty
            || isEDI

        if not isSerializable then raise <| new NonSerializableTypeException(ty)

        let fields = 
            gatherSerializedFields ty
            |> Array.filter (not << containsAttr<NonSerializedAttribute>)

        let fields =
            // if ExceptionDispatchInfo, do not serialize Watson metadata.
            if isEDI then fields |> Array.filter (fun f -> not <| f.Name.Contains "Watson")
            else fields

        let picklers = fields |> Array.map (fun f -> resolver.Resolve f.FieldType)
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

                        emitSerializationMethodCalls onSerializing (Choice1Of3 writer) value ilGen

                        emitSerializeMembers fields tags writer picklers value ilGen

                        emitSerializationMethodCalls onSerialized (Choice1Of3 writer) value ilGen
                            
                        ilGen.Emit OpCodes.Ret)

                fun w tag t -> writerDele.Invoke(picklers, w, t)

        let readerDele =
            DynamicMethod.compileFunc2<Pickler [], ReadState, 'T> "classDeserializer" (fun picklers reader ilGen ->

                // get uninitialized object and store locally
                let value = EnvItem<'T>(ilGen)
                emitObjectInitializer typeof<'T> ilGen
                value.Store ()

                emitSerializationMethodCalls onDeserializing (Choice2Of3 reader) value ilGen

                emitDeserializeMembers fields tags reader picklers value ilGen

                emitSerializationMethodCalls onDeserialized (Choice2Of3 reader) value ilGen

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
                emitObjectInitializer typeof<'T> ilGen
                value'.Store ()

                emitSerializationMethodCalls onSerializing (Choice3Of3 state) value ilGen
                emitSerializationMethodCalls onDeserializing (Choice3Of3 state) value' ilGen

                emitCloneMembers fields state picklers value value' ilGen

                emitSerializationMethodCalls onSerialized (Choice3Of3 state) value ilGen
                emitSerializationMethodCalls onDeserialized (Choice3Of3 state) value' ilGen

                if isDeserializationCallback then emitDeserializationCallback value' ilGen

                if isObjectReference then 
                    emitObjectReferenceResolver<'T, 'T> value' (Choice2Of2 state) ilGen
                else
                    value'.Load ()

                ilGen.Emit OpCodes.Ret

            )

        let reader r (tag : string) = readerDele.Invoke(picklers, r)
        let cloner c t = clonerDele.Invoke(picklers, c, t)
#else
        let inline run (ms : MethodInfo []) (x : obj) w =
            for i = 0 to ms.Length - 1 do 
                ms.[i].Invoke(x, [| getStreamingContext w :> obj |]) |> ignore

        let writer (w : WriteState) (tag : string) (t : 'T) =
            run onSerializing t w

            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                let o = f.GetValue(t)
                picklers.[i].UntypedWrite w tags.[i] o

            run onSerialized t w

        let reader (r : ReadState) (tag : string) =
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
                t
#endif

        CompositePickler.Create(reader, writer, cloner, PicklerInfo.FieldSerialization, cacheByRef = true, useWithSubtypes = false)