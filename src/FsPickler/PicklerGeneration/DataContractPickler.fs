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

    type internal DataContractPickler =

        static member Create<'T>(resolver : IPicklerResolver) =
            let t = typeof<'T>
            let cacheByRef = not <| t.IsValueType

            // following specs in http://msdn.microsoft.com/en-us/library/ms733127%28v=vs.110%29.aspx
            let tryGetDataMemberInfo (m : MemberInfo) =
                match tryGetAttr<DataMemberAttribute> m with
                | None -> None
                | Some attr ->
                    match m with
                    | :? FieldInfo as f -> Some (attr, m, f.FieldType)
                    | :? PropertyInfo as p ->
                        if not p.CanRead then
                            let msg = sprintf "property '%s' marked as data member but missing getter." p.Name
                            raise <| new PicklerGenerationException(t, msg)
                        elif not p.CanWrite then
                            let msg = sprintf "property '%s' marked as data member but missing setter." p.Name
                            raise <| new PicklerGenerationException(t, msg)

                        Some(attr, m, p.PropertyType)

                    | _ -> None

            let dataContractInfo = 
                gatherMembers t
                |> Seq.choose tryGetDataMemberInfo
                |> Seq.mapi (fun i v -> (i,v))
                // sort data members: primarily specified by user-specified order
                // and secondarily by definition order
                |> Seq.sortBy (fun (i,(attr,_,_)) -> (attr.Order, i))
                |> Seq.map snd
                |> Seq.toArray

            // if type has parameterless constructor, use that on deserialization
            let ctor = t.GetConstructor(allConstructors, null, [||], [||])
            let members = dataContractInfo |> Array.map (fun (_,m,_) -> m)
            let picklers = dataContractInfo |> Array.map (fun (_,_,t) -> resolver.Resolve t)
            let names = 
                dataContractInfo 
                |> Array.mapi (fun i (attr,m,_) -> 
                    match attr.Name with 
                    | null | "" -> getNormalizedFieldName i m.Name 
                    | name -> getNormalizedFieldName i name)

            let isDeserializationCallback = isAssignableFrom typeof<IDeserializationCallback> typeof<'T>

            let allMethods = typeof<'T>.GetMethods(allMembers)
            let onSerializing = allMethods |> getSerializationMethods<OnSerializingAttribute>
            let onSerialized = allMethods |> getSerializationMethods<OnSerializedAttribute>
            let onDeserializing = allMethods |> getSerializationMethods<OnDeserializingAttribute>
            let onDeserialized = allMethods |> getSerializationMethods<OnDeserializedAttribute>

#if EMIT_IL
            let writerDele =
                DynamicMethod.compileAction3<Pickler [], WriteState, 'T> "dataContractSerializer" (fun picklers writer parent ilGen ->

                    emitSerializationMethodCalls onSerializing (Choice1Of2 writer) parent ilGen

                    emitSerializeMembers members names writer picklers parent ilGen

                    emitSerializationMethodCalls onSerialized (Choice1Of2 writer) parent ilGen

                    ilGen.Emit OpCodes.Ret)

            let readerDele =
                DynamicMethod.compileFunc2<Pickler [], ReadState, 'T> "dataContractDeserializer" (fun picklers reader ilGen ->
                    // initialize empty value type
                    let value = EnvItem<'T>(ilGen)

                    // use parameterless constructor, if available
                    if ctor = null then
                        emitObjectInitializer typeof<'T> ilGen
                    else
                        ilGen.Emit(OpCodes.Newobj, ctor)

                    value.Store ()

                    emitSerializationMethodCalls onDeserializing (Choice2Of2 reader) value ilGen

                    emitDeserializeMembers members names reader picklers value ilGen

                    emitSerializationMethodCalls onDeserialized (Choice2Of2 reader) value ilGen

                    if isDeserializationCallback then emitDeserializationCallback value ilGen

                    value.Load ()
                    ilGen.Emit OpCodes.Ret
                )

            let writer w t v = writerDele.Invoke(picklers, w, v)
            let reader r t = readerDele.Invoke(picklers, r)
                
#else
            let inline run (ms : MethodInfo []) (x : obj) w =
                for i = 0 to ms.Length - 1 do 
                    ms.[i].Invoke(x, [| getStreamingContext w :> obj |]) |> ignore

            let writer (w : WriteState) (tag : string) (t : 'T) =
                run onSerializing t w

                for i = 0 to members.Length - 1 do
                    let value =
                        match members.[i] with
                        | :? PropertyInfo as p -> p.GetValue t
                        | :? FieldInfo as f -> f.GetValue t
                        | _ -> invalidOp "internal error on serializing '%O'." typeof<'T>

                    picklers.[i].UntypedWrite w names.[i] value

                run onSerialized t w

            let reader (r : ReadState) (tag : string) =
                let t =
                    // use parameterless constructor, if available
                    if obj.ReferenceEquals(ctor, null) then
                        FormatterServices.GetUninitializedObject(t) |> fastUnbox<'T>
                    else
                        ctor.Invoke(null) |> fastUnbox<'T>

                run onDeserializing t r

                for i = 0 to members.Length - 1 do
                    let value = picklers.[i].UntypedRead r names.[i]
                    match members.[i] with
                    | :? PropertyInfo as p -> p.SetValue(t, value)
                    | :? FieldInfo as f -> f.SetValue(t, value)
                    | _ -> invalidOp <| sprintf "internal error on deserializing '%O'." typeof<'T>

                run onDeserialized t r
                if isDeserializationCallback then (fastUnbox<IDeserializationCallback> t).OnDeserialization null
                t
#endif

            CompositePickler.Create(reader, writer, PicklerInfo.DataContract, cacheByRef = cacheByRef, useWithSubtypes = false)