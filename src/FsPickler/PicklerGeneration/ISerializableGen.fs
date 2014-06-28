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

    module private ISerializableUtils =

        let inline writeSerializationEntry (w : WriteState) (entry : SerializationEntry) =
            let formatter = w.Formatter
            formatter.BeginWriteObject "entry" ObjectFlags.None
            formatter.WriteString "Name" entry.Name
            w.TypePickler.Write w "Type" entry.ObjectType
            let op = w.PicklerResolver.Resolve entry.ObjectType
            op.UntypedWrite w "Value" entry.Value
            formatter.EndWriteObject()

        let inline readSerializationEntry (r : ReadState) (sI : SerializationInfo) =
            let formatter = r.Formatter
            let _ = formatter.BeginReadObject "entry"
            let name = formatter.ReadString "Name"
            let objectType = r.TypePickler.Read r "Type"
            let op = r.PicklerResolver.Resolve objectType
            let value = op.UntypedRead r "Value"
            sI.AddValue(name, value)
            formatter.EndReadObject()

    type internal ISerializablePickler =

        static member Create<'T when 'T :> ISerializable>(resolver : IPicklerResolver) =
            match typeof<'T>.TryGetConstructor [| typeof<SerializationInfo> ; typeof<StreamingContext> |] with
            | None -> raise <| new NonSerializableTypeException(typeof<'T>, "is ISerializable but does not implement constructor with parameters (SerializationInfo, StreamingContext).")
            | Some ctorInfo ->
                let allMethods = typeof<'T>.GetMethods(allMembers)
                let onSerializing = allMethods |> getSerializationMethods<OnSerializingAttribute> |> wrapDelegate<Action<'T, StreamingContext>>
                let onSerialized = allMethods |> getSerializationMethods<OnSerializedAttribute> |> wrapDelegate<Action<'T, StreamingContext>>
                let onDeserialized = allMethods |> getSerializationMethods<OnDeserializedAttribute> |> wrapDelegate<Action<'T, StreamingContext>>

                let isDeserializationCallback = typeof<IDeserializationCallback>.IsAssignableFrom typeof<'T>

                let entryP = resolver.Resolve<SerializationEntry> ()

                let inline run (dele : Action<'T, StreamingContext> []) w x =
                    for d in dele do
                        d.Invoke(x, getStreamingContext w)

#if EMIT_IL
                let ctorDele = wrapISerializableConstructor<'T> ctorInfo

                let inline create si sc = ctorDele.Invoke(si, sc)
#else
                let inline create (si : SerializationInfo) (sc : StreamingContext) = 
                    ctorInfo.Invoke [| si :> obj ; sc :> obj |] |> fastUnbox<'T>
#endif
                let writer (w : WriteState) (tag : string) (t : 'T) =
                    let formatter = w.Formatter

                    run onSerializing w t
                    let sI = new SerializationInfo(typeof<'T>, new FormatterConverter())
                    t.GetObjectData(sI, w.StreamingContext)

                    let enum = sI.GetEnumerator()

                    if formatter.PreferLengthPrefixInSequences then
                        formatter.WriteInt32 "memberCount" sI.MemberCount
                        formatter.BeginWriteObject "serializationEntries" ObjectFlags.IsSequenceHeader

                        while enum.MoveNext() do
                            ISerializableUtils.writeSerializationEntry w enum.Current

                        formatter.EndWriteObject ()

                    else
                        formatter.BeginWriteObject "serializationEntries" ObjectFlags.IsSequenceHeader

                        while enum.MoveNext() do
                            formatter.WriteNextSequenceElement true
                            ISerializableUtils.writeSerializationEntry w enum.Current

                        formatter.WriteNextSequenceElement false
                        formatter.EndWriteObject ()

                    run onSerialized w t

                let reader (r : ReadState) (tag : string) =
                    let formatter = r.Formatter
                    let sI = new SerializationInfo(typeof<'T>, new FormatterConverter())

                    if formatter.PreferLengthPrefixInSequences then
                        let memberCount = formatter.ReadInt32 "memberCount"
                        let _ = formatter.BeginReadObject "serializationEntries"
                        for i = 1 to memberCount do
                            ISerializableUtils.readSerializationEntry r sI
                        formatter.EndReadObject ()
                    else
                        let _ = formatter.BeginReadObject "serializationEntries"
                        while formatter.ReadNextSequenceElement() do
                            ISerializableUtils.readSerializationEntry r sI
                        formatter.EndReadObject ()

                    let t = create sI r.StreamingContext

                    run onDeserialized r t
                    if isDeserializationCallback then (fastUnbox<IDeserializationCallback> t).OnDeserialization null
                    t

                CompositePickler.Create(reader, writer, PicklerInfo.ISerializable, cacheByRef = true, useWithSubtypes = false)