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

    // pickler combinator for ISerializable types

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

                let objPickler = resolver.Resolve<obj> ()

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
                let writer (w : WriteState) (tag : string) (x : 'T) =
                    run onSerializing w x
                    let sI = new SerializationInfo(typeof<'T>, new FormatterConverter())
                    x.GetObjectData(sI, w.StreamingContext)
                    w.Formatter.WriteInt32 "count" sI.MemberCount
                    let enum = sI.GetEnumerator()
                    while enum.MoveNext() do
                        w.Formatter.WriteString "name" enum.Current.Name
                        objPickler.Write w "value" enum.Current.Value

                    run onSerialized w x

                let reader (r : ReadState) (tag : string) =
                    let sI = new SerializationInfo(typeof<'T>, new FormatterConverter())
                    let memberCount = r.Formatter.ReadInt32 "count"
                    for i = 1 to memberCount do
                        let name = r.Formatter.ReadString "name"
                        let v = objPickler.Read r "value"
                        sI.AddValue(name, v)

                    let x = create sI r.StreamingContext

                    run onDeserialized r x
                    if isDeserializationCallback then (fastUnbox<IDeserializationCallback> x).OnDeserialization null
                    x

                CompositePickler.Create(reader, writer, PicklerInfo.ISerializable, cacheByRef = true, useWithSubtypes = false)