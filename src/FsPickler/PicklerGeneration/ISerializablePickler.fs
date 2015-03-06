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

[<AutoOpen>]
module private ISerializableUtils =

    let inline mkSerializationInfo<'T> () = new SerializationInfo(typeof<'T>, new FormatterConverter())

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

    let inline writeSerializationInfo (w : WriteState) (sI : SerializationInfo) =
        let enum = sI.GetEnumerator()
        let formatter = w.Formatter

        if formatter.PreferLengthPrefixInSequences then
            formatter.WriteInt32 "memberCount" sI.MemberCount
            formatter.BeginWriteObject "serializationEntries" ObjectFlags.IsSequenceHeader

            while enum.MoveNext() do
                writeSerializationEntry w enum.Current

            formatter.EndWriteObject ()

        else
            formatter.BeginWriteObject "serializationEntries" ObjectFlags.IsSequenceHeader

            while enum.MoveNext() do
                formatter.WriteNextSequenceElement true
                writeSerializationEntry w enum.Current

            formatter.WriteNextSequenceElement false
            formatter.EndWriteObject ()

    let inline readSerializationInfo<'T> (r : ReadState) =
        let sI = mkSerializationInfo<'T> ()
        let formatter = r.Formatter

        if formatter.PreferLengthPrefixInSequences then
            let memberCount = formatter.ReadInt32 "memberCount"
            let _ = formatter.BeginReadObject "serializationEntries"
            for i = 1 to memberCount do
                readSerializationEntry r sI
            formatter.EndReadObject ()
        else
            let _ = formatter.BeginReadObject "serializationEntries"
            while formatter.ReadNextSequenceElement() do
                readSerializationEntry r sI
            formatter.EndReadObject ()

        sI

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
                run onSerializing w t
                let sI = mkSerializationInfo<'T> ()
                t.GetObjectData(sI, w.StreamingContext)
                writeSerializationInfo w sI
                run onSerialized w t

            let reader (r : ReadState) (tag : string) =
                let sI = readSerializationInfo<'T> r
                let t = create sI r.StreamingContext
                run onDeserialized r t
                if isDeserializationCallback then (fastUnbox<IDeserializationCallback> t).OnDeserialization null
                t

            CompositePickler.Create(reader, writer, PicklerInfo.ISerializable, cacheByRef = true, useWithSubtypes = false)

    /// SerializationInfo-based pickler combinator
    static member FromSerializationInfo<'T>(ctor : SerializationInfo -> 'T, proj : SerializationInfo -> 'T -> unit) : Pickler<'T> =
        let writer (state : WriteState) (tag : string) (t : 'T) =
            let sI = mkSerializationInfo<'T> ()
            do proj sI t
            writeSerializationInfo state sI

        let reader (state : ReadState) (tag : string) =
            let sI = readSerializationInfo<'T> state
            ctor sI

        CompositePickler.Create(reader, writer, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = false)