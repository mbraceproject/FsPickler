namespace MBrace.FsPickler

open System
open System.Collections
open System.Collections.Generic

module internal SequenceUtils =

    [<Literal>]
    let elemTag = "elem"

    /// reads a new object and ensures it is a sequence header

    let inline beginReadSequence (formatter : IPickleFormatReader) (tag : string) =
        let flags = formatter.BeginReadObject tag
        if not <| Enum.hasFlag flags ObjectFlags.IsSequenceHeader then
            let msg = sprintf "Expected new array but was '%O'." flags
            raise <| new FormatException(msg)

    /// writes a sequence whose length is known a priori
#if DEBUG
    let writeBoundedSequence
#else
    let inline writeBoundedSequence
#endif
            (ep : Pickler<'T>) (_ : int) (w : WriteState) (tag : string) (ts : seq<'T>) =

        w.Formatter.BeginWriteObject tag ObjectFlags.IsSequenceHeader
        for e in ts do ep.Write w elemTag e
        w.Formatter.EndWriteObject ()


    /// reads a sequence whose length is known a priori

#if DEBUG
    let readBoundedSequence
#else
    let inline readBoundedSequence
#endif
            (ep : Pickler<'T>) (count : int) (r : ReadState) (tag : string) =

        do beginReadSequence r.Formatter tag
        let array = Array.zeroCreate<'T> count
        for i = 0 to array.Length - 1 do array.[i] <- ep.Read r elemTag
        r.Formatter.EndReadObject ()
        array

    //
    //  Sequence serialization of which length is unknown a priori
    //

    // number of objects after which to reset cache state in sequence serialization
    // this relates to an ObjectIdGenerator performance issue that hits as the number
    // of cached objects increases. See https://github.com/mbraceproject/FsPickler/issues/38 for more.
    // Resetting the cache is safe when serializing top-level sequences.
    [<Literal>]
    let objectCountResetThreshold = 50000L

    /// serializes a sequence of unknown length to the stream ; returns its eventual length
    let writeUnboundedSequence enableReset (ep : Pickler<'T>) (w : WriteState) (tag : string) (ts : seq<'T>) : int =

        let formatter = w.Formatter
        do formatter.BeginWriteObject tag ObjectFlags.IsSequenceHeader

        let inline checkReset () = if enableReset && w.ObjectCount > objectCountResetThreshold then w.Reset()

        let count =
            // specialize enumeration strategy
            match ts with
            | :? ('T []) as array ->
                let len = array.Length
                for i = 0 to len - 1 do
                    formatter.WriteNextSequenceElement true
                    checkReset ()
                    ep.Write w elemTag array.[i]

                len

            | :? ('T list) as list ->
                let rec write i ts =
                    match ts with
                    | [] -> i
                    | t :: rest -> 
                        formatter.WriteNextSequenceElement true
                        checkReset ()
                        ep.Write w elemTag t
                        write (i+1) rest

                write 0 list

            | _ ->
                let mutable i = 0
                for t in ts do
                    formatter.WriteNextSequenceElement true
                    checkReset ()
                    ep.Write w elemTag t
                    i <- i + 1

                i

        formatter.WriteNextSequenceElement false
        formatter.EndWriteObject ()
        count

    /// lazily deserialize a sequence of elements ; reserved for top-level sequence deserializations only.

    [<AutoSerializable(false)>]
    type private SequenceDeserializerEnumerator<'T>(ep : Pickler<'T>, r : ReadState, enableReset : bool) =
        let mutable current = Unchecked.defaultof<'T>
        let mutable i = 0
        interface IEnumerator<'T> with
            member __.Current = current
            member __.Current = box current
            member __.Dispose () = r.Formatter.Dispose()
            member __.MoveNext () =
                if r.Formatter.ReadNextSequenceElement () then
                    if enableReset && r.ObjectCount > objectCountResetThreshold then r.Reset()
                    current <- ep.Read r elemTag
                    i <- i + 1
                    true
                else
                    r.Formatter.EndReadObject ()
                    r.Formatter.EndReadRoot ()
                    false

            member __.Reset () = raise <| NotSupportedException()


    [<AutoSerializable(false)>]
    type private SequenceDeserializerEnumerable<'T>(ep : Pickler<'T>, r : ReadState, enableReset : bool) =
        let unique = new Latch()
        let getEnumerator() =
            if unique.Trigger() then
                new SequenceDeserializerEnumerator<'T>(ep, r, enableReset) :> IEnumerator<'T>
            else
                invalidOp "Deserialization enumerable can only be consumed once."

        interface IEnumerable<'T> with
            member __.GetEnumerator () = getEnumerator ()
            member __.GetEnumerator () = getEnumerator () :> IEnumerator


    let readUnboundedSequenceLazy enableReset (ep : Pickler<'T>) (r : ReadState) (tag : string) : seq<'T> =
        do beginReadSequence r.Formatter tag
        new SequenceDeserializerEnumerable<'T>(ep, r, enableReset) :> _


open SequenceUtils

// Defines a pickler combinator for IEnumerables
// note that this is not the default pickling behaviour as this
// always forces enumeration and serialization of all elements regardless of
// underlying implementation

type internal SeqPickler =
    static member Create(ep : Pickler<'T>) =

        let writer (w : WriteState) (tag : string) (ts : seq<'T>) =
            let _ = writeUnboundedSequence false ep w tag ts
            ()

        let reader (r : ReadState) (tag : string) =
            let formatter = r.Formatter
            do beginReadSequence formatter tag
            let ra = new ResizeArray<'T> ()
            while formatter.ReadNextSequenceElement() do
                ra.Add <| ep.Read r elemTag

            formatter.EndReadObject()

            ra :> seq<'T>

        let cloner (c : CloneState) (ts : seq<'T>) =
            ts |> Seq.map (ep.Clone c) |> Seq.toArray :> seq<'T>

        let accepter (v : VisitState) (ts : seq<'T>) =
            for t in ts do ep.Accept v t

        CompositePickler.Create<seq<'T>>(reader, writer, cloner, accepter, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = true, bypass = true)