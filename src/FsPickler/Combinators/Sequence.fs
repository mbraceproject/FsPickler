namespace Nessos.FsPickler

open System
open System.Collections
open System.Collections.Generic

module internal SequenceUtils =

    [<Literal>]
    let elemTag = "elem"

    /// reads a new object and ensures it is a sequence header

    let inline beginReadSequence (formatter : IPickleFormatReader) (tag : string) =
        let flags = formatter.BeginReadObject tag
        if not <| ObjectFlags.hasFlag flags ObjectFlags.IsSequenceHeader then
            let msg = sprintf "Expected new array but was '%O'." flags
            raise <| new FormatException(msg)

    /// writes a sequence whose length is known a priori
#if DEBUG
    let writeBoundedSequence
#else
    let inline writeBoundedSequence
#endif
            (ep : Pickler<'T>) (count : int) (w : WriteState) (tag : string) (ts : seq<'T>) =

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


    /// serializes a sequence of unknown length to the stream ; returns its eventual length

    let writeUnboundedSequence (ep : Pickler<'T>) (w : WriteState) (tag : string) (ts : seq<'T>) : int =

        let formatter = w.Formatter
        do formatter.BeginWriteObject tag ObjectFlags.IsSequenceHeader

        let count =
            // specialize enumeration strategy
            match ts with
            | :? ('T []) as array ->
                let len = array.Length
                for i = 0 to len - 1 do
                    formatter.WriteNextSequenceElement true
                    ep.Write w elemTag array.[i]

                len

            | :? ('T list) as list ->
                let rec write i ts =
                    match ts with
                    | [] -> i
                    | t :: rest -> 
                        formatter.WriteNextSequenceElement true
                        ep.Write w elemTag t
                        write (i+1) rest

                write 0 list
            | _ ->
                let mutable cnt = 0
                for t in ts do
                    formatter.WriteNextSequenceElement true
                    ep.Write w elemTag t
                    cnt <- cnt + 1

                cnt

        formatter.WriteNextSequenceElement false
        formatter.EndWriteObject ()
        count

    /// lazily deserialize a sequence of elements ; reserved for top-level sequence deserializations only.

    let readUnboundedSequenceLazy (ep : Pickler<'T>) (r : ReadState) (tag : string) : seq<'T> =

        do beginReadSequence r.Formatter tag

        let unique = new Latch ()
        let getEnumerator () =
            if unique.Trigger() then
                let current = ref Unchecked.defaultof<'T>
                { 
                    new IEnumerator<'T> with
                        member __.Current = current.Value
                        member __.Current = box current.Value
                        member __.Dispose () = r.Formatter.Dispose()
                        member __.MoveNext () =
                            if r.Formatter.ReadNextSequenceElement () then
                                current := ep.Read r elemTag
                                true
                            else
                                r.Formatter.EndReadObject ()
                                r.Formatter.EndReadRoot ()
                                false

                        member __.Reset () = raise <| NotSupportedException()
                }
            else
                invalidOp "Deserialization enumerable can only be consumed once."
        {
            new IEnumerable<'T> with
                member __.GetEnumerator () = getEnumerator ()
                member __.GetEnumerator () = getEnumerator () :> IEnumerator
        }

open SequenceUtils

// Defines a pickler combinator for IEnumerables
// note that this is not the default pickling behaviour as this
// always forces enumeration and serialization of all elements regardless of
// underlying implementation

type internal SeqPickler =
    static member Create(ep : Pickler<'T>) =

        let writer (w : WriteState) (tag : string) (ts : seq<'T>) =
            let length = writeUnboundedSequence ep w tag ts
            ()

        let reader (r : ReadState) (tag : string) =
            let formatter = r.Formatter
            do beginReadSequence formatter tag
            let ra = new ResizeArray<'T> ()
            while formatter.ReadNextSequenceElement() do
                ra.Add <| ep.Read r elemTag

            formatter.EndReadObject()

            ra :> seq<'T>

        CompositePickler.Create<seq<'T>>(reader, writer, PicklerInfo.Combinator, cacheByRef = false, useWithSubtypes = false, bypass = true)