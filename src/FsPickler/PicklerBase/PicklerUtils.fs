module internal Nessos.FsPickler.PicklerUtils

    open System
    open System.IO
    open System.Collections
    open System.Collections.Generic
    open System.Reflection
    open System.Runtime.Serialization
    open System.Text.RegularExpressions
    
    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection

    /// pickler initialization abbreviation
    let inline mkPickler<'T> (info:PicklerInfo) (useWithSubtypes:bool) (cache:bool) 
                                        (reader : ReadState -> string -> 'T) (writer : WriteState -> string -> 'T -> unit) =

        CompositePickler.Create<'T>(reader, writer, info, cacheByRef = cache, useWithSubtypes = useWithSubtypes)


    let private memberNameRegex = new Regex(@"[^a-zA-Z0-9]")
    /// normalizes member name into a serialializable string.
    let getTagFromMemberInfo (m : MemberInfo) =
        memberNameRegex.Replace(m.Name, "")

    //
    //  Array/IEnumerable serialization utils
    //

#if DEBUG
    let writeArray (w : WriteState) (p : Pickler<'T>) tag (ts : 'T []) =
#else
    let inline writeArray (w : WriteState) (p : Pickler<'T>) tag (ts : 'T []) =
#endif
        w.Formatter.BeginWriteBoundedSequence tag ts.Length
        for t in ts do p.Write w "elem" t
        w.Formatter.EndWriteBoundedSequence ()

#if DEBUG
    let readArray (r : ReadState) (p : Pickler<'T>) tag =
#else
    let inline readArray (r : ReadState) (p : Pickler<'T>) tag =
#endif
        let length = r.Formatter.BeginReadBoundedSequence tag
        let array = Array.zeroCreate<'T> length
        for i = 0 to length - 1 do
            array.[i] <- p.Read r "elem"
        r.Formatter.EndReadBoundedSequence ()
        array

    /// Serializes a sequence where the length is known beforehand.

#if DEBUG
    let writeBoundedSequence (w : WriteState) (p : Pickler<'T>) (length : int) tag (ts : seq<'T>) =
#else
    let inline writeBoundedSequence (w : WriteState) (p : Pickler<'T>) (length : int) tag (ts : seq<'T>) =
#endif
        w.Formatter.BeginWriteBoundedSequence tag length
        for t in ts do p.Write w "elem" t
        w.Formatter.EndWriteBoundedSequence ()

#if DEBUG
    let readBoundedSequence (r : ReadState) (p : Pickler<'T>) tag =
#else
    let inline readBoundedSequence (r : ReadState) (p : Pickler<'T>) tag =
#endif
        let length = r.Formatter.BeginReadBoundedSequence tag
        let ts = Array.zeroCreate<'T> length
        for i = 0 to length - 1 do
            ts.[i] <- p.Read r "elem"
        r.Formatter.EndReadBoundedSequence ()
        
        ts


    let writeUnboundedSequence (ep : Pickler<'T>) (w : WriteState) (tag : string) (ts : seq<'T>) =
        let formatter = w.Formatter
        formatter.BeginWriteUnBoundedSequence tag
        use e = ts.GetEnumerator()
        while e.MoveNext() do
            formatter.WriteHasNextElement true
            ep.Write w "elem" e.Current

        formatter.WriteHasNextElement false

    let readUnboundedSequence (ep : Pickler<'T>) (r : ReadState) (tag : string) : seq<'T> =
        let formatter = r.Formatter
        do formatter.BeginReadUnBoundedSequence tag
        let ra = new ResizeArray<'T> ()
        while formatter.ReadHasNextElement () do
            let t = ep.Read r "elem"
            ra.Add t

        ra :> _