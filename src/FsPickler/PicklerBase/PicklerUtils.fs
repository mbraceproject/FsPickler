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
                                        (reader : ReadState -> 'T) (writer : WriteState -> 'T -> unit) =

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

    /// write a sequence where length is not known beforehand

    let writeSequence (p : Pickler<'T>) tag (w : WriteState) (ts : 'T seq) : unit =
        let formatter = w.Formatter
        match ts with
        | :? ('T []) as arr ->
            formatter.WriteBoolean "isBounded" true
            formatter.BeginWriteBoundedSequence tag arr.Length
            for i = 0 to arr.Length - 1 do
                p.Write w "elem" arr.[i]

            formatter.EndWriteBoundedSequence ()

        | :? ('T list) as list ->
            formatter.WriteBoolean "isBounded" true
            formatter.BeginWriteBoundedSequence tag list.Length

            let rec iter rest =
                match rest with
                | [] -> ()
                | t :: tl ->
                    p.Write w "elem" t
                    iter tl

            iter list
            formatter.EndWriteBoundedSequence ()

        | _ ->
            formatter.WriteBoolean "isBounded" false
            formatter.BeginWriteUnBoundedSequence tag
            use e = ts.GetEnumerator()
            while e.MoveNext() do
                formatter.WriteHasNextElement true
                p.Write w "elem" e.Current

            formatter.WriteHasNextElement false

    let readSequence (p : Pickler<'T>) tag (r : ReadState) : 'T seq =
        let formatter = r.Formatter

        if formatter.ReadBoolean "isBounded" then
            let length = formatter.BeginReadBoundedSequence tag
            let array = Array.zeroCreate<'T> length
            for i = 0 to length - 1 do
                array.[i] <- p.Read r "elem"
            formatter.EndReadBoundedSequence ()
            array :> _
        else
            formatter.BeginReadUnBoundedSequence tag
            let ra = new ResizeArray<'T> ()
            while formatter.ReadHasNextElement () do
                let next = p.Read r "elem"
                ra.Add next

            ra :> _