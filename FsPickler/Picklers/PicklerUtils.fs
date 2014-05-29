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


    /// set pickler id based on set of source picklers
    /// will result in error if source picklers have conflicting source ids
    /// used with external combinator library
    let setPicklerId<'T when 'T :> Pickler> (sourcePicklers : seq<Pickler>) (targetPickler : ^T) =
        let mutable current = null
        for p in sourcePicklers do
            match p.CacheId with
            | null -> ()
            | source when current = null -> current <- source
            | source when current = source -> ()
            | source -> 
                let msg = "attempting to generate pickler using incompatible sources."
                raise <| new PicklerGenerationException(p.Type, msg)

        targetPickler.CacheId <- current
        targetPickler

    // checks pickler compatibility at runtime
    let checkPicklerCompat (uuid : string) (p : Pickler) =
        match p.CacheId with
        | null -> ()
        | id when id <> uuid ->
            let msg = sprintf "Attempting to use pickler of type '%O' generated from incompatible cache." p.Type
            raise <| new SerializationException(msg)
        | _ -> ()


    let private memberNameRegex = new Regex(@"[^a-zA-Z0-9]")
    let getTagFromMemberInfo (m : MemberInfo) =
        memberNameRegex.Replace(m.Name, "")

    //
    //  Array/IEnumerable serialization utils
    //

#if DEBUG
    let writeArray (w : WriteState) (p : Pickler<'T>) (ts : 'T []) =
#else
    let inline writeArray (w : WriteState) (p : Pickler<'T>) (ts : 'T []) =
#endif
        w.Formatter.BeginWriteBoundedSequence ts.Length
        for t in ts do p.Write w "elem" t
        w.Formatter.EndWriteBoundedSequence ()

#if DEBUG
    let readArray (r : ReadState) (p : Pickler<'T>) =
#else
    let inline readArray (r : ReadState) (p : Pickler<'T>) =
#endif
        let length = r.Formatter.BeginReadBoundedSequence ()
        let array = Array.zeroCreate<'T> length
        for i = 0 to length - 1 do
            array.[i] <- p.Read r "elem"
        r.Formatter.EndReadBoundedSequence ()
        array



    /// Serializes a sequence where the length is known beforehand.

#if DEBUG
    let writeBoundedSequence (w : WriteState) (p : Pickler<'T>) (length : int) (ts : seq<'T>) =
#else
    let inline writeBoundedSequence (w : WriteState) (p : Pickler<'T>) (length : int) (ts : seq<'T>) =
#endif
        w.Formatter.BeginWriteBoundedSequence length
        for t in ts do p.Write w "elem" t
        w.Formatter.EndWriteBoundedSequence ()

#if DEBUG
    let readBoundedSequence (r : ReadState) (p : Pickler<'T>) =
#else
    let inline readBoundedSequence (r : ReadState) (p : Pickler<'T>) =
#endif
        let length = r.Formatter.BeginReadBoundedSequence ()
        let ts = Array.zeroCreate<'T> length
        for i = 0 to length - 1 do
            ts.[i] <- p.Read r "elem"
        r.Formatter.EndReadBoundedSequence ()
        
        ts


    /// length passed as argument to avoid unecessary evaluations of sequence

#if DEBUG
    let writeBoundedPairSequence (w : WriteState) (kp : Pickler<'K>) (vp : Pickler<'V>) (length : int) (xs : ('K * 'V) seq) =
#else
    let inline writeBoundedPairSequence (w : WriteState) (kp : Pickler<'K>) (vp : Pickler<'V>) (length : int) (xs : ('K * 'V) seq) =
#endif
        w.Formatter.BeginWriteBoundedSequence length
        for k,v in xs do
            kp.Write w "key" k
            vp.Write w "val" v
        w.Formatter.EndWriteBoundedSequence ()

#if DEBUG
    let readBoundedPairSequence (r : ReadState) (kp : Pickler<'K>) (vp : Pickler<'V>) =
#else
    let inline readBoundedPairSequence (r : ReadState) (kp : Pickler<'K>) (vp : Pickler<'V>) =
#endif
        let length = r.Formatter.BeginReadBoundedSequence ()
        let xs = Array.zeroCreate<'K * 'V> length

        for i = 0 to length - 1 do
            let k = kp.Read r "key"
            let v = vp.Read r "val"
            xs.[i] <- k,v

        r.Formatter.EndReadBoundedSequence ()

        xs

    /// write a sequence where length is not known beforehand

    let writeSequence (p : Pickler<'T>) (w : WriteState) (ts : 'T seq) : unit =
        let formatter = w.Formatter
        match ts with
        | :? ('T []) as arr ->
            formatter.WriteBoolean "isBounded" true
            formatter.BeginWriteBoundedSequence arr.Length
            for i = 0 to arr.Length - 1 do
                p.Write w "elem" arr.[i]

            formatter.EndWriteBoundedSequence ()

        | :? ('T list) as list ->
            formatter.WriteBoolean "isBounded" true
            formatter.BeginWriteBoundedSequence list.Length

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
            formatter.BeginWriteUnBoundedSequence ()
            use e = ts.GetEnumerator()
            while e.MoveNext() do
                formatter.WriteHasNextElement true
                p.Write w "elem" e.Current

            formatter.WriteHasNextElement false

    let readSequence (p : Pickler<'T>) (r : ReadState) : 'T seq =
        let formatter = r.Formatter

        if formatter.ReadBoolean "isBounded" then
            let length = formatter.BeginReadBoundedSequence ()
            let array = Array.zeroCreate<'T> length
            for i = 0 to length - 1 do
                array.[i] <- p.Read r "elem"
            formatter.EndReadBoundedSequence ()
            array :> _
        else
            formatter.BeginReadUnBoundedSequence ()
            let ra = new ResizeArray<'T> ()
            while formatter.ReadHasNextElement () do
                let next = p.Read r "elem"
                ra.Add next

            ra :> _

    let writePairSequenceNoLength (kp : Pickler<'K>) (vp : Pickler<'V>) (w : WriteState) (xs : ('K * 'V) seq) : unit =
        match xs with
        | :? (('K * 'V) []) as arr ->
            w.Formatter.WriteBoolean "isMaterialized" true
            w.Formatter.WriteInt32 "length" arr.Length

            for i = 0 to arr.Length - 1 do
                let k,v = arr.[i]
                kp.Write w "key" k
                vp.Write w "val" v

        | :? (('K * 'V) list) as list ->
            w.Formatter.WriteBoolean "isMaterialized" true
            w.Formatter.WriteInt32 "length" list.Length

            let rec iter rest =
                match rest with
                | [] -> ()
                | (k,v) :: tl ->
                    kp.Write w "key" k
                    vp.Write w "val" v
                    iter tl

            iter list

        | _ ->
            w.Formatter.WriteBoolean "isMaterialized" false
            let e = xs.GetEnumerator()

            while e.MoveNext() do
                w.Formatter.WriteBoolean "done" false
                let k,v = e.Current
                kp.Write w "key" k
                vp.Write w "val" v

            w.Formatter.WriteBoolean "done" true


    /// Deserializes a sequence of key/value pairs from the underlying stream
    let readPairSequenceNoLength (kp : Pickler<'K>) (vp : Pickler<'V>) (r : ReadState) =

        if r.Formatter.ReadBoolean "isMaterialized" then
            let length = r.Formatter.ReadInt32 "length"
            let arr = Array.zeroCreate<'K * 'V> length
            for i = 0 to length - 1 do
                let k = kp.Read r "key"
                let v = vp.Read r "val"
                arr.[i] <- k,v
            arr :> seq<'K * 'V>
        else
            let ra = new ResizeArray<'K * 'V> ()
            while not <| r.Formatter.ReadBoolean "done" do
                let k = kp.Read r "key"
                let v = vp.Read r "val"
                ra.Add (k,v)

            ra :> seq<'K * 'V>

    //
    //  top-level sequence serialization
    //

    let sequenceStateResetThreshold = 1000 // resets write/read state inside the given interval

    /// serializes a sequence of objects to stream

    let writeTopLevelSequence (pickler : Pickler<'T>) (state : WriteState) (tag : string) (values : seq<'T>) : int =

        let isValue = pickler.TypeInfo <= TypeInfo.Value

#if DEBUG
        let write idx t =
#else
        let inline write idx t =
#endif
            if isValue && idx % sequenceStateResetThreshold = 0 then
                state.ResetCounters()

            pickler.Write state "elem" t

        state.Formatter.BeginWriteRoot tag
        state.Formatter.BeginWriteObject pickler.TypeInfo pickler.PicklerInfo tag ObjectFlags.IsSequenceHeader
            
        // specialize enumeration
        let length =
            match values with
            | :? ('T []) as array ->
                let n = array.Length
                for i = 0 to n - 1 do
                    write i array.[i]
                n

            | :? ('T list) as list ->
                let rec writeLst i lst =
                    match lst with
                    | [] -> i
                    | t :: ts -> write i t ; writeLst (i+1) ts

                writeLst 0 list
            | _ ->
                let mutable i = 0
                for t in values do
                    write i t
                    i <- i + 1
                i

        state.Formatter.EndWriteObject()
        state.Formatter.EndWriteRoot()
        length

    let readTopLevelSequence (pickler : Pickler<'T>) (state : ReadState) (tag : string) (length : int) : IEnumerator<'T> =

        let isValue = pickler.TypeInfo <= TypeInfo.Value

        let read idx =
            if isValue && idx % sequenceStateResetThreshold = 0 then
                state.ResetCounters()
                
            pickler.Read state "elem"

        // read id
        state.Formatter.BeginReadRoot tag
        let flags = state.Formatter.BeginReadObject pickler.TypeInfo pickler.PicklerInfo tag
            
        // read object header
        if flags <> ObjectFlags.IsSequenceHeader then
            let msg = "FsPickler: invalid stream data; expected sequence serialization."
            raise <| new SerializationException(msg)

        let cnt = ref 0
        let curr = ref Unchecked.defaultof<'T>
        {
            new System.Collections.Generic.IEnumerator<'T> with
                member __.Current = !curr
                member __.Current = box !curr
                member __.Dispose () = state.Formatter.EndReadRoot() ; (state :> IDisposable).Dispose()
                member __.MoveNext () =
                    if !cnt < length then
                        curr := read !cnt
                        incr cnt
                        true
                    else
                        false

                member __.Reset () = raise <| NotSupportedException()
        }

    let writeTopLevelSequenceUntyped (pickler : Pickler) (state : WriteState) (tag : string) (values : IEnumerable) : int =
        let unpacker =
            {
                new IPicklerUnpacker<int> with
                    member __.Apply (p : Pickler<'T>) =
                        writeTopLevelSequence p state tag (values :?> IEnumerable<'T>)
            }

        pickler.Unpack unpacker

    let readTopLevelSequenceUntyped (pickler : Pickler) (state : ReadState) (tag : string) (length : int) : IEnumerator =
        let unpacker =
            {
                new IPicklerUnpacker<IEnumerator> with
                    member __.Apply (p : Pickler<'T>) =
                        readTopLevelSequence p state tag length :> _
            }

        pickler.Unpack unpacker