module internal Nessos.FsPickler.PicklerUtils

    open System
    open System.IO
    open System.Collections
    open System.Collections.Generic
    open System.Reflection
    open System.Runtime.Serialization
    
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


    //
    //  Array/IEnumerable serialization utils
    //

#if DEBUG
    let writeArray (w : WriteState) (p : Pickler<'T>) (ts : 'T []) =
#else
    let inline writeArray (w : WriteState) (p : Pickler<'T>) (ts : 'T []) =
#endif
        w.Formatter.WriteInt32 "length" ts.Length
        for t in ts do p.Write w "item" t

#if DEBUG
    let readArray (r : ReadState) (p : Pickler<'T>) =
#else
    let inline readArray (r : ReadState) (p : Pickler<'T>) =
#endif
        let length = r.Formatter.ReadInt32 "length"
        let array = Array.zeroCreate<'T> length
        for i = 0 to length - 1 do
            array.[i] <- p.Read r "item"
        array



    /// Serializes a sequence where the length is known beforehand.

#if DEBUG
    let writeSequence (w : WriteState) (p : Pickler<'T>) (length : int) (ts : seq<'T>) =
#else
    let inline writeSequence (w : WriteState) (p : Pickler<'T>) (length : int) (ts : seq<'T>) =
#endif
        w.Formatter.WriteInt32 "length" length
        for t in ts do p.Write w "item" t

#if DEBUG
    let readSequence (r : ReadState) (p : Pickler<'T>) =
#else
    let inline readSequence (r : ReadState) (p : Pickler<'T>) =
#endif
        let length = r.Formatter.ReadInt32 "length"
        let ts = Array.zeroCreate<'T> length
        for i = 0 to length - 1 do
            ts.[i] <- p.Read r "item"
        ts


    /// length passed as argument to avoid unecessary evaluations of sequence

#if DEBUG
    let writePairSequence (w : WriteState) (kp : Pickler<'K>) (vp : Pickler<'V>) (length : int) (xs : ('K * 'V) seq) =
#else
    let inline writePairSequence (w : WriteState) (kp : Pickler<'K>) (vp : Pickler<'V>) (length : int) (xs : ('K * 'V) seq) =
#endif
        w.Formatter.WriteInt32 "length" length
        for k,v in xs do
            kp.Write w "key" k
            vp.Write w "val" v

#if DEBUG
    let readPairSequence (r : ReadState) (kp : Pickler<'K>) (vp : Pickler<'V>) =
#else
    let inline readPairSequence (r : ReadState) (kp : Pickler<'K>) (vp : Pickler<'V>) =
#endif
        let length = r.Formatter.ReadInt32 "length"
        let xs = Array.zeroCreate<'K * 'V> length

        for i = 0 to length - 1 do
            let k = kp.Read r "key"
            let v = vp.Read r "val"
            xs.[i] <- k,v

        xs

    /// write a sequence where length is not known beforehand

    let writeSequenceNoLength (p : Pickler<'T>) (w : WriteState) (ts : 'T seq) : unit =
        match ts with
        | :? ('T []) as arr ->
            w.Formatter.WriteBoolean "isMaterialized" true
            w.Formatter.WriteInt32 "length" arr.Length
            for i = 0 to arr.Length - 1 do
                p.Write w "item" arr.[i]

        | :? ('T list) as list ->
            w.Formatter.WriteBoolean "isMaterialized" true
            w.Formatter.WriteInt32 "length" list.Length

            let rec iter rest =
                match rest with
                | [] -> ()
                | t :: tl ->
                    p.Write w "item" t
                    iter tl

            iter list
        | _ ->
            w.Formatter.WriteBoolean "isMaterialized" false
            use e = ts.GetEnumerator()
            while e.MoveNext() do
                w.Formatter.WriteBoolean "done" false
                p.Write w "item" e.Current

            w.Formatter.WriteBoolean "done" true

    let readSequenceNoLength (p : Pickler<'T>) (r : ReadState) : 'T seq =

        if r.Formatter.ReadBoolean "isMaterialized" then
            let length = r.Formatter.ReadInt32 "length"
            let array = Array.zeroCreate<'T> length
            for i = 0 to length - 1 do
                array.[i] <- p.Read r "item"
            array :> _
        else
            let ra = new ResizeArray<'T> ()
            while not <| r.Formatter.ReadBoolean "done" do
                let next = p.Read r "item"
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

            pickler.Write state "item" t

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
                
            pickler.Read state "item"

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