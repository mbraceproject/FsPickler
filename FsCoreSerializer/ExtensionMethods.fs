namespace FsCoreSerializer


    [<AutoOpen>]
    module ExtensionMethods =

        open FsCoreSerializer.FormatterUtils
        
        type Formatter with
            /// <summary>Initializes a formatter out of a pair of read/write lambdas.</summary>
            /// <param name="cache">Specifies whether the serializer should cache by reference when serializing.</param>
            /// <param name="useWithSubtypes">Specifies whether this specific formatter should apply to all subtypes.</param>
            static member Create(reader : Reader -> 'T, writer : Writer -> 'T -> unit, ?cache, ?useWithSubtypes) =
                let cache = defaultArg cache (not typeof<'T>.IsValueType)
                let useWithSubtypes = defaultArg useWithSubtypes false
                mkFormatter FormatterInfo.Custom useWithSubtypes cache reader writer

        type Writer with

            /// Serializes a sequence of values to the underlying stream
            member w.WriteSeq<'T> (xs : 'T seq) : unit =
                let fmt = w.ResolveFormatter<'T> ()
                let isValue = fmt.TypeInfo <= TypeInfo.Value
                match xs with
                | :? ('T []) as arr ->
                    w.BW.Write true
                    w.BW.Write arr.Length
                    for i = 0 to arr.Length - 1 do
                        write isValue w fmt <| arr.[i]
                | :? ('T list) as list ->
                    w.BW.Write true
                    w.BW.Write list.Length
                    let rec iter rest =
                        match rest with
                        | [] -> ()
                        | hd :: tl ->
                            write isValue w fmt hd
                            iter tl

                    iter list
                | _ ->
                    w.BW.Write false
                    use e = xs.GetEnumerator()
                    while e.MoveNext() do
                        w.BW.Write true
                        write isValue w fmt e.Current

                    w.BW.Write false

            /// Serializes a sequence of key/value pairs to the underlying stream
            member w.WriteKeyValueSeq<'K,'V> (xs : ('K * 'V) seq) : unit =
                let kf = w.ResolveFormatter<'K> ()
                let vf = w.ResolveFormatter<'V> ()
                let kIsValue = kf.TypeInfo <= TypeInfo.Value
                let vIsValue = vf.TypeInfo <= TypeInfo.Value
                match xs with
                | :? (('K * 'V) []) as arr ->
                    w.BW.Write true
                    w.BW.Write arr.Length
                    for i = 0 to arr.Length - 1 do
                        let k,v = arr.[i]
                        write kIsValue w kf k
                        write vIsValue w vf v
                | :? (('K * 'V) list) as list ->
                    w.BW.Write true
                    w.BW.Write list.Length
                    let rec iter rest =
                        match rest with
                        | [] -> ()
                        | (k,v) :: tl ->
                            write kIsValue w kf k
                            write vIsValue w vf v
                            iter tl

                    iter list
                | _ ->
                    w.BW.Write false
                    let e = xs.GetEnumerator()
                    while e.MoveNext() do
                        w.BW.Write true
                        let k,v = e.Current
                        write kIsValue w kf k
                        write vIsValue w vf v

                    w.BW.Write false

        type Reader with
            /// Deserializes a sequence of objects from the underlying stream
            member r.ReadSeq<'T> () : 'T seq =
                let fmt = r.ResolveFormatter<'T> ()
                let isValue = fmt.TypeInfo <= TypeInfo.Value

                if r.BR.ReadBoolean() then
                    let length = r.BR.ReadInt32()
                    let arr = Array.zeroCreate<'T> length
                    for i = 0 to length - 1 do
                        arr.[i] <- read isValue r fmt
                    arr :> _
                else
                    let ra = new ResizeArray<'T> ()
                    while r.BR.ReadBoolean() do
                        let next = read isValue r fmt
                        ra.Add next

                    ra :> _

            /// Deserializes a sequence of key/value pairs from the underlying stream
            member r.ReadKeyValueSeq<'K,'V> () : seq<'K * 'V> =
                let kf = r.ResolveFormatter<'K> ()
                let vf = r.ResolveFormatter<'V> ()
                let kIsValue = kf.TypeInfo <= TypeInfo.Value
                let vIsValue = vf.TypeInfo <= TypeInfo.Value

                if r.BR.ReadBoolean() then
                    let length = r.BR.ReadInt32()
                    let arr = Array.zeroCreate<'K * 'V> length
                    for i = 0 to length - 1 do
                        let k = read kIsValue r kf
                        let v = read vIsValue r vf
                        arr.[i] <- k,v
                    arr :> _
                else
                    let ra = new ResizeArray<'K * 'V> ()
                    while r.BR.ReadBoolean() do
                        let k = read kIsValue r kf
                        let v = read vIsValue r vf
                        ra.Add (k,v)

                    ra :> _