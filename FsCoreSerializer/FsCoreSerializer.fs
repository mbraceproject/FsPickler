namespace FsCoreSerializer
    
    open System
    open System.IO
    open System.Reflection
    open System.Text
    open System.Collections.Generic
    open System.Collections.Concurrent
    open System.Runtime.Serialization

    open FsCoreSerializer
    open FsCoreSerializer.Utils
    open FsCoreSerializer.TypeShape
    open FsCoreSerializer.FormatterUtils
    open FsCoreSerializer.BaseFormatters
    open FsCoreSerializer.FSharpTypeFormatters
    open FsCoreSerializer.FormatterResolution

    [<Sealed>]
    type FormatterRegistry () =

        let typeNameConverter = ref (DefaultTypeNameConverter() :> ITypeNameConverter)
        let formatters = Atom.atom Map.empty<string, Formatter>
        let formatterFactories = Atom.atom Map.empty<string, IFormatterFactory>
        let genericFactories = Atom.atom GenericFormatterIndex.Empty

        /// register custom type serialization rules; useful for FSI type serializations
        member __.TypeNameConverter
            with get () = typeNameConverter.Value
            and set tyConv = typeNameConverter := tyConv

        /// register formatter for a specific type
        member __.RegisterFormatter(f : Formatter) =
            formatters.Swap(fun fmts -> fmts.AddNoOverwrite(f.Type.AssemblyQualifiedName, f))

        /// register a formatter factory
        member __.RegisterFormatterFactory(ff : IFormatterFactory) =
            formatterFactories.Swap(fun fmtf -> fmtf.AddNoOverwrite(ff.Type.AssemblyQualifiedName, ff))

        /// register generic formatter rules
        member __.RegisterGenericFormatter(gf : IGenericFormatterFactory) =
            genericFactories.Swap(fun genericFactories -> genericFactories.AddGenericFormatter(gf, Fail))

        member internal __.FormatterFactories = formatterFactories.Value
        member internal __.GenericFactories = genericFactories.Value

        member __.RegisteredFormatters = formatters.Value |> Map.toSeq |> Seq.map snd |> List.ofSeq
        member __.RegisteredFormatterFactories = formatterFactories.Value |> Map.toSeq |> Seq.map snd |> List.ofSeq
        member __.RegisteredGenericFormatterFactories = genericFactories.Value.GetEntries()



    and private FormatterCache (tyConv : ITypeNameConverter, formatters : seq<Formatter>, 
                                    gfi : GenericFormatterIndex, ffs : Map<string, IFormatterFactory>) =
        
        let cache =
            seq {
                yield! mkPrimitiveFormatters ()
                yield! mkValueFormatters ()
                yield! mkReflectionFormatters tyConv
            }
            |> Seq.map (fun f -> KeyValuePair(f.Type, f)) 
            |> fun x -> new ConcurrentDictionary<_,_>(x)

        let gfi =
            let fsharpGenericFormatters = mkFSharpGenericFormatters ()
            gfi.AddGenericFormatters(fsharpGenericFormatters, Discard)

        do 
            for f in formatters do 
                cache.AddOrUpdate(f.Type, f, fun _ _ -> f) |> ignore

        member __.TypeNameConverter = tyConv
        member __.ResolveFormatter (t : Type) = YParametric cache (resolveFormatter tyConv ffs gfi) t
        
        static member FromFormatterRegistry(fr : FormatterRegistry) =
            new FormatterCache(fr.TypeNameConverter, fr.RegisteredFormatters, fr.GenericFactories, fr.FormatterFactories)

        static member Default() = 
            new FormatterCache(new DefaultTypeNameConverter(), [], GenericFormatterIndex.Empty, Map.empty)


    and FsCoreSerializer private (cache : FormatterCache) =

        new () = new FsCoreSerializer(FormatterCache.Default())
        new (registry : FormatterRegistry) = new FsCoreSerializer(FormatterCache.FromFormatterRegistry registry)

        /// <summary>Initializes an object writer for the given stream.</summary>
        /// <param name="stream">The target stream.</param>
        /// <param name="context">The optional streaming context object.</param>
        /// <param name="leaveOpen">Leave the stream open when finished. Defaults to true.</param>
        /// <param name="encoding">The encoding used to in the binary writer. Defaults to UTF8.</param>
        member s.GetObjectWriter(stream : Stream, ?context : obj, ?leaveOpen, ?encoding) =
            if not stream.CanWrite then invalidOp "Cannot write to stream."
            let sc = match context with None -> StreamingContext() | Some ctx -> StreamingContext(StreamingContextStates.All, ctx)
            new Writer(stream, cache.TypeNameConverter, cache.ResolveFormatter, sc, ?leaveOpen = leaveOpen, ?encoding = encoding)

        /// <summary>Initializes an object reader for given the stream.</summary>
        /// <param name="stream">The source stream.</param>
        /// <param name="context">The optional streaming context object.</param>
        /// <param name="leaveOpen">Leave the stream open when finished. Defaults to true.</param>
        /// <param name="encoding">The encoding used to in the binary writer. Defaults to UTF8.</param>
        member __.GetObjectReader(stream : Stream, ?context : obj, ?leaveOpen, ?encoding) =
            if not stream.CanRead then invalidOp "Cannot read from stream."
            let sc = match context with None -> StreamingContext() | Some ctx -> StreamingContext(StreamingContextStates.All, ctx)
            new Reader(stream, cache.TypeNameConverter, cache.ResolveFormatter, sc, ?leaveOpen = leaveOpen, ?encoding = encoding)

        /// <summary>Serialize an object of given type to the underlying stream.</summary>
        /// <param name="stream">The target stream.</param>
        /// <param name="context">The untyped parameter passed to the streaming context.</param>
        /// <param name="encoding">The encoding passed to the binary writer.</param>
        ///     Useful when serializing sequences of small objects.</param>
        member __.Serialize<'T>(stream : Stream, graph : 'T, ?context : obj, ?encoding) : unit =
            use writer = __.GetObjectWriter(stream, ?context = context, leaveOpen = true, ?encoding = encoding)

            writer.Write<'T> graph

        /// <summary>Deserialize object of given type from the underlying stream.</summary>
        /// <param name="stream">The source stream.</param>
        /// <param name="context">The untyped parameter passed to the streaming context.</param>
        /// <param name="encoding">The encoding passed to the binary reader.</param>
        ///     Useful when serializing sequences of small objects.</param>
        member __.Deserialize<'T> (stream : Stream, ?context : obj, ?encoding) : 'T =
            use reader = __.GetObjectReader(stream, ?context = context, leaveOpen = true, ?encoding = encoding)
            
            reader.Read<'T> ()

        /// <summary>Deserialize object of given type from the underlying stream.</summary>
        /// <param name="stream">The source stream.</param>
        /// <param name="graphType">The given graph type.</param>
        /// <param name="context">The untyped parameter passed to the streaming context.</param>
        /// <param name="encoding">The encoding passed to the binary reader.</param>
        ///     Useful when serializing sequences of small objects.</param>
        member __.SerializeUntyped (stream : Stream, graphType : Type, graph : obj, ?context : obj, ?encoding) : unit =
            use writer = __.GetObjectWriter(stream, ?context = context, leaveOpen = true, ?encoding = encoding)

            writer.WriteObj(graphType, graph)

        /// <summary>Deserialize object of given type from the underlying stream.</summary>
        /// <param name="stream">The source stream.</param>
        /// <param name="graphType">The given graph type.</param>
        /// <param name="context">The untyped parameter passed to the streaming context.</param>
        /// <param name="encoding">The encoding passed to the binary reader.</param>
        ///     Useful when serializing sequences of small objects.</param>
        member __.DeserializeUntyped (stream : Stream, graphType : Type, ?context : obj, ?encoding) : obj =
            use reader = __.GetObjectReader(stream, ?context = context, leaveOpen = true, ?encoding = encoding)

            reader.ReadObj graphType

        member __.IsSerializableType (t : Type) =
            try cache.ResolveFormatter t |> ignore ; true
            with :? NonSerializableTypeException -> false

        member __.ResolveFormatter<'T> () : Formatter<'T> = Typed(cache.ResolveFormatter typeof<'T>)

    [<AutoOpen>]
    module ExtensionMethods =
        
        type Formatter with
            /// <summary>Initializes a formatter out of a pair of read/write lambdas.</summary>
            /// <param name="cache">Specifies whether the serializer should cache by reference when serializing.</param>
            /// <param name="useWithSubtypes">Specifies whether this specific formatter should apply to all subtypes.</param>
            static member Create(reader : Reader -> 'T, writer : Writer -> 'T -> unit, ?cache, ?useWithSubtypes) : Formatter<'T> =
                let cache = defaultArg cache (not typeof<'T>.IsValueType)
                let useWithSubtypes = defaultArg useWithSubtypes false
                let fmt = mkFormatter FormatterInfo.Custom useWithSubtypes cache reader writer
                Typed fmt

        type Writer with

            /// Serializes a sequence of values to the underlying stream
            member w.WriteSeq<'T> (xs : 'T seq) : unit =
                let (Typed fmt) = w.ResolveFormatter<'T> ()
                match xs with
                | :? ('T []) as arr ->
                    w.BW.Write true
                    w.BW.Write arr.Length
                    for i = 0 to arr.Length - 1 do
                        write w fmt <| arr.[i]
                | :? ('T list) as list ->
                    w.BW.Write true
                    w.BW.Write list.Length
                    let rec iter rest =
                        match rest with
                        | [] -> ()
                        | hd :: tl ->
                            write w fmt hd
                            iter tl

                    iter list
                | _ ->
                    w.BW.Write false
                    use e = xs.GetEnumerator()
                    while e.MoveNext() do
                        w.BW.Write true
                        write w fmt e.Current

                    w.BW.Write false

            /// Serializes a sequence of key/value pairs to the underlying stream
            member w.WriteKeyValueSeq<'K,'V> (xs : ('K * 'V) seq) : unit =
                let (Typed kf) = w.ResolveFormatter<'K> ()
                let (Typed vf) = w.ResolveFormatter<'V> ()
                match xs with
                | :? (('K * 'V) []) as arr ->
                    w.BW.Write true
                    w.BW.Write arr.Length
                    for i = 0 to arr.Length - 1 do
                        let k,v = arr.[i]
                        write w kf k
                        write w vf v
                | :? (('K * 'V) list) as list ->
                    w.BW.Write true
                    w.BW.Write list.Length
                    let rec iter rest =
                        match rest with
                        | [] -> ()
                        | (k,v) :: tl ->
                            write w kf k
                            write w vf v
                            iter tl

                    iter list
                | _ ->
                    w.BW.Write false
                    let e = xs.GetEnumerator()
                    while e.MoveNext() do
                        w.BW.Write true
                        let k,v = e.Current
                        write w kf k
                        write w vf v

                    w.BW.Write false

        type Reader with
            /// Deserializes a sequence of objects from the underlying stream
            member r.ReadSeq<'T> () : 'T seq =
                let (Typed fmt) = r.ResolveFormatter<'T> ()

                if r.BR.ReadBoolean() then
                    let length = r.BR.ReadInt32()
                    let arr = Array.zeroCreate<'T> length
                    for i = 0 to length - 1 do
                        arr.[i] <- read r fmt :?> 'T
                    arr :> _
                else
                    let ra = new ResizeArray<'T> ()
                    while r.BR.ReadBoolean() do
                        let next = read r fmt :?> 'T
                        ra.Add next

                    ra :> _

            /// Deserializes a sequence of key/value pairs from the underlying stream
            member r.ReadKeyValueSeq<'K,'V> () : seq<'K * 'V> =
                let (Typed kf) = r.ResolveFormatter<'K> ()
                let (Typed vf) = r.ResolveFormatter<'V> ()
                if r.BR.ReadBoolean() then
                    let length = r.BR.ReadInt32()
                    let arr = Array.zeroCreate<'K * 'V> length
                    for i = 0 to length - 1 do
                        let k = read r kf :?> 'K
                        let v = read r vf :?> 'V
                        arr.[i] <- k,v
                    arr :> _
                else
                    let ra = new ResizeArray<'K * 'V> ()
                    while r.BR.ReadBoolean() do
                        let k = read r kf :?> 'K
                        let v = read r vf :?> 'V
                        ra.Add (k,v)

                    ra :> _