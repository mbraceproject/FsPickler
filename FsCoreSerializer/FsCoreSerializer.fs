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

        /// <summary>Serialize a given object graph to underlying stream.</summary>
        /// <param name="stream">The target stream.</param>
        /// <param name="context">The untyped parameter passed to the streaming context.</param>
        /// <param name="encoding">The encoding passed to the binary writer.</param>
        /// <param name="writeType">Specifies whether the type parameter should be recorded in the serialization header.
        ///     Useful when serializing sequences of small objects.</param>
        member __.Serialize(stream : Stream, graph : 'T, ?context : obj, ?encoding, ?writeType) : unit =
            let writeType = defaultArg writeType true
            use writer = __.GetObjectWriter(stream, ?context = context, leaveOpen = true, ?encoding = encoding)

            if writeType then writer.WriteObj graph
            else
                writer.Write<'T> graph

        /// <summary>Deserialize a given object graph from and underlying stream.</summary>
        /// <param name="stream">The source stream.</param>
        /// <param name="context">The untyped parameter passed to the streaming context.</param>
        /// <param name="encoding">The encoding passed to the binary reader.</param>
        /// <param name="readType">Specifies whether the type parameter should be read from the serialization header.
        ///     Useful when serializing sequences of small objects.</param>
        member __.Deserialize<'T> (stream : Stream, ?context : obj, ?encoding, ?readType) : 'T =
            let readType = defaultArg readType true
            use reader = __.GetObjectReader(stream, ?context = context, leaveOpen = true, ?encoding = encoding)
            
            if readType then reader.ReadObj() :?> 'T
            else
                reader.Read<'T> ()

        member __.IsSerializableType (t : Type) =
            try cache.ResolveFormatter t |> ignore ; true
            with :? NonSerializableTypeException -> false

    [<AutoOpen>]
    module ExtensionMethods =
        
        type Formatter with
            /// <summary>Initializes a formatter out of a pair of read/write lambdas.</summary>
            /// <param name="cache">Specifies whether the serializer should cache by reference when serializing.</param>
            /// <param name="useWithSubtypes">Specifies whether this specific formatter should apply to all of its subtypes.</param>
            static member Create(reader : Reader -> 'T, writer : Writer -> 'T -> unit, ?cache, ?useWithSubtypes) =
                let cache = defaultArg cache (not typeof<'T>.IsValueType)
                let useWithSubtypes = defaultArg useWithSubtypes false
                mkFormatter FormatterInfo.Custom useWithSubtypes cache reader writer

        type Writer with
            /// Serializes a sequence of values to the underlying stream
            member w.WriteSeq<'T> (xs : 'T seq) : unit =
                let fmt = w.ResolveFormatter typeof<'T>
                use e = xs.GetEnumerator()
                while e.MoveNext() do
                    w.BW.Write true
                    write w fmt e.Current

                w.BW.Write false

            /// Serializes a sequence of key/value pairs to the underlying stream
            member w.WriteKeyValueSeq<'K,'V> (xs : ('K * 'V) seq) : unit =
                let kf = w.ResolveFormatter typeof<'K>
                let vf = w.ResolveFormatter typeof<'V>
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
                let fmt = r.ResolveFormatter typeof<'T>
                let ra = new ResizeArray<'T> ()
                while r.BR.ReadBoolean() do
                    let next = read r fmt :?> 'T
                    ra.Add next

                ra :> _

            /// Deserializes a sequence of key/value pairs from the underlying stream
            member r.ReadKeyValueSeq<'K,'V> () : seq<'K * 'V> =
                let kf = r.ResolveFormatter typeof<'K>
                let vf = r.ResolveFormatter typeof<'V>
                let ra = new ResizeArray<'K * 'V> ()
                while r.BR.ReadBoolean() do
                    let k = read r kf :?> 'K
                    let v = read r vf :?> 'V
                    ra.Add (k,v)

                ra :> _