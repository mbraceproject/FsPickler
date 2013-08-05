namespace FsCoreSerializer
    
    open System
    open System.IO
    open System.Reflection
    open System.Text
    open System.Collections.Generic
    open System.Collections.Concurrent
    open System.Runtime.Serialization

    open FsCoreSerializer
    open FsCoreSerializer.TypeShape
    open FsCoreSerializer.FormatterUtils
    open FsCoreSerializer.BaseFormatters
    open FsCoreSerializer.FSharpTypeFormatters
    open FsCoreSerializer.FormatterResolution


    type FsCoreSerializerRegistry private () =

        static let mutable registrationPhase = true

        static let checkRegistration () =
            if not registrationPhase then
                invalidOp "FsCoreSerializer: can only register formatter rules *before* any serializations have begun."

        static let formatterFactories = new ConcurrentDictionary<Type,IFormatterFactory> ()

        static let genericFactories = 
            let gI = new GenericFormatterIndex()
            gI.AddGenericFormatters(genericFormatters) ; gI

        static let formatterCache =
            seq {
                yield! primitiveFormatters
                yield! valueFormatters
                yield! reflectionFormatters
                yield! fsFormatters
            } 
            |> Seq.map (fun f -> KeyValuePair(f.Type, f)) 
            |> fun x -> new ConcurrentDictionary<_,_>(x)

        /// resolve formatter for a given type
        static member ResolveFormatter (t : Type) : Formatter =
            // disable any further formatter registrations
            do registrationPhase <- false
            YParametric formatterCache (resolveFormatter formatterFactories genericFactories) t

        /// register custom type serialization rules; useful for FSI type serializations
        static member RegisterTypeNameConverter(tyConverter : ITypeNameConverter) =
            do checkRegistration ()
            TypeFormatter.TypeNameConverter <- tyConverter

        /// register custom serialization rules for generic types
        static member RegisterGenericFormatter(gf : IGenericFormatterFactory) =
            do checkRegistration ()
            genericFactories.AddGenericFormatter gf

        /// register an individual formatter
        static member RegisterFormatter(f : Formatter) =
            do checkRegistration ()
            formatterCache.AddOrUpdate(f.Type, f, fun _ _ -> f)

        /// register a formatter factory
        static member RegisterFormatterFactory(ff : IFormatterFactory) =
            do checkRegistration ()
            formatterFactories.AddOrUpdate(ff.Type, ff, fun _ _ -> ff) |> ignore



    type FsCoreSerializer (?encoding : Encoding) =
        // observed up to 2x performance improvement when using UTF8 encoding in BinaryWriter/Reader
        let encoding = defaultArg encoding Encoding.UTF8

        /// initializes a writer object for given stream
        static member GetObjectWriter(stream : Stream, ?context : obj, ?leaveOpen, ?encoding) =
            let sc = match context with None -> StreamingContext() | Some ctx -> StreamingContext(StreamingContextStates.All, ctx)
            new Writer(stream, FsCoreSerializerRegistry.ResolveFormatter, sc, ?leaveOpen = leaveOpen, ?encoding = encoding)

        /// initializes a reader object for given stream
        static member GetObjectReader(stream : Stream, ?context : obj, ?leaveOpen, ?encoding) =
            let sc = match context with None -> StreamingContext() | Some ctx -> StreamingContext(StreamingContextStates.All, ctx)
            new Reader(stream, FsCoreSerializerRegistry.ResolveFormatter, sc, ?leaveOpen = leaveOpen, ?encoding = encoding)

        /// serialize a given object graph to underlying stream
        static member Serialize(stream : Stream, graph : 'T, ?context : obj, ?encoding, ?writeType) : unit =
            let writeType = defaultArg writeType true
            use writer = FsCoreSerializer.GetObjectWriter(stream, ?context = context, leaveOpen = true, ?encoding = encoding)

            if writeType then writer.WriteObj graph
            else
                writer.Write<'T> graph

        /// deserialize from underlying stream
        static member Deserialize<'T> (stream : Stream, ?context : obj, ?encoding, ?readType) : 'T =
            let readType = defaultArg readType true
            use reader = FsCoreSerializer.GetObjectReader(stream, ?context = context, leaveOpen = true, ?encoding = encoding)
            
            if readType then reader.ReadObj() :?> 'T
            else
                reader.Read<'T> ()


        // interface implementation
        interface ISerializer with
            member c.Name = "FsCoreSerializer"

            member c.Serialize(stream : Stream, graph : obj, ?context) : unit =
                FsCoreSerializer.Serialize(stream, graph, ?context = context, encoding = encoding, writeType = true)

            member c.Serialize(graph : obj, ?context) : byte [] =
                use m = new MemoryStream()
                FsCoreSerializer.Serialize(m, graph, ?context = context, encoding = encoding, writeType = true)
                m.ToArray()

            member c.Deserialize(stream : Stream, ?context) : obj =
                FsCoreSerializer.Deserialize(stream, ?context = context, encoding = encoding, readType = true)

            member c.Deserialize(bytes : byte [], ?context) : obj =
                use m = new MemoryStream(bytes)
                FsCoreSerializer.Deserialize(m, ?context = context, encoding = encoding, readType = true)


    [<AutoOpen>]
    module ExtensionMethods =
        
        type Formatter with
            /// creates a new custom formatter
            static member Create(reader : Reader -> 'T, writer : Writer -> 'T -> unit, ?cache, ?useWithSubtypes) =
                let cache = defaultArg cache (not typeof<'T>.IsValueType)
                let useWithSubtypes = defaultArg useWithSubtypes false
                mkFormatter FormatterInfo.Custom useWithSubtypes cache reader writer

        type Writer with
            /// Serializes a sequence of values to the underlying stream
            member w.WriteSeq<'T> (xs : 'T seq) : unit =
                let fmt = FsCoreSerializerRegistry.ResolveFormatter typeof<'T>
                use e = xs.GetEnumerator()
                while e.MoveNext() do
                    w.BW.Write true
                    write w fmt e.Current

                w.BW.Write false

            /// Serializes a sequence of key/value pairs to the underlying stream
            member w.WriteKeyValueSeq<'K,'V> (xs : ('K * 'V) seq) : unit =
                let kf = FsCoreSerializerRegistry.ResolveFormatter typeof<'K>
                let vf = FsCoreSerializerRegistry.ResolveFormatter typeof<'V>
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
                let fmt = FsCoreSerializerRegistry.ResolveFormatter typeof<'T>
                let ra = new ResizeArray<'T> ()
                while r.BR.ReadBoolean() do
                    let next = read r fmt :?> 'T
                    ra.Add next

                ra :> _

            /// Deserializes a sequence of key/value pairs from the underlying stream
            member r.ReadKeyValueSeq<'K,'V> () : seq<'K * 'V> =
                let kf = FsCoreSerializerRegistry.ResolveFormatter typeof<'K>
                let vf = FsCoreSerializerRegistry.ResolveFormatter typeof<'V>
                let ra = new ResizeArray<'K * 'V> ()
                while r.BR.ReadBoolean() do
                    let k = read r kf :?> 'K
                    let v = read r vf :?> 'V
                    ra.Add (k,v)

                ra :> _