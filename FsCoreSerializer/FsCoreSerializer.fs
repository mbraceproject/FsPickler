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
    open FsCoreSerializer.BaseFormatters
    open FsCoreSerializer.FSharpFormatters
    open FsCoreSerializer.FormatterResolution


    type FsCoreSerializer () =
        static let genericFormatters = 
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

        static let resolver t = YParametric formatterCache (resolveFormatter genericFormatters) t

        static member RegisterTypeSerializer(tySerializer : ITypeSerializer) =
            TypeSerializer.Default <- tySerializer

        static member RegisterGenericFormatter(gf : IGenericFormatterFactory) =
            genericFormatters.AddGenericFormatter gf

        static member RegisterFormatter(f : Formatter) =
            formatterCache.AddOrUpdate(f.Type, f, fun _ _ -> f)

        static member internal ResolveFormatter (t : Type) = resolver t

        member __.Serialize(stream : Stream, graph : obj, ?context : obj) =
            let sc = match context with None -> StreamingContext() | Some ctx -> StreamingContext(StreamingContextStates.All, ctx)
            use bw = new BinaryWriter(stream, Encoding.UTF8, true)
            let writer = new Writer(bw, typeFormatter, resolver, sc)
            writer.WriteObj graph
            bw.Flush()

        member __.Deserialize(stream : Stream, ?context : obj) =
            let sc = match context with None -> StreamingContext() | Some ctx -> StreamingContext(StreamingContextStates.All, ctx)
            use br = new BinaryReader(stream, Encoding.UTF8, true)
            let reader = new Reader(br, typeFormatter, resolver, sc)
            reader.ReadObj ()


        interface ISerializer with
            member c.Serialize(graph : obj, ?context : obj) =
                use mem = new MemoryStream()
                c.Serialize(mem, graph, ?context = context)
                mem.ToArray()

            member c.Deserialize(bytes : byte [], ?context : obj) =
                use mem = new MemoryStream(bytes)
                c.Deserialize(mem, ?context = context)

            member c.Serialize(stream : Stream, graph, ?context : obj) = c.Serialize(stream, graph, ?context = context)
            member c.Deserialize(stream : Stream, ?context : obj) = c.Deserialize(stream, ?context = context)
            member c.WriteObj(stream : Stream, graph, ?context : obj) = c.Serialize(stream, graph, ?context = context)
            member c.ReadObj(stream : Stream, ?context : obj) = c.Deserialize(stream, ?context = context)


    and FixedTypeSerializer(ty : Type) =

        let formatter = FsCoreSerializer.ResolveFormatter ty

        member __.Serialize(bw : BinaryWriter, o : obj, ?context : obj) =
            let sc = match context with None -> StreamingContext() | Some ctx -> StreamingContext(StreamingContextStates.All, ctx)
            let w = new Writer(bw, typeFormatter, FsCoreSerializer.ResolveFormatter, sc)
            w.WriteObj(formatter, o)

        member __.Deserialize(br : BinaryReader, ?context : obj) : obj =
            let sc = match context with None -> StreamingContext() | Some ctx -> StreamingContext(StreamingContextStates.All, ctx)
            let r = new Reader(br, typeFormatter, FsCoreSerializer.ResolveFormatter, sc)
            r.ReadObj formatter