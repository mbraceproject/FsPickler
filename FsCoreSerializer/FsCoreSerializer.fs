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

        static let resolver t = YParametric formatterCache (resolveFormatter formatterFactories genericFactories) t

        /// initializes a writer object for given stream
        static member GetObjWriter(stream : Stream, ?context : obj, ?leaveOpen) =
            let sc = match context with None -> StreamingContext() | Some ctx -> StreamingContext(StreamingContextStates.All, ctx)
            new Writer(stream, resolver, sc, ?leaveOpen = leaveOpen)

        /// initializes a reader object for given stream
        static member GetObjReader(stream : Stream, ?context : obj, ?leaveOpen) =
            let sc = match context with None -> StreamingContext() | Some ctx -> StreamingContext(StreamingContextStates.All, ctx)
            new Reader(stream, resolver, sc, ?leaveOpen = leaveOpen)

        /// register custom type serialization rules; useful for FSI type serializations
        static member RegisterTypeSerializer(tyFormatter : ITypeFormatter) : unit =
            TypeFormatter.Default <- tyFormatter

        /// register custom serialization rules for generic types
        static member RegisterGenericFormatter(gf : IGenericFormatterFactory) =
            genericFactories.AddGenericFormatter gf

        /// register an individual formatter
        static member RegisterFormatter(f : Formatter) =
            formatterCache.AddOrUpdate(f.Type, f, fun _ _ -> f) |> ignore
            
        /// register a formatter factory
        static member RegisterFormatterFactory(ff : IFormatterFactory) =
            formatterFactories.AddOrUpdate(ff.Type, ff, fun _ _ -> ff) |> ignore
            // force precomputation
            resolver ff.Type |> ignore
        
        /// resolve formatter for a given type
        static member ResolveFormatter (t : Type) = resolver t

        static member Serialize(stream : Stream, graph : obj, ?context : obj, ?leaveOpen) =
            use writer = FsCoreSerializer.GetObjWriter(stream, ?context = context, ?leaveOpen = leaveOpen)
            writer.WriteObj graph

        static member Deserialize(stream : Stream, ?context : obj, ?leaveOpen) =
            use reader = FsCoreSerializer.GetObjReader(stream, ?context = context, ?leaveOpen = leaveOpen)
            reader.ReadObj ()

        interface ISerializer with
            member c.Serialize(graph : obj, ?context : obj) =
                use mem = new MemoryStream()
                FsCoreSerializer.Serialize(mem, graph, ?context = context, leaveOpen = true)
                mem.ToArray()

            member c.Deserialize(bytes : byte [], ?context : obj) =
                use mem = new MemoryStream(bytes)
                FsCoreSerializer.Deserialize(mem, ?context = context, leaveOpen = true)

            member c.Serialize(stream : Stream, graph, ?context : obj) = FsCoreSerializer.Serialize(stream, graph, ?context = context)
            member c.Deserialize(stream : Stream, ?context : obj) = FsCoreSerializer.Deserialize(stream, ?context = context)


    [<AutoOpen>]
    module ExtensionMethods =

        open FsCoreSerializer.ObjHeader
        open FsCoreSerializer.BaseFormatters
        open FsCoreSerializer.BaseFormatters.Utils
        
        type Formatter with
            static member Create(reader : Reader -> 'T, writer : Writer -> 'T -> unit, ?cache, ?useWithSubtypes) =
                let cache = defaultArg cache true
                let useWithSubtypes = defaultArg useWithSubtypes false
                mkFormatter FormatterInfo.Custom useWithSubtypes cache reader writer

        type Writer with
            member w.WriteSeq (xs : 'T seq) =
                let fmt = w.ResolveFormatter typeof<'T>
                let xs = Array.ofSeq xs
                writeSeq w fmt xs.Length xs

        type Reader with
            member r.ReadSeq<'T> () =
                let fmt = r.ResolveFormatter typeof<'T>
                readSeq<'T> r fmt
