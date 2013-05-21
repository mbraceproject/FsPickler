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

        /// register custom type serialization rules; useful for FSI type serializations
        static member RegisterTypeSerializer(tyFormatter : ITypeFormatter) : unit =
            TypeFormatter.Default <- tyFormatter

        /// register custom serialization rules for generic types
        static member RegisterGenericFormatter(gf : IGenericFormatterFactory) =
            genericFormatters.AddGenericFormatter gf

        /// register an individual formatter
        static member RegisterFormatter(f : Formatter) =
            formatterCache.AddOrUpdate(f.Type, f, fun _ _ -> f)
        
        /// recursively resolves formatter for a given type
        static member ResolveFormatter (t : Type) = resolver t

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


    and FixedStreamSerializer(stream : Stream, ?fixedType, ?leaveOpen) =
        let bw = new BinaryWriter(stream, Encoding.UTF8, defaultArg leaveOpen true)
        let formatter = fixedType |> Option.map FsCoreSerializer.ResolveFormatter

        member __.Serialize(o : obj, ?context : obj) =
            let sc = match context with None -> StreamingContext() | Some ctx -> StreamingContext(StreamingContextStates.All, ctx)
            let w = new Writer(bw, typeFormatter, FsCoreSerializer.ResolveFormatter, sc)
            
            match formatter with
            | None -> w.WriteObj o
            | Some fmt -> w.WriteObj(fmt, o)

        interface IDisposable with
            member __.Dispose () = bw.Flush () ; bw.Dispose()

    and FixedStreamDeserializer(stream : Stream, ?fixedType, ?leaveOpen) =
        let br = new BinaryReader(stream, Encoding.UTF8, defaultArg leaveOpen true)
        let formatter = fixedType |> Option.map FsCoreSerializer.ResolveFormatter

        member __.Deserialize(?context : obj) =
            let sc = match context with None -> StreamingContext() | Some ctx -> StreamingContext(StreamingContextStates.All, ctx)
            let r = new Reader(br, typeFormatter, FsCoreSerializer.ResolveFormatter, sc)

            match formatter with
            | None -> r.ReadObj ()
            | Some fmt -> r.ReadObj fmt

        interface IDisposable with
            member __.Dispose () = br.Dispose ()



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