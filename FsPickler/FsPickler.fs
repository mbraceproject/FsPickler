namespace FsPickler
    
    open System
    open System.IO
    open System.Reflection
    open System.Text
    open System.Collections.Generic
    open System.Collections.Concurrent
    open System.Runtime.Serialization

    open FsPickler
    open FsPickler.Utils
    open FsPickler.TypeShape
    open FsPickler.PicklerUtils
    open FsPickler.BasePicklers
    open FsPickler.FSharpCombinators
    open FsPickler.PicklerResolution

    [<Sealed>]
    type PicklerRegistry () =

        let mutable typeNameConverter = DefaultTypeNameConverter() :> ITypeNameConverter
        let formatters = Atom.atom Map.empty<string, Pickler>
//        let formatterFactories = Atom.atom Map.empty<string, IPicklerFactory>
        let genericFactories = Atom.atom GenericPicklerIndex.Empty

        /// register custom type serialization rules; useful for FSI type serializations
        member __.TypeNameConverter
            with get () = typeNameConverter
            and set tyConv = typeNameConverter <- tyConv

        /// register formatter for a specific type
        member __.RegisterPickler(f : Pickler) =
            formatters.Swap(fun fmts -> fmts.AddNoOverwrite(f.Type.AssemblyQualifiedName, f))
//
//        /// register a formatter factory
//        member __.RegisterPicklerFactory(ff : IPicklerFactory) =
//            formatterFactories.Swap(fun fmtf -> fmtf.AddNoOverwrite(ff.Type.AssemblyQualifiedName, ff))

        /// register generic formatter rules
        member __.RegisterGenericPickler(gf : IGenericPicklerFactory) =
            genericFactories.Swap(fun genericFactories -> genericFactories.AddGenericPickler(gf, Fail))

//        member internal __.PicklerFactories = formatterFactories.Value
        member internal __.GenericFactories = genericFactories.Value

        member __.RegisteredPicklers = formatters.Value |> Map.toSeq |> Seq.map snd |> List.ofSeq
//        member __.RegisteredPicklerFactories = formatterFactories.Value |> Map.toSeq |> Seq.map snd |> List.ofSeq
        member __.RegisteredGenericPicklerFactories = genericFactories.Value.GetEntries()



    and PicklerCache internal (tyConv : ITypeNameConverter, formatters : seq<Pickler>, 
                                    gfi : GenericPicklerIndex) = //, ffs : Map<string, IPicklerFactory>) =
        
        let cache =
            seq {
                yield! mkPrimitivePicklers ()
                yield! mkAtomicPicklers ()
                yield! mkReflectionPicklers tyConv
            }
            |> Seq.map (fun f -> KeyValuePair(f.Type, f)) 
            |> fun x -> new ConcurrentDictionary<_,_>(x)

        let gfi =
            let fsharpGenericPicklers = mkGenericPicklers ()
            gfi.AddGenericPicklers(fsharpGenericPicklers, Discard)

        do 
            for f in formatters do 
                cache.AddOrUpdate(f.Type, f, fun _ _ -> f) |> ignore

        member internal __.TypeNameConverter = tyConv

        interface IPicklerResolver with
            member s.Resolve<'T> () = YParametric cache (resolvePickler tyConv gfi) typeof<'T> :?> Pickler<'T>
            member s.Resolve (t : Type) = YParametric cache (resolvePickler tyConv gfi) t
        
        static member internal FromPicklerRegistry(fr : PicklerRegistry) =
            new PicklerCache(fr.TypeNameConverter, fr.RegisteredPicklers, fr.GenericFactories)

        static member internal Default() = 
            new PicklerCache(new DefaultTypeNameConverter(), [], GenericPicklerIndex.Empty)


    and FsPickler private (cache : IPicklerResolver) =

        new () = new FsPickler(PicklerCache.Default())
        new (registry : PicklerRegistry) = new FsPickler(PicklerCache.FromPicklerRegistry registry)

        /// <summary>Initializes an object writer for the given stream.</summary>
        /// <param name="stream">The target stream.</param>
        /// <param name="context">The optional streaming context object.</param>
        /// <param name="leaveOpen">Leave the stream open when finished. Defaults to true.</param>
        /// <param name="encoding">The encoding used to in the binary writer. Defaults to UTF8.</param>
        member s.GetObjectWriter(stream : Stream, ?context : obj, ?leaveOpen, ?encoding) =
            if not stream.CanWrite then invalidOp "Cannot write to stream."
            let sc = match context with None -> StreamingContext() | Some ctx -> StreamingContext(StreamingContextStates.All, ctx)
            new Writer(stream, cache, sc, ?leaveOpen = leaveOpen, ?encoding = encoding)

        /// <summary>Initializes an object reader for given the stream.</summary>
        /// <param name="stream">The source stream.</param>
        /// <param name="context">The optional streaming context object.</param>
        /// <param name="leaveOpen">Leave the stream open when finished. Defaults to true.</param>
        /// <param name="encoding">The encoding used to in the binary writer. Defaults to UTF8.</param>
        member __.GetObjectReader(stream : Stream, ?context : obj, ?leaveOpen, ?encoding) =
            if not stream.CanRead then invalidOp "Cannot read from stream."
            let sc = match context with None -> StreamingContext() | Some ctx -> StreamingContext(StreamingContextStates.All, ctx)
            new Reader(stream, cache, sc, ?leaveOpen = leaveOpen, ?encoding = encoding)

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
            try cache.Resolve t |> ignore ; true
            with :? NonSerializableTypeException -> false

        member __.IsSerializableType<'T> () =
            try cache.Resolve<'T> () |> ignore ; true
            with :? NonSerializableTypeException -> false

        member __.ResolvePickler<'T> () = cache.Resolve<'T> ()
