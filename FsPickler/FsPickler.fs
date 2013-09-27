namespace FsPickler
    
    open System
    open System.IO
    open System.Runtime.Serialization


    type FsPickler internal (resolver : IPicklerResolver) =
        
        static let singleton = lazy(new FsPickler())

        new () = FsPickler(PicklerCache.GetDefault ())
        new (registry : CustomPicklerRegistry) = new FsPickler(PicklerCache.FromPicklerRegistry registry)

        /// <summary>Serialize an object of given type to the underlying stream.</summary>
        /// <param name="stream">The target stream.</param>
        /// <param name="context">The untyped parameter passed to the streaming context.</param>
        /// <param name="encoding">The encoding passed to the binary writer.</param>
        ///     Useful when serializing sequences of small objects.</param>
        member __.Serialize<'T>(stream : Stream, value : 'T, ?streamingContext : obj, ?encoding, ?leaveOpen) : unit =
            use writer = new Writer(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)

            writer.Write<'T> value

        /// <summary>Deserialize object of given type from the underlying stream.</summary>
        /// <param name="stream">The source stream.</param>
        /// <param name="context">The untyped parameter passed to the streaming context.</param>
        /// <param name="encoding">The encoding passed to the binary reader.</param>
        ///     Useful when serializing sequences of small objects.</param>
        member __.Deserialize<'T> (stream : Stream, ?streamingContext : obj, ?encoding, ?leaveOpen) : 'T =
            use reader = new Reader(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)
            
            // TODO : type header
            reader.Read<'T> ()

        /// <summary>Deserialize object of given type from the underlying stream.</summary>
        /// <param name="stream">The source stream.</param>
        /// <param name="graphType">The given graph type.</param>
        /// <param name="context">The untyped parameter passed to the streaming context.</param>
        /// <param name="encoding">The encoding passed to the binary reader.</param>
        ///     Useful when serializing sequences of small objects.</param>
        member __.Serialize(stream : Stream, graphType : Type, graph : obj, ?streamingContext : obj, ?encoding, ?leaveOpen) : unit =
            use writer = new Writer(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)

            writer.WriteObj(graphType, graph)

        /// <summary>Deserialize object of given type from the underlying stream.</summary>
        /// <param name="stream">The source stream.</param>
        /// <param name="graphType">The given graph type.</param>
        /// <param name="context">The untyped parameter passed to the streaming context.</param>
        /// <param name="encoding">The encoding passed to the binary reader.</param>
        ///     Useful when serializing sequences of small objects.</param>
        member __.Deserialize (stream : Stream, graphType : Type, ?streamingContext : obj, ?encoding, ?leaveOpen) : obj =
            use reader = new Reader(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)
            
            reader.ReadObj graphType

        member __.Pickle (pickler : Pickler<'T>) (value : 'T) =
            use mem = new MemoryStream()
            use writer = new Writer(mem, resolver)
            writer.Write(pickler, value)
            mem.ToArray()

        member __.UnPickle (pickler : Pickler<'T>) (data : byte []) =
            use mem = new MemoryStream(data)
            use reader = new Reader(mem, resolver)
            reader.Read pickler

        member __.IsSerializableType (t : Type) =
            try resolver.Resolve t |> ignore ; true
            with :? NonSerializableTypeException -> false

        member __.IsSerializableType<'T> () =
            try resolver.Resolve<'T> () |> ignore ; true
            with :? NonSerializableTypeException -> false

        member __.ResolvePickler<'T> () = resolver.Resolve<'T> ()

        static member internal Singleton = singleton.Value