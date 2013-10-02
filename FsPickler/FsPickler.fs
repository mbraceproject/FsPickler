namespace FsPickler
    
    open System
    open System.IO
    open System.Runtime.Serialization


    type FsPickler private (resolver : IPicklerResolver) =
        
        /// initializes an instance that resolves picklers from a global cache
        new () = new FsPickler(PicklerCache.GetDefaultInstance())
        /// initializes a new pickler cache that resolves picklers using custom rules
        new (registry : CustomPicklerRegistry) = new FsPickler(PicklerCache.FromPicklerRegistry registry)

        /// Identifier of the cache instance used by the serializer.
        member __.CacheID = resolver.Id

        /// <summary>Serialize value to the underlying stream.</summary>
        /// <param name="stream">The target stream.</param>
        /// <param name="value">The value to be serialized.</param>
        /// <param name="streamingContext">The untyped parameter passed to the streaming context.</param>
        /// <param name="encoding">The encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize<'T>(stream : Stream, value : 'T, ?streamingContext : obj, ?encoding, ?leaveOpen) : unit =
            use writer = new Writer(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)
            writer.Write<'T> value

        /// <summary>Serialize value to the underlying stream using given pickler.</summary>
        /// <param name="pickler">The pickler used for serialization.</param>
        /// <param name="stream">The target stream.</param>
        /// <param name="value">The value to be serialized.</param>
        /// <param name="streamingContext">The untyped parameter passed to the streaming context.</param>
        /// <param name="encoding">The encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize<'T>(pickler : Pickler<'T>, stream : Stream, value : 'T, ?streamingContext : obj, ?encoding, ?leaveOpen) : unit =
            use writer = new Writer(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)
            writer.Write(pickler, value)

        /// <summary>Serialize object of given type to the underlying stream.</summary>
        /// <param name="valueType">The type of the given object.</param>
        /// <param name="stream">The target stream.</param>
        /// <param name="value">The value to be serialized.</param>
        /// <param name="streamingContext">The untyped parameter passed to the streaming context.</param>
        /// <param name="encoding">The encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize(valueType : Type, stream : Stream, value : obj, ?streamingContext : obj, ?encoding, ?leaveOpen) : unit =
            use writer = new Writer(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)
            writer.WriteObj(valueType, value)

        /// <summary>Deserialize value of given type from the underlying stream.</summary>
        /// <param name="stream">The source stream.</param>
        /// <param name="streamingContext">The untyped parameter passed to the streaming context.</param>
        /// <param name="encoding">The encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Deserialize<'T> (stream : Stream, ?streamingContext : obj, ?encoding, ?leaveOpen) : 'T =
            use reader = new Reader(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)
            reader.Read<'T> ()

        /// <summary>Deserialize value of given type from the underlying stream, using given pickler.</summary>
        /// <param name="pickler">The pickler used for serialization.</param>
        /// <param name="stream">The source stream.</param>
        /// <param name="streamingContext">The untyped parameter passed to the streaming context.</param>
        /// <param name="encoding">The encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Deserialize<'T> (pickler : Pickler<'T>, stream : Stream, ?streamingContext : obj, ?encoding, ?leaveOpen) : 'T =
            use reader = new Reader(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)
            reader.Read<'T> pickler

        /// <summary>Deserialize object of given type from the underlying stream.</summary>
        /// <param name="valueType">The anticipated value type.</param>
        /// <param name="stream">The source stream.</param>
        /// <param name="streamingContext">The untyped parameter passed to the streaming context.</param>
        /// <param name="encoding">The encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Deserialize (valueType : Type, stream : Stream, ?streamingContext : obj, ?encoding, ?leaveOpen) : obj =
            use reader = new Reader(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)
            reader.ReadObj valueType

        /// Auto generates a pickler for given type variable
        member __.GeneratePickler<'T> () = resolver.Resolve<'T> ()

        /// Decides if given type is serializable by FsPickler
        member __.IsSerializableType (t : Type) =
            try resolver.Resolve t |> ignore ; true
            with :? NonSerializableTypeException -> false

        /// Decides if given type is serializable by FsPickler
        member __.IsSerializableType<'T> () =
            try resolver.Resolve<'T> () |> ignore ; true
            with :? NonSerializableTypeException -> false