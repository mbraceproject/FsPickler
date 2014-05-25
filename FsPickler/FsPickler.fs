namespace Nessos.FsPickler
    
    open System
    open System.IO
    open System.Collections
    open System.Collections.Generic
    open System.Runtime.Serialization

    open Nessos.FsPickler.Utils
    open Nessos.FsPickler.Hashing
    open Nessos.FsPickler.PicklerUtils

    type private OAttribute = System.Runtime.InteropServices.OptionalAttribute
    type private DAttribute = System.Runtime.InteropServices.DefaultParameterValueAttribute

    [<Sealed>]
    [<AutoSerializableAttribute(false)>]
    type FsPickler private (cache : PicklerCache) =

        let resolver = cache :> IPicklerResolver

        static let formatP = new XmlPickleFormatProvider(System.Text.Encoding.UTF8, indent = true)
//        static let formatP = new BinaryFormatProvider()
        
        /// initializes an instance that resolves picklers from a global cache
        new () = new FsPickler(PicklerCache.GetDefaultInstance())
        /// initializes a new pickler cache that resolves picklers using custom rules
        new (registry : CustomPicklerRegistry) = new FsPickler(PicklerCache.FromPicklerRegistry registry)

        /// Name for the pickler cache
        member __.Name = cache.Name
        /// Identifier of the cache instance used by the serializer.
        member __.UUId = cache.UUId

        /// <summary>Serialize value to the underlying stream.</summary>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize<'T>(stream : Stream, value : 'T, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : unit =
            use state = new WriteState(stream, formatP, resolver, ?streamingContext = streamingContext)
            let pickler = resolver.Resolve<'T> ()
            let qualifiedName = cache.GetQualifiedName pickler.Type
            state.Formatter.BeginWriteRoot qualifiedName
            pickler.Write state "object" value
            state.Formatter.EndWriteRoot()

        /// <summary>Serialize value to the underlying stream using given pickler.</summary>
        /// <param name="pickler">pickler used for serialization.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize<'T>(pickler : Pickler<'T>, stream : Stream, value : 'T, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : unit =
            do checkPicklerCompat cache.UUId pickler
            use state = new WriteState(stream, formatP, resolver, ?streamingContext = streamingContext) //, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let qualifiedName = cache.GetQualifiedName pickler.Type
            state.Formatter.BeginWriteRoot qualifiedName
            pickler.Write state "object" value
            state.Formatter.EndWriteRoot()

        /// <summary>Serialize object of given type to the underlying stream.</summary>
        /// <param name="valueType">type of the given object.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize(valueType : Type, stream : Stream, value : obj, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : unit =
            use state = new WriteState(stream, formatP, resolver, ?streamingContext = streamingContext) //, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let pickler = resolver.Resolve valueType
            let qualifiedName = cache.GetQualifiedName valueType
            state.Formatter.BeginWriteRoot qualifiedName
            pickler.UntypedWrite state "object" value
            state.Formatter.EndWriteRoot ()

        /// <summary>Serialize object to the underlying stream using given pickler.</summary>
        /// <param name="pickler">untyped pickler used for serialization.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize(pickler : Pickler, stream : Stream, value : obj, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : unit =
            use state = new WriteState(stream, formatP, resolver, ?streamingContext = streamingContext) //, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let qualifiedName = cache.GetQualifiedName pickler.Type
            state.Formatter.BeginWriteRoot qualifiedName
            pickler.UntypedWrite state "object" value
            state.Formatter.EndWriteRoot ()

        /// <summary>Deserialize value of given type from the underlying stream.</summary>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        member __.Deserialize<'T> (stream : Stream, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : 'T =
            use state = new ReadState(stream, formatP, resolver, ?streamingContext = streamingContext) //, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let pickler = resolver.Resolve<'T> ()
            let qualifiedName = cache.GetQualifiedName pickler.Type
            state.Formatter.BeginReadRoot qualifiedName
            let v = pickler.Read state "object"
            state.Formatter.EndReadRoot ()
            v

        /// <summary>Deserialize value of given type from the underlying stream, using given pickler.</summary>
        /// <param name="pickler">pickler used for serialization.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext"> streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        member __.Deserialize<'T> (pickler : Pickler<'T>, stream : Stream, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : 'T =
            do checkPicklerCompat cache.UUId pickler
            use state = new ReadState(stream, formatP, resolver, ?streamingContext = streamingContext) //, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let qualifiedName = cache.GetQualifiedName pickler.Type
            state.Formatter.BeginReadRoot qualifiedName
            let v = pickler.Read state "object"
            state.Formatter.EndReadRoot ()
            v

        /// <summary>Deserialize object of given type from the underlying stream.</summary>
        /// <param name="valueType">anticipated value type.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        member __.Deserialize (valueType : Type, stream : Stream, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : obj =
            use state = new ReadState(stream, formatP, resolver, ?streamingContext = streamingContext) //, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let pickler = resolver.Resolve valueType
            let qualifiedName = cache.GetQualifiedName pickler.Type
            state.Formatter.BeginReadRoot qualifiedName
            let v = pickler.UntypedRead state "object"
            state.Formatter.EndReadRoot ()
            v

        /// <summary>Deserialize object from the underlying stream using given pickler.</summary>
        /// <param name="pickler">untyped pickler used for deserialization.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        /// <return>number of elements written to the stream.</return>
        member __.Deserialize (pickler : Pickler, stream : Stream, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : obj =
            use state = new ReadState(stream, formatP, resolver, ?streamingContext = streamingContext)
            let qualifiedName = cache.GetQualifiedName pickler.Type
            state.Formatter.BeginReadRoot qualifiedName
            let v = pickler.UntypedRead state "object"
            state.Formatter.EndReadRoot ()
            v

        /// <summary>Serialize a sequence of objects to the underlying stream.</summary>
        /// <param name="stream">target stream.</param>
        /// <param name="sequence">input sequence.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        member __.SerializeSequence<'T>(stream : Stream, sequence:seq<'T>, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : int =
            use writer = new WriteState(stream, formatP, resolver) //, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let pickler = resolver.Resolve<'T> ()
            writeTopLevelSequence pickler writer "object" sequence

        /// <summary>Serialize a sequence of objects to the underlying stream.</summary>
        /// <param name="elementType">element type used in sequence.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="sequence">input sequence.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        /// <return>number of elements written to the stream.</return>
        member __.SerializeSequence(elementType : Type, stream : Stream, sequence : IEnumerable, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : int =
            use writer = new WriteState(stream, formatP, resolver) //, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let pickler = resolver.Resolve elementType
            writeTopLevelSequenceUntyped pickler writer "object" sequence
//            let qn = cache.GetQualifiedName pickler.Type
//            pickler.WriteSequence(writer, qn, sequence)

        /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
        /// <param name="stream">source stream.</param>
        /// <param name="length">number of elements to be deserialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
        member __.DeserializeSequence<'T>(stream : Stream, length : int, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : IEnumerator<'T> =
            let reader = new ReadState(stream, formatP, resolver) //, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let pickler = resolver.Resolve<'T> ()
            readTopLevelSequence pickler reader "object" length
//            let qn = cache.GetQualifiedName pickler.Type
//            reader.readSequenceNoLength(pickler, qn, length)

        /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
        /// <param name="elementType">element type used in sequence.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="length">number of elements to be deserialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
        member __.DeserializeSequence(elementType : Type, stream : Stream, length : int, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : IEnumerator =
            let reader = new ReadState(stream, formatP, resolver) //, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let pickler = resolver.Resolve elementType
            readTopLevelSequenceUntyped pickler reader "object" length
//            let qn = cache.GetQualifiedName pickler.Type
//            pickler.readSequenceNoLength(reader, qn, length)

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="pickler">Pickler to use.</param>
        /// <param name="value">Value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        member f.Pickle (pickler : Pickler<'T>, value : 'T, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : byte [] =
            pickle (fun m v -> f.Serialize(pickler, m, v, ?streamingContext = streamingContext, ?encoding = encoding)) value

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="value">Value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        member f.Pickle (value : 'T, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : byte [] =
            pickle (fun m v -> f.Serialize(m, v, ?streamingContext = streamingContext, ?encoding = encoding)) value

        /// <summary>
        ///     Unpickles value using given pickler.
        /// </summary>
        /// <param name="pickler">Pickler to use.</param>
        /// <param name="data">Pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        member f.UnPickle (pickler : Pickler<'T>, data : byte [], [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : 'T =
            unpickle (fun m -> f.Deserialize(pickler, m, ?streamingContext = streamingContext, ?encoding = encoding)) data

        /// <summary>
        ///     Unpickle value to given type.
        /// </summary>
        /// <param name="data">Pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        member f.UnPickle<'T> (data : byte [], [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : 'T =
            unpickle (fun m -> f.Deserialize<'T>(m, ?streamingContext = streamingContext, ?encoding = encoding)) data

        /// <summary>Compute size and hashcode for given input.</summary>
        /// <param name="value">input value.</param>
        /// <param name="pickler">use specific pickler for hashcode generation.</param>
        /// <param name="hashFactory">the hashing algorithm to be used. MurMur3 by default</param>
        member f.ComputeHash<'T>(value : 'T, [<O;D(null)>] ?pickler : Pickler<'T>, [<O;D(null)>] ?hashFactory : IHashStreamFactory) =
            let hashStream = 
                match hashFactory with 
                | Some h -> h.Create()
                | None -> new MurMur3Stream() :> HashStream

            match pickler with
            | None -> f.Serialize(hashStream, value)
            | Some p -> f.Serialize(p, hashStream, value)

            {
                Algorithm = hashStream.HashAlgorithm
                Length = hashStream.Length
                Hash = hashStream.ComputeHash()
            }

        /// <summary>Compute size in bytes for given input.</summary>
        /// <param name="pickler">use specific pickler for length computation.</param>
        /// <param name="value">input value.</param>
        member f.ComputeSize<'T>(value : 'T, [<O;D(null)>]?pickler : Pickler<'T>) =
            let lengthCounter = new LengthCounter()
            match pickler with
            | None -> f.Serialize(lengthCounter, value)
            | Some p -> f.Serialize(p, lengthCounter, value)
            lengthCounter.Length

        /// Auto generates a pickler for given type variable
        member __.GeneratePickler<'T> () = resolver.Resolve<'T> ()
        
        /// Auto generates a pickler for given type
        member __.GeneratePickler (t : Type) = resolver.Resolve t


        /// Decides if given type is serializable by FsPickler
        member __.IsSerializableType (t : Type) =
            try resolver.Resolve t |> ignore ; true
            with :? NonSerializableTypeException -> false

        /// Decides if given type is serializable by FsPickler
        member __.IsSerializableType<'T> () =
            try resolver.Resolve<'T> () |> ignore ; true
            with :? NonSerializableTypeException -> false