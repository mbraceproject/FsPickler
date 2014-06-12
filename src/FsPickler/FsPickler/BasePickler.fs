namespace Nessos.FsPickler

    open System

    open System
    open System.IO
    open System.Collections
    open System.Collections.Generic
    open System.Runtime.Serialization

    open Nessos.FsPickler.Utils
    open Nessos.FsPickler.Hashing
    open Nessos.FsPickler.TypeCache
    open Nessos.FsPickler.RootObjectSerialization

    type internal OAttribute = System.Runtime.InteropServices.OptionalAttribute
    type internal DAttribute = System.Runtime.InteropServices.DefaultParameterValueAttribute

    /// <summary>
    ///     The base class for the public pickler API. Provides basic serialization functionality
    /// </summary>
    [<AbstractClass>]
    type BasePickler (formatProvider : IPickleFormatProvider, [<O;D(null)>]?typeConverter) =

        let resolver = PicklerCache.Instance :> IPicklerResolver
        let reflectionCache = ReflectionCache.Create(?tyConv = typeConverter)

        member internal __.Resolver = resolver
        member internal __.ReflectionCache = reflectionCache
        member __.PickleFormat = formatProvider.Name

        /// <summary>Serialize value to the underlying stream.</summary>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        member __.Serialize<'T>(stream : Stream, value : 'T, [<O;D(null)>]?streamingContext, 
                                                [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : unit =

            let pickler = resolver.Resolve<'T> ()
            use writer = initStreamWriter formatProvider stream encoding leaveOpen
            writeRootObject resolver reflectionCache writer streamingContext pickler value

        /// <summary>Serialize value to the underlying stream using given pickler.</summary>
        /// <param name="pickler">pickler used for serialization.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        member __.Serialize<'T>(pickler : Pickler<'T>, stream : Stream, value : 'T, 
                                    [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : unit =

            use writer = initStreamWriter formatProvider stream encoding leaveOpen
            writeRootObject resolver reflectionCache writer streamingContext pickler value

        /// <summary>Serialize object of given type to the underlying stream.</summary>
        /// <param name="valueType">type of the given object.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        member __.Serialize(valueType : Type, stream : Stream, value : obj, 
                                    [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : unit =

            let pickler = resolver.Resolve valueType
            use writer = initStreamWriter formatProvider stream encoding leaveOpen
            writeRootObjectUntyped resolver reflectionCache writer streamingContext pickler value

        /// <summary>Serialize object to the underlying stream using given pickler.</summary>
        /// <param name="pickler">untyped pickler used for serialization.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        member __.Serialize(pickler : Pickler, stream : Stream, value : obj, 
                                    [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : unit =

            use writer = initStreamWriter formatProvider stream encoding leaveOpen
            writeRootObjectUntyped resolver reflectionCache writer streamingContext pickler value

        /// <summary>Deserialize value of given type from the underlying stream.</summary>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        member __.Deserialize<'T> (stream : Stream, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : 'T =

            let pickler = resolver.Resolve<'T> ()
            use reader = initStreamReader formatProvider stream encoding leaveOpen
            readRootObject resolver reflectionCache reader streamingContext pickler

        /// <summary>Deserialize value of given type from the underlying stream, using given pickler.</summary>
        /// <param name="pickler">pickler used for serialization.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        member __.Deserialize<'T> (pickler : Pickler<'T>, stream : Stream, 
                                            [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : 'T =

            use reader = initStreamReader formatProvider stream encoding leaveOpen
            readRootObject resolver reflectionCache reader streamingContext pickler

        /// <summary>Deserialize object of given type from the underlying stream.</summary>
        /// <param name="valueType">anticipated value type.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        member __.Deserialize (valueType : Type, stream : Stream, 
                                    [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : obj =

            let pickler = resolver.Resolve valueType
            use reader = initStreamReader formatProvider stream encoding leaveOpen
            readRootObjectUntyped resolver reflectionCache reader streamingContext pickler

        /// <summary>Deserialize object from the underlying stream using given pickler.</summary>
        /// <param name="pickler">untyped pickler used for deserialization.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        /// <return>number of elements written to the stream.</return>
        member __.Deserialize (pickler : Pickler, stream : Stream, 
                                    [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : obj =

            use reader = initStreamReader formatProvider stream encoding leaveOpen
            readRootObjectUntyped resolver reflectionCache reader streamingContext pickler

        /// <summary>Serialize a sequence of objects to the underlying stream.</summary>
        /// <param name="stream">target stream.</param>
        /// <param name="sequence">input sequence.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        /// <return>number of elements written to the stream.</return>
        member __.SerializeSequence<'T>(stream : Stream, sequence:seq<'T>, 
                                            [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : int =

            let pickler = resolver.Resolve<'T> ()
            use writer = initStreamWriter formatProvider stream encoding leaveOpen
            writeTopLevelSequence resolver reflectionCache writer streamingContext pickler sequence

        /// <summary>Serialize a sequence of objects to the underlying stream.</summary>
        /// <param name="elementType">element type used in sequence.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="sequence">input sequence.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        /// <return>number of elements written to the stream.</return>
        member __.SerializeSequence(elementType : Type, stream : Stream, sequence : IEnumerable, 
                                            [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : int =

            let pickler = resolver.Resolve elementType
            use writer = initStreamWriter formatProvider stream encoding leaveOpen
            writeTopLevelSequenceUntyped resolver reflectionCache writer streamingContext pickler sequence

        /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
        /// <param name="stream">source stream.</param>
        /// <param name="length">number of elements to be deserialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
        member __.DeserializeSequence<'T>(stream : Stream, length : int, 
                                            [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : seq<'T> =

            let pickler = resolver.Resolve<'T> ()
            let reader = initStreamReader formatProvider stream encoding leaveOpen
            readTopLevelSequence resolver reflectionCache reader streamingContext pickler

        /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
        /// <param name="elementType">element type used in sequence.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
        member __.DeserializeSequence(elementType : Type, stream : Stream,
                                            [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : IEnumerable =

            let pickler = resolver.Resolve elementType
            let reader = initStreamReader formatProvider stream encoding leaveOpen
            readTopLevelSequenceUntyped resolver reflectionCache reader streamingContext pickler

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="pickler">Pickler to use.</param>
        /// <param name="value">Value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member bp.Pickle (pickler : Pickler<'T>, value : 'T, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : byte [] =
            pickleBinary (fun m v -> bp.Serialize(pickler, m, v, ?streamingContext = streamingContext, ?encoding = encoding)) value

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="value">Value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member bp.Pickle (value : 'T, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : byte [] =
            pickleBinary (fun m v -> bp.Serialize(m, v, ?streamingContext = streamingContext, ?encoding = encoding)) value

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="pickler">pickler to use.</param>
        /// <param name="value">value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member bp.Pickle (pickler : Pickler, value : obj, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : byte [] =
            pickleBinary (fun m v -> bp.Serialize(pickler, m, v, ?streamingContext = streamingContext, ?encoding = encoding)) value

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="valueType">type of pickled value.</param>
        /// <param name="value">value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member bp.Pickle (valueType : Type, value : obj, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : byte [] =
            pickleBinary (fun m v -> bp.Serialize(valueType, m, v, ?streamingContext = streamingContext, ?encoding = encoding)) value

        /// <summary>
        ///     Unpickles value using given pickler.
        /// </summary>
        /// <param name="pickler">Pickler to use.</param>
        /// <param name="data">Pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member bp.UnPickle (pickler : Pickler<'T>, data : byte [], [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : 'T =
            unpickleBinary (fun m -> bp.Deserialize(pickler, m, ?streamingContext = streamingContext, ?encoding = encoding)) data

        /// <summary>
        ///     Unpickle value to given type.
        /// </summary>
        /// <param name="pickle">pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        member bp.UnPickle<'T> (pickle : byte [], [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : 'T =
            unpickleBinary (fun m -> bp.Deserialize<'T>(m, ?streamingContext = streamingContext, ?encoding = encoding)) pickle

        /// <summary>
        ///     Unpickle value to given type.
        /// </summary>
        /// <param name="valueType">type of pickled value.</param>
        /// <param name="pickle">pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        member bp.UnPickle (valueType : Type, pickle : byte [], [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : obj =
            unpickleBinary (fun m -> bp.Deserialize(valueType, m, ?streamingContext = streamingContext, ?encoding = encoding)) pickle

        /// <summary>
        ///     Unpickle value to given type.
        /// </summary>
        /// <param name="pickler">pickler to use.</param>
        /// <param name="pickle">pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        member bp.UnPickle (pickler : Pickler, pickle : byte [], [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : obj =
            unpickleBinary (fun m -> bp.Deserialize(pickler, m, ?streamingContext = streamingContext, ?encoding = encoding)) pickle


        /// <summary>Compute size and hashcode for given input.</summary>
        /// <param name="value">input value.</param>
        /// <param name="pickler">use specific pickler for hash computation.</param>
        /// <param name="hashFactory">the hashing algorithm to be used. MurMur3 by default.</param>
        member bp.ComputeHash<'T>(value : 'T, [<O;D(null)>] ?pickler : Pickler<'T>, [<O;D(null)>] ?hashFactory : IHashStreamFactory) =
            let hashStream = 
                match hashFactory with 
                | Some h -> h.Create()
                | None -> new MurMur3Stream() :> HashStream

            match pickler with
            | None -> bp.Serialize(hashStream, value)
            | Some p -> bp.Serialize(p, hashStream, value)

            {
                Algorithm = hashStream.HashAlgorithm
                Length = hashStream.Length
                Hash = hashStream.ComputeHash()
            }

        /// <summary>Compute size in bytes for given input.</summary>
        /// <param name="value">input value.</param>
        member bp.ComputeSize<'T>(value : 'T) =
            let lengthCounter = new LengthCounter()
            bp.Serialize(lengthCounter, value)
            lengthCounter.Length

        /// <summary>
        ///     Visits all reference types that appear in the given object graph.
        /// </summary>
        /// <param name="visitor">Visitor implementation.</param>
        /// <param name="graph">Object graph.</param>
        member bp.VisitObject(visitor : IObjectVisitor, graph : 'T) : unit =
            let pickler = resolver.Resolve<'T> ()
            initVisit resolver reflectionCache visitor pickler graph