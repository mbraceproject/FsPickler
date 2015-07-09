namespace Nessos.FsPickler

open System

open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Runtime.Serialization
open System.Text

open Nessos.FsPickler.Utils
open Nessos.FsPickler.Hashing
open Nessos.FsPickler.ReflectionCache
open Nessos.FsPickler.RootSerialization

type internal OAttribute = System.Runtime.InteropServices.OptionalAttribute
type internal DAttribute = System.Runtime.InteropServices.DefaultParameterValueAttribute

/// <summary>
///     An abstract class containg the basic serialization API.
/// </summary>
[<AbstractClass>]
type FsPicklerSerializer (formatProvider : IPickleFormatProvider, [<O;D(null)>]?typeConverter : ITypeNameConverter) =

    let resolver = PicklerCache.Instance :> IPicklerResolver
    let reflectionCache = ReflectionCache.Create(?tyConv = typeConverter)

    member internal __.Resolver = resolver
    member internal __.ReflectionCache = reflectionCache

    /// <summary>
    ///     Description of the pickle format used by the serializer.
    /// </summary>
    member __.PickleFormat = formatProvider.Name

    //
    //  Typed API
    //

    /// <summary>Serialize value to the underlying stream.</summary>
    /// <param name="stream">Target write stream.</param>
    /// <param name="value">Value to be serialized.</param>
    /// <param name="pickler">Pickler used for serialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">Text encoding used by the serializer.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    member __.Serialize<'T>(stream : Stream, value : 'T, 
                                [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext,
                                [<O;D(null)>]?encoding : Encoding, [<O;D(null)>]?leaveOpen : bool) : unit =

        let pickler = match pickler with None -> resolver.Resolve<'T> () | Some p -> p
        use writer = initStreamWriter formatProvider stream encoding false leaveOpen
        let _ = writeRootObject resolver reflectionCache writer streamingContext None pickler value
        ()

    /// <summary>Deserialize value of given type from the underlying stream.</summary>
    /// <param name="stream">Source read stream.</param>
    /// <param name="pickler">Pickler used for deserialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for deserialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">Text encoding used by the deserializer.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    member __.Deserialize<'T> (stream : Stream, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext, 
                                                [<O;D(null)>]?encoding : Encoding, [<O;D(null)>]?leaveOpen : bool) : 'T =

        let pickler = match pickler with None -> resolver.Resolve<'T> () | Some p -> p
        use reader = initStreamReader formatProvider stream encoding false leaveOpen
        readRootObject resolver reflectionCache reader streamingContext None pickler

    /// <summary>Serialize a sequence of objects to the underlying stream.</summary>
    /// <param name="stream">Target write stream.</param>
    /// <param name="sequence">Input sequence to be evaluated and serialized.</param>
    /// <param name="pickler">Pickler used for serialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">Text encoding used by the serializer.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    /// <return>Number of elements written to the stream.</return>
    member __.SerializeSequence<'T>(stream : Stream, sequence:seq<'T>, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext, 
                                                                        [<O;D(null)>]?encoding : Encoding, [<O;D(null)>]?leaveOpen : bool) : int =

        let pickler = match pickler with None -> resolver.Resolve<'T> () | Some p -> p
        use writer = initStreamWriter formatProvider stream encoding true leaveOpen
        writeTopLevelSequence resolver reflectionCache writer streamingContext pickler sequence


    /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
    /// <param name="stream">Source read stream.</param>
    /// <param name="pickler">Pickler used for element deserialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for deserialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">Text encoding used by the deserializer.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    /// <returns>An IEnumerable that lazily consumes elements from the stream.</returns>
    member __.DeserializeSequence<'T>(stream : Stream, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext, 
                                                        [<O;D(null)>]?encoding : Encoding, [<O;D(null)>]?leaveOpen : bool) : seq<'T> =

        let pickler = match pickler with None -> resolver.Resolve<'T>() | Some p -> p
        let reader = initStreamReader formatProvider stream encoding true leaveOpen
        readTopLevelSequence resolver reflectionCache reader streamingContext pickler

    /// <summary>
    ///     Serializes a value to stream, excluding objects mandated by the provided IObjectSifter instance.
    ///     Values excluded from serialization will be returned tagged by their ids. 
    ////    Sifted objects will have to be provided on deserialization along with their accompanying id's.
    /// </summary>
    /// <param name="stream">Target write stream.</param>
    /// <param name="value">Value to be serialized.</param>
    /// <param name="sifter">User supplied sifter implementation. Used to specify which nodes in the object graph are to be excluded from serialization.</param>
    /// <param name="pickler">Pickler used for element deserialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">Text encoding used by the serializer.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    /// <returns>Sifted values along with their graph ids.</returns>
    member __.SerializeSifted<'T>(stream : Stream, value:'T, sifter : IObjectSifter, 
                                                    [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext, 
                                                    [<O;D(null)>]?encoding : Encoding, [<O;D(null)>]?leaveOpen : bool) : (int64 * obj) [] =

        let pickler = match pickler with None -> resolver.Resolve<'T>() | Some p -> p
        use writer = initStreamWriter formatProvider stream encoding false leaveOpen
        let state = writeRootObject resolver reflectionCache writer streamingContext (Some sifter) pickler value
        state.Sifted.ToArray()

    /// <summary>
    ///     Deserializes a sifted value from stream, filling in sifted holes from the serialized using supplied objects.
    /// </summary>
    /// <param name="stream">Source read stream.</param>
    /// <param name="sifted">Object-id pairs used for filling sifted holes in serialization.s</param>
    /// <param name="pickler">Pickler used for element deserialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">Text encoding used by the serializer.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    member __.DeserializeSifted<'T>(stream : Stream, sifted : (int64 * obj) [],
                                                    [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext, 
                                                    [<O;D(null)>]?encoding : Encoding, [<O;D(null)>]?leaveOpen : bool) : 'T =

        let pickler = match pickler with None -> resolver.Resolve<'T> () | Some p -> p
        use reader = initStreamReader formatProvider stream encoding false leaveOpen
        readRootObject resolver reflectionCache reader streamingContext (Some sifted) pickler

    /// <summary>
    ///     Pickles given value to byte array.
    /// </summary>
    /// <param name="value">Value to pickle.</param>
    /// <param name="pickler">Pickler used for element serialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">Text encoding used by the serializer.</param>
    member bp.Pickle<'T>(value : 'T, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext, [<O;D(null)>]?encoding : Encoding) : byte [] =
        pickleBinary (fun m v -> bp.Serialize(m, v, ?pickler = pickler, ?streamingContext = streamingContext, ?encoding = encoding)) value


    /// <summary>
    ///     Unpickles value using given pickler.
    /// </summary>
    /// <param name="data">Pickle to deserialize.</param>
    /// <param name="pickler">Pickler used for element serialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for deserialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">Text encoding used by the deserializer.</param>
    member bp.UnPickle<'T> (data : byte [], [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext, [<O;D(null)>]?encoding : Encoding) : 'T =
        unpickleBinary (fun m -> bp.Deserialize(m, ?pickler = pickler, ?streamingContext = streamingContext, ?encoding = encoding)) data


    /// <summary>
    ///     Pickles value to bytes, excluding objects mandated by the provided IObjectSifter instance.
    ///     Values excluded from serialization will be returned tagged by their ids. 
    ////    Sifted objects will have to be provided on deserialization along with their accompanying id's.
    /// </summary>
    /// <param name="value">Value to be serialized.</param>
    /// <param name="sifter">User supplied sifter implementation. Used to specify which nodes in the object graph are to be excluded from serialization.</param>
    /// <param name="pickler">Pickler used for element deserialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">Text encoding used by the serializer.</param>
    /// <returns>Pickled value along with sifted values along with their graph ids.</returns>
    member bp.PickleSifted<'T>(value:'T, sifter : IObjectSifter, 
                                                    [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext, 
                                                    [<O;D(null)>]?encoding : Encoding) : byte [] * (int64 * obj) [] =
        use m = new MemoryStream()
        let sifted = bp.SerializeSifted(m, value, sifter, ?pickler = pickler, ?streamingContext = streamingContext, ?encoding = encoding)
        m.ToArray(), sifted

    /// <summary>
    ///     Unpickles a sifted value, filling in sifted holes from the serialized using supplied objects.
    /// </summary>
    /// <param name="data">Pickle to deserialize.</param>
    /// <param name="sifted">Object-id pairs used for filling sifted holes in serialization.</param>
    /// <param name="pickler">Pickler used for element deserialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">Text encoding used by the serializer.</param>
    member bp.UnPickleSifted<'T>(pickle : byte[], sifted : (int64 * obj) [],
                                                    [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext, 
                                                    [<O;D(null)>]?encoding : Encoding) : 'T =
        use m = new MemoryStream(pickle)
        bp.DeserializeSifted<'T>(m, sifted, ?pickler = pickler, ?streamingContext = streamingContext, ?encoding = encoding)


    //
    //  Untyped API
    //


    /// <summary>Serialize untyped object to the underlying stream with provided pickler.</summary>
    /// <param name="stream">Target write stream.</param>
    /// <param name="value">Value to be serialized.</param>
    /// <param name="pickler">Untyped pickler used for serialization. Its type should be compatible with that of the supplied object.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">Text encoding used by the serializer.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    member __.SerializeUntyped(stream : Stream, value : obj, pickler : Pickler, [<O;D(null)>]?streamingContext : StreamingContext, 
                                                [<O;D(null)>]?encoding : Encoding, [<O;D(null)>]?leaveOpen : bool) : unit =
        use writer = initStreamWriter formatProvider stream encoding false leaveOpen
        let _ = writeRootObjectUntyped resolver reflectionCache writer streamingContext None pickler value
        ()

    /// <summary>Deserialize untyped object from the underlying stream with provided pickler.</summary>
    /// <param name="stream">Source read stream.</param>
    /// <param name="pickler">Pickler used for deserialization. Its type should be compatible with that of the supplied object.</param>
    /// <param name="streamingContext">Streaming context for deserialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">Text encoding used by the deserializer.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    member __.DeserializeUntyped(stream : Stream, pickler : Pickler, [<O;D(null)>]?streamingContext : StreamingContext, 
                                                                [<O;D(null)>]?encoding : Encoding, [<O;D(null)>]?leaveOpen : bool) : obj =

        use reader = initStreamReader formatProvider stream encoding false leaveOpen
        readRootObjectUntyped resolver reflectionCache reader streamingContext None pickler

    /// <summary>Serialize an untyped sequence of objects to the underlying stream.</summary>
    /// <param name="elementType">element type used in sequence.</param>
    /// <param name="stream">Target write stream.</param>
    /// <param name="sequence">Input sequence to be evaluated and serialized.</param>
    /// <param name="pickler">Pickler used for element serialization. Its type should be compatible with that of the supplied sequence.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">Text encoding used by the serializer.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    /// <return>Number of elements written to the stream.</return>
    member __.SerializeSequenceUntyped(stream : Stream, sequence : IEnumerable, pickler : Pickler, 
                                        [<O;D(null)>]?streamingContext : StreamingContext, [<O;D(null)>]?encoding : Encoding, [<O;D(null)>]?leaveOpen : bool) : int =

        use writer = initStreamWriter formatProvider stream encoding true leaveOpen
        writeTopLevelSequenceUntyped resolver reflectionCache writer streamingContext pickler sequence

    /// <summary>Lazily deserialize an untyped sequence of objects from the underlying stream.</summary>
    /// <param name="stream">source stream.</param>
    /// <param name="pickler">Pickler used for element deserialization. Its type should be compatible with that of the supplied sequence.</param>
    /// <param name="streamingContext">Streaming context for deserialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">Text encoding used by the deserializer.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    /// <returns>An IEnumerable that lazily consumes elements from the stream.</returns>
    member __.DeserializeSequenceUntyped(stream : Stream, pickler : Pickler, [<O;D(null)>]?streamingContext : StreamingContext, 
                                            [<O;D(null)>]?encoding : Encoding, [<O;D(null)>]?leaveOpen : bool) : IEnumerable =

        let reader = initStreamReader formatProvider stream encoding true leaveOpen
        readTopLevelSequenceUntyped resolver reflectionCache reader streamingContext pickler

    /// <summary>
    ///     Pickles given value to byte array.
    /// </summary>
    /// <param name="value">Value to pickle.</param>
    /// <param name="pickler">Pickler used for value serialization. Its type should be compatible with that of the supplied value.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">Text encoding used by the serializer.</param>
    member bp.PickleUntyped (value : obj, pickler : Pickler, [<O;D(null)>]?streamingContext : StreamingContext, [<O;D(null)>]?encoding : Encoding) : byte [] =
        pickleBinary (fun m v -> bp.SerializeUntyped(m, v, pickler, ?streamingContext = streamingContext, ?encoding = encoding)) value


    /// <summary>
    ///     Unpickle value to given type.
    /// </summary>
    /// <param name="pickle">Byte array to unpickler.</param>
    /// <param name="pickler">Pickler used for value serialization. Its type should be compatible with that of the supplied pickle.</param>
    /// <param name="streamingContext">Streaming context for deserialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">Text encoding used by the deserializer.</param>
    member bp.UnPickleUntyped (pickle : byte [], pickler : Pickler, [<O;D(null)>]?streamingContext : StreamingContext, [<O;D(null)>]?encoding : Encoding) : obj =
        unpickleBinary (fun m -> bp.DeserializeUntyped(m, pickler, ?streamingContext = streamingContext, ?encoding = encoding)) pickle

    
    //
    //  Misc tools
    //

    /// <summary>Compute size and hashcode for given input.</summary>
    /// <param name="value">input value.</param>
    /// <param name="hashFactory">the hashing algorithm to be used. MurMur3 by default.</param>
    member bp.ComputeHash<'T>(value : 'T, [<O;D(null)>] ?hashFactory : IHashStreamFactory) =
        let hashStream = 
            match hashFactory with 
            | Some h -> h.Create()
            | None -> new MurMur3Stream() :> HashStream

        let signature = reflectionCache.GetTypeSignature (if obj.ReferenceEquals(value,null) then typeof<obj> else value.GetType())
        bp.Serialize<obj>(hashStream, value)

        {
            Algorithm = hashStream.HashAlgorithm
            Type = signature
            Length = hashStream.Length
            Hash = hashStream.ComputeHash()
        }

    /// <summary>Compute size in bytes for given input.</summary>
    /// <param name="value">input value.</param>
    /// <param name="pickler">Pickler to be used for size computation. Defaults to auto-generated pickler.</param>
    member bp.ComputeSize<'T>(value : 'T, [<O;D(null)>] ?pickler : Pickler<'T>) =
        let lengthCounter = new LengthCounter()
        bp.Serialize(lengthCounter, value)
        lengthCounter.Length