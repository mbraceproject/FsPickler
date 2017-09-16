namespace MBrace.FsPickler

open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.Text
open System.Runtime.Serialization

open MBrace.FsPickler.RootSerialization

/// <summary>
///     An abstract class containing the text-based serialization API.
/// </summary>
[<AbstractClass>]
type FsPicklerTextSerializer (formatProvider : ITextPickleFormatProvider, [<O;D(null)>] ?typeConverter : ITypeNameConverter, [<O;D(null)>]?picklerResolver : IPicklerResolver) =
    inherit FsPicklerSerializer(formatProvider, ?typeConverter = typeConverter, ?picklerResolver = picklerResolver)

    let resolver = base.Resolver
    let reflectionCache = base.ReflectionCache

    //
    //  Typed API
    //

    /// <summary>Serialize value to the underlying writer.</summary>
    /// <param name="writer">Target text writer.</param>
    /// <param name="value">Value to be serialized.</param>
    /// <param name="pickler">Pickler used for serialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <param name="leaveOpen">Leave underlying text writer open when finished. Defaults to false.</param>
    member __.Serialize<'T>(writer : TextWriter, value : 'T, [<O;D(null)>]?pickler : Pickler<'T>, 
                                [<O;D(null)>]?streamingContext : StreamingContext, [<O;D(null)>]?leaveOpen : bool) : unit =

        let pickler = match pickler with None -> resolver.Resolve<'T> () | Some p -> p
        use formatter = initTextWriter formatProvider writer false leaveOpen
        let _ = writeRootObject resolver reflectionCache formatter streamingContext None false __.DisableSubtypeResolution pickler value
        ()

    /// <summary>Deserialize value of given type from the underlying stream.</summary>
    /// <param name="reader">source reader.</param>
    /// <param name="pickler">Pickler used for serialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for deserialization state. Defaults to the empty streaming context.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    member __.Deserialize<'T> (reader : TextReader, [<O;D(null)>]?pickler : Pickler<'T>, 
                                    [<O;D(null)>]?streamingContext : StreamingContext, [<O;D(null)>]?leaveOpen : bool) : 'T =

        let pickler = match pickler with None -> resolver.Resolve<'T> () | Some p -> p
        use formatter = initTextReader formatProvider reader false leaveOpen
        readRootObject resolver reflectionCache formatter streamingContext None __.DisableSubtypeResolution __.DisableAssemblyLoading pickler

    /// <summary>Evaluates and serializes a sequence of objects to the underlying stream.</summary>
    /// <param name="writer">Target text writer.</param>
    /// <param name="sequence">Input sequence to be evaluated and serialized.</param>
    /// <param name="pickler">Pickler used for serialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    /// <return>Number of elements written to the stream.</return>
    member __.SerializeSequence<'T>(writer : TextWriter, sequence:seq<'T>, [<O;D(null)>]?pickler : Pickler<'T>, 
                                        [<O;D(null)>]?streamingContext : StreamingContext, [<O;D(null)>]?leaveOpen : bool) : int =

        let pickler = match pickler with None -> resolver.Resolve<'T> () | Some p -> p
        use formatter = initTextWriter formatProvider writer true leaveOpen
        writeTopLevelSequence resolver reflectionCache formatter streamingContext false __.DisableSubtypeResolution pickler sequence

    /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
    /// <param name="reader">Source text reader.</param>
    /// <param name="pickler">Pickler used for deserialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for deserialization state. Defaults to the empty streaming context.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
    member __.DeserializeSequence<'T>(reader : TextReader, [<O;D(null)>]?pickler : Pickler<'T>, 
                                        [<O;D(null)>]?streamingContext : StreamingContext, [<O;D(null)>]?leaveOpen : bool) : seq<'T> =

        let pickler = match pickler with None -> resolver.Resolve<'T> () | Some p -> p
        let formatter = initTextReader formatProvider reader true leaveOpen
        readTopLevelSequence resolver reflectionCache formatter streamingContext __.DisableSubtypeResolution __.DisableAssemblyLoading pickler

    /// <summary>
    ///     Serializes a value to text writer, excluding values mandated by the provided IObjectSifter instance.
    ///     Values excluded from serialization will be returned tagged by their ids.
    /// </summary>
    /// <param name="stream">Target write stream.</param>
    /// <param name="value">Value to be serialized.</param>
    /// <param name="sifter">User supplied sifter implementation. Used to specify which nodes in the object graph are to be excluded from serialization.</param>
    /// <param name="pickler">Pickler used for element deserialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    /// <returns>Sifted values along with their graph ids.</returns>
    member __.SerializeSifted<'T>(writer : TextWriter, value:'T, sifter : IObjectSifter, 
                                                    [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext, 
                                                    [<O;D(null)>]?leaveOpen : bool) : (int64 * obj) [] =

        let pickler = match pickler with None -> resolver.Resolve<'T>() | Some p -> p
        use writer = initTextWriter formatProvider writer false leaveOpen
        let state = writeRootObject resolver reflectionCache writer streamingContext (Some sifter) false __.DisableSubtypeResolution pickler value
        state.Sifted.ToArray()

    /// <summary>
    ///     Deserializes a sifted value from stream, filling in sifted holes from the serialized using supplied objects.
    /// </summary>
    /// <param name="reader">Source text reader.</param>
    /// <param name="sifted">Object-id pairs used for filling sifted holes in serialization.s</param>
    /// <param name="pickler">Pickler used for element deserialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    member __.DeserializeSifted<'T>(reader : TextReader, sifted : (int64 * obj) [],
                                                    [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext, 
                                                    [<O;D(null)>]?leaveOpen : bool) : 'T =

        let pickler = match pickler with None -> resolver.Resolve<'T> () | Some p -> p
        use reader = initTextReader formatProvider reader false leaveOpen
        readRootObject resolver reflectionCache reader streamingContext (Some sifted) __.DisableSubtypeResolution __.DisableAssemblyLoading pickler

    /// <summary>
    ///     Pickles given value to string.
    /// </summary>
    /// <param name="value">Value to pickle.</param>
    /// <param name="pickler">Pickler used for serialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    member f.PickleToString (value : 'T, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : string =
        pickleString (fun m v -> f.Serialize(m, v, ?pickler = pickler, ?streamingContext = streamingContext)) value

    /// <summary>
    ///     Unpickles value from string.
    /// </summary>
    /// <param name="pickle">Input pickle.</param>
    /// <param name="pickler">Pickler used for deserialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for deserialization state. Defaults to the empty streaming context.</param>
    member f.UnPickleOfString (pickle : string, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext) : 'T =
        unpickleString (fun m -> f.Deserialize(m, ?pickler = pickler, ?streamingContext = streamingContext)) pickle

    /// <summary>
    ///     Pickles value to string, excluding objects mandated by the provided IObjectSifter instance.
    ///     Values excluded from serialization will be returned tagged by their ids. 
    ////    Sifted objects will have to be provided on deserialization along with their accompanying id's.
    /// </summary>
    /// <param name="value">Value to be serialized.</param>
    /// <param name="sifter">User supplied sifter implementation. Used to specify which nodes in the object graph are to be excluded from serialization.</param>
    /// <param name="pickler">Pickler used for element deserialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <returns>Pickled value along with sifted values along with their graph ids.</returns>
    member f.PickleToStringSifted<'T>(value:'T, sifter : IObjectSifter, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : string * (int64 * obj) [] =
        use sw = new StringWriter()
        let sifted = f.SerializeSifted<'T>(sw, value, sifter, ?pickler = pickler, ?streamingContext = streamingContext)
        sw.ToString(), sifted

    /// <summary>
    ///     Unpickles a sifted value, filling in sifted holes from the serialized using supplied objects.
    /// </summary>
    /// <param name="pickle">Pickle to deserialize.</param>
    /// <param name="sifted">Object-id pairs used for filling sifted holes in serialization.</param>
    /// <param name="pickler">Pickler used for element deserialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    member bp.UnPickleOfStringSifted<'T>(pickle : string, sifted : (int64 * obj) [], [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : 'T =
        use sr = new StringReader(pickle)
        bp.DeserializeSifted<'T>(sr, sifted, ?pickler = pickler, ?streamingContext = streamingContext)

    //
    //  Untyped API
    //

    /// <summary>Serialize object of given type to the underlying stream.</summary>
    /// <param name="writer">Target text writer.</param>
    /// <param name="value">Value to be serialized.</param>
    /// <param name="pickler">Untyped pickler used for serialization. Its type should be compatible with that of the supplied object.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <param name="encoding">encoding passed to the binary writer.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    member __.SerializeUntyped(writer : TextWriter, value : obj, pickler : Pickler, [<O;D(null)>]?streamingContext : StreamingContext, [<O;D(null)>]?leaveOpen : bool) : unit =
        use formatter = initTextWriter formatProvider writer false leaveOpen
        let _ = writeRootObjectUntyped resolver reflectionCache formatter streamingContext None false __.DisableSubtypeResolution pickler value
        ()

    /// <summary>Deserialize object of given type from the underlying stream.</summary>
    /// <param name="reader">Source text reader.</param>
    /// <param name="pickler">Untyped pickler used for deserialization. Its type should be compatible with that of the supplied object.</param>
    /// <param name="streamingContext">Streaming context for deserialization state. Defaults to the empty streaming context.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    member __.DeserializeUntyped(reader : TextReader, pickler : Pickler, [<O;D(null)>]?streamingContext : StreamingContext, [<O;D(null)>]?leaveOpen : bool) : obj =
        use formatter = initTextReader formatProvider reader false leaveOpen
        readRootObjectUntyped resolver reflectionCache formatter streamingContext None __.DisableSubtypeResolution __.DisableAssemblyLoading pickler

    /// <summary>Evaluate and serialize a sequence of objects to the underlying stream.</summary>
    /// <param name="writer">Target text writer.</param>
    /// <param name="sequence">Input sequence to be evaluated and serialized.</param>
    /// <param name="pickler">Untyped pickler used for element serialization. Its type should be compatible with that of the supplied sequence elements.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    /// <return>Number of elements written to the stream.</return>
    member __.SerializeSequenceUntyped(writer : TextWriter, sequence : IEnumerable, pickler : Pickler,
                                            [<O;D(null)>]?streamingContext : StreamingContext, [<O;D(null)>]?leaveOpen : bool) : int =

        use formatter = initTextWriter formatProvider writer true leaveOpen
        writeTopLevelSequenceUntyped resolver reflectionCache formatter streamingContext false __.DisableSubtypeResolution pickler sequence

    /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
    /// <param name="reader">source reader.</param>
    /// <param name="pickler">Untyped pickler used for element deserialization. Its type should be compatible with that of the supplied sequence elements.</param>
    /// <param name="streamingContext">Streaming context for deserialization state. Defaults to the empty streaming context.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
    member __.DeserializeSequenceUntyped(reader : TextReader, pickler : Pickler, [<O;D(null)>]?streamingContext : StreamingContext, [<O;D(null)>]?leaveOpen : bool) : IEnumerable =
        let formatter = initTextReader formatProvider reader true leaveOpen
        readTopLevelSequenceUntyped resolver reflectionCache formatter streamingContext __.DisableSubtypeResolution __.DisableAssemblyLoading pickler

    /// <summary>
    ///     Pickles given value to string using provided pickler.
    /// </summary>
    /// <param name="value">Value to pickle.</param>
    /// <param name="pickler">Untyped pickler used for serialization. Its type should be compatible with that of the supplied object.</param>
    /// <param name="streamingContext">Streaming context for serialization state. Defaults to the empty streaming context.</param>
    member f.PickleToStringUntyped (value : obj, pickler : Pickler, [<O;D(null)>]?streamingContext : StreamingContext) : string =
        pickleString (fun m v -> f.SerializeUntyped(m, v, pickler, ?streamingContext = streamingContext)) value

    /// <summary>
    ///     Unpickle using provided pickler.
    /// </summary>
    /// <param name="pickle">String to unpickle</param>
    /// <param name="pickler">Untyped pickler used for deserialization. Its type should be compatible with that of the pickle.</param>
    /// <param name="streamingContext">streaming context.</param>
    member f.UnPickleOfStringUntyped (pickle : string, pickler : Pickler, [<O;D(null)>]?streamingContext : StreamingContext) : obj =
        unpickleString (fun m -> f.DeserializeUntyped(m, pickler, ?streamingContext = streamingContext)) pickle