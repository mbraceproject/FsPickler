namespace Nessos.FsPickler

open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.Text
open System.Runtime.Serialization

open Nessos.FsPickler.RootSerialization

/// <summary>
///     An abstract class containing the text-based serialization API.
/// </summary>
[<AbstractClass>]
type FsPicklerTextSerializer (formatProvider : ITextPickleFormatProvider, [<O;D(null)>] ?typeConverter) =
    inherit FsPicklerSerializer(formatProvider, ?typeConverter = typeConverter)

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
        writeRootObject resolver reflectionCache formatter streamingContext pickler value

    /// <summary>Deserialize value of given type from the underlying stream.</summary>
    /// <param name="reader">source reader.</param>
    /// <param name="pickler">Pickler used for serialization. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context for deserialization state. Defaults to the empty streaming context.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    member __.Deserialize<'T> (reader : TextReader, [<O;D(null)>]?pickler : Pickler<'T>, 
                                    [<O;D(null)>]?streamingContext : StreamingContext, [<O;D(null)>]?leaveOpen : bool) : 'T =

        let pickler = match pickler with None -> resolver.Resolve<'T> () | Some p -> p
        use formatter = initTextReader formatProvider reader false leaveOpen
        readRootObject resolver reflectionCache formatter streamingContext pickler

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
        writeTopLevelSequence resolver reflectionCache formatter streamingContext pickler sequence

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
        readTopLevelSequence resolver reflectionCache formatter streamingContext pickler

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
        writeRootObjectUntyped resolver reflectionCache formatter streamingContext pickler value

    /// <summary>Deserialize object of given type from the underlying stream.</summary>
    /// <param name="reader">Source text reader.</param>
    /// <param name="pickler">Untyped pickler used for deserialization. Its type should be compatible with that of the supplied object.</param>
    /// <param name="streamingContext">Streaming context for deserialization state. Defaults to the empty streaming context.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    member __.DeserializeUntyped(reader : TextReader, pickler : Pickler, [<O;D(null)>]?streamingContext : StreamingContext, [<O;D(null)>]?leaveOpen : bool) : obj =
        use formatter = initTextReader formatProvider reader false leaveOpen
        readRootObjectUntyped resolver reflectionCache formatter streamingContext pickler

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
        writeTopLevelSequenceUntyped resolver reflectionCache formatter streamingContext pickler sequence

    /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
    /// <param name="reader">source reader.</param>
    /// <param name="pickler">Untyped pickler used for element deserialization. Its type should be compatible with that of the supplied sequence elements.</param>
    /// <param name="streamingContext">Streaming context for deserialization state. Defaults to the empty streaming context.</param>
    /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
    /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
    member __.DeserializeSequenceUntyped(reader : TextReader, pickler : Pickler, [<O;D(null)>]?streamingContext : StreamingContext, [<O;D(null)>]?leaveOpen : bool) : IEnumerable =
        let formatter = initTextReader formatProvider reader true leaveOpen
        readTopLevelSequenceUntyped resolver reflectionCache formatter streamingContext pickler

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