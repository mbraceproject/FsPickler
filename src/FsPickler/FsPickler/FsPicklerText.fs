namespace Nessos.FsPickler

    open System
    open System.Collections
    open System.Collections.Generic
    open System.IO
    open System.Text

    open Nessos.FsPickler.RootSerialization

    /// <summary>
    ///     The base class for text-based pickle format implementations.
    /// </summary>
    [<AbstractClass>]
    type FsPicklerText (formatProvider : ITextPickleFormatProvider, [<O;D(null)>] ?typeConverter) =
        inherit FsPicklerBase(formatProvider, ?typeConverter = typeConverter)

        let resolver = base.Resolver
        let reflectionCache = base.ReflectionCache

        /// <summary>Serialize value to the underlying stream.</summary>
        /// <param name="writer">target writer.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        member __.Serialize<'T>(writer : TextWriter, value : 'T, 
                                        [<O;D(null)>]?streamingContext, [<O;D(null)>]?leaveOpen) : unit =

            let pickler = resolver.Resolve<'T> ()
            use formatter = initTextWriter formatProvider writer false leaveOpen
            writeRootObject resolver reflectionCache formatter streamingContext pickler value

        /// <summary>Serialize value to the underlying stream using given pickler.</summary>
        /// <param name="pickler">pickler used for serialization.</param>
        /// <param name="writer">target writer.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        member __.Serialize<'T>(pickler : Pickler<'T>, writer : TextWriter, value : 'T, 
                                        [<O;D(null)>]?streamingContext, [<O;D(null)>]?leaveOpen) : unit =

            use formatter = initTextWriter formatProvider writer false leaveOpen
            writeRootObject resolver reflectionCache formatter streamingContext pickler value

        /// <summary>Serialize object of given type to the underlying stream.</summary>
        /// <param name="valueType">type of the given object.</param>
        /// <param name="writer">target writer.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        member __.Serialize(valueType : Type, writer : TextWriter, value : obj, 
                                        [<O;D(null)>]?streamingContext, [<O;D(null)>]?leaveOpen) : unit =

            let pickler = resolver.Resolve valueType
            use formatter = initTextWriter formatProvider writer false leaveOpen
            writeRootObjectUntyped resolver reflectionCache formatter streamingContext pickler value

        /// <summary>Serialize object to the underlying stream using given pickler.</summary>
        /// <param name="pickler">untyped pickler used for serialization.</param>
        /// <param name="writer">target writer.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        member __.Serialize(pickler : Pickler, writer : TextWriter, value : obj, 
                                            [<O;D(null)>]?streamingContext, [<O;D(null)>]?leaveOpen) : unit =

            use formatter = initTextWriter formatProvider writer false leaveOpen
            writeRootObjectUntyped resolver reflectionCache formatter streamingContext pickler value

        /// <summary>Deserialize value of given type from the underlying stream.</summary>
        /// <param name="reader">source reader.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        member __.Deserialize<'T> (reader : TextReader, 
                                        [<O;D(null)>]?streamingContext, [<O;D(null)>]?leaveOpen) : 'T =

            let pickler = resolver.Resolve<'T> ()
            use formatter = initTextReader formatProvider reader false leaveOpen
            readRootObject resolver reflectionCache formatter streamingContext pickler

        /// <summary>Deserialize value of given type from the underlying stream, using given pickler.</summary>
        /// <param name="pickler">pickler used for serialization.</param>
        /// <param name="reader">source reader.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        member __.Deserialize<'T> (pickler : Pickler<'T>, reader : TextReader, 
                                        [<O;D(null)>]?streamingContext, [<O;D(null)>]?leaveOpen) : 'T =

            use formatter = initTextReader formatProvider reader false leaveOpen
            readRootObject resolver reflectionCache formatter streamingContext pickler

        /// <summary>Deserialize object of given type from the underlying stream.</summary>
        /// <param name="valueType">anticipated value type.</param>
        /// <param name="reader">source reader.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        member __.Deserialize (valueType : Type, reader : TextReader, 
                                    [<O;D(null)>]?streamingContext, [<O;D(null)>]?leaveOpen) : obj =

            let pickler = resolver.Resolve valueType
            use formatter = initTextReader formatProvider reader false leaveOpen
            readRootObjectUntyped resolver reflectionCache formatter streamingContext pickler

        /// <summary>Deserialize object from the underlying stream using given pickler.</summary>
        /// <param name="pickler">untyped pickler used for deserialization.</param>
        /// <param name="reader">source reader.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <return>number of elements written to the stream.</return>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        member __.Deserialize (pickler : Pickler, reader : TextReader, 
                                    [<O;D(null)>]?streamingContext, [<O;D(null)>]?leaveOpen) : obj =

            use formatter = initTextReader formatProvider reader false leaveOpen
            readRootObjectUntyped resolver reflectionCache formatter streamingContext pickler

        /// <summary>Serialize a sequence of objects to the underlying stream.</summary>
        /// <param name="writer">target writer.</param>
        /// <param name="sequence">input sequence.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        /// <return>number of elements written to the stream.</return>
        member __.SerializeSequence<'T>(writer : TextWriter, sequence:seq<'T>, 
                                            [<O;D(null)>]?streamingContext, [<O;D(null)>]?leaveOpen) : int =

            let pickler = resolver.Resolve<'T> ()
            use formatter = initTextWriter formatProvider writer true leaveOpen
            writeTopLevelSequence resolver reflectionCache formatter streamingContext pickler sequence

        /// <summary>Serialize a sequence of objects to the underlying stream.</summary>
        /// <param name="elementType">element type used in sequence.</param>
        /// <param name="writer">target writer.</param>
        /// <param name="sequence">input sequence.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        /// <return>number of elements written to the stream.</return>
        member __.SerializeSequence(elementType : Type, writer : TextWriter, sequence : IEnumerable, 
                                        [<O;D(null)>]?streamingContext, [<O;D(null)>]?leaveOpen) : int =

            let pickler = resolver.Resolve elementType
            use formatter = initTextWriter formatProvider writer true leaveOpen
            writeTopLevelSequenceUntyped resolver reflectionCache formatter streamingContext pickler sequence

        /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
        /// <param name="reader">source reader.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
        member __.DeserializeSequence<'T>(reader : TextReader, 
                                            [<O;D(null)>]?streamingContext, [<O;D(null)>]?leaveOpen) : seq<'T> =

            let pickler = resolver.Resolve<'T> ()
            let formatter = initTextReader formatProvider reader true leaveOpen
            readTopLevelSequence resolver reflectionCache formatter streamingContext pickler

        /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
        /// <param name="elementType">element type used in sequence.</param>
        /// <param name="reader">source reader.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to false.</param>
        /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
        member __.DeserializeSequence(elementType : Type, reader : TextReader, 
                                            [<O;D(null)>]?streamingContext, [<O;D(null)>]?leaveOpen) : IEnumerable =

            let pickler = resolver.Resolve elementType
            let formatter = initTextReader formatProvider reader true leaveOpen
            readTopLevelSequenceUntyped resolver reflectionCache formatter streamingContext pickler


        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="pickler">Pickler to use.</param>
        /// <param name="value">Value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.PickleToString (pickler : Pickler<'T>, value : 'T, [<O;D(null)>]?streamingContext) : string =
            pickleString (fun m v -> f.Serialize(pickler, m, v, ?streamingContext = streamingContext)) value

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="value">Value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.PickleToString (value : 'T, [<O;D(null)>]?streamingContext) : string =
            pickleString (fun m v -> f.Serialize(m, v, ?streamingContext = streamingContext)) value

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="pickler">pickler to use.</param>
        /// <param name="value">value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.PickleToString (pickler: Pickler, value : obj, [<O;D(null)>]?streamingContext) : string =
            pickleString (fun m v -> f.Serialize(pickler, m, v, ?streamingContext = streamingContext)) value

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="valueType">type of pickled value.</param>
        /// <param name="value">value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.PickleToString (valueType : Type, value : obj, [<O;D(null)>]?streamingContext) : string =
            pickleString (fun m v -> f.Serialize(valueType, m, v, ?streamingContext = streamingContext)) value

        /// <summary>
        ///     Unpickles value using given pickler.
        /// </summary>
        /// <param name="pickler">Pickler to use.</param>
        /// <param name="pickle">Pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.UnPickleOfString (pickler : Pickler<'T>, pickle : string, [<O;D(null)>]?streamingContext) : 'T =
            unpickleString (fun m -> f.Deserialize(pickler, m, ?streamingContext = streamingContext)) pickle

        /// <summary>
        ///     Unpickle value to given type.
        /// </summary>
        /// <param name="pickle">Pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.UnPickleOfString<'T> (pickle : string, [<O;D(null)>]?streamingContext) : 'T =
            unpickleString (fun m -> f.Deserialize<'T>(m, ?streamingContext = streamingContext)) pickle

        /// <summary>
        ///     Unpickle value to given type.
        /// </summary>
        /// <param name="valueType">type of pickled value.</param>
        /// <param name="pickle">Pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.UnPickleOfString (valueType : Type, pickle : string, [<O;D(null)>]?streamingContext) : obj =
            unpickleString (fun m -> f.Deserialize(valueType, m, ?streamingContext = streamingContext)) pickle

        /// <summary>
        ///     Unpickle value to given type.
        /// </summary>
        /// <param name="pickler">pickler to use.</param>
        /// <param name="pickle">Pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.UnPickleOfString (pickler : Pickler, pickle : string, [<O;D(null)>]?streamingContext) : obj =
            unpickleString (fun m -> f.Deserialize(pickler, m, ?streamingContext = streamingContext)) pickle