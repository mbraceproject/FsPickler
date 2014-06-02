namespace Nessos.FsPickler

    open System
    open System.Collections
    open System.Collections.Generic
    open System.IO
    open System.Text

    open Nessos.FsPickler.RootObjectSerialization

    [<AbstractClass>]
    [<AutoSerializableAttribute(false)>]
    type StringPickler (formatP : IStringPickleFormatProvider, ?tyConv) =
        inherit FsPickler(formatP, ?tyConv = tyConv)

        let resolver = base.Resolver
        let reflectionCache = base.ReflectionCache

        /// <summary>Serialize value to the underlying stream.</summary>
        /// <param name="writer">target writer.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        member __.Serialize<'T>(writer : TextWriter, value : 'T, [<O;D(null)>]?streamingContext) : unit =
            let pickler = resolver.Resolve<'T> ()
            let formatter = formatP.CreateWriter writer
            writeRootObject resolver reflectionCache formatter streamingContext pickler value

        /// <summary>Serialize value to the underlying stream using given pickler.</summary>
        /// <param name="pickler">pickler used for serialization.</param>
        /// <param name="writer">target writer.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        member __.Serialize<'T>(pickler : Pickler<'T>, writer : TextWriter, value : 'T, [<O;D(null)>]?streamingContext) : unit =
            let formatter = formatP.CreateWriter writer
            writeRootObject resolver reflectionCache formatter streamingContext pickler value

        /// <summary>Serialize object of given type to the underlying stream.</summary>
        /// <param name="valueType">type of the given object.</param>
        /// <param name="writer">target writer.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize(valueType : Type, writer : TextWriter, value : obj, [<O;D(null)>]?streamingContext) : unit =
            let pickler = resolver.Resolve valueType
            let formatter = formatP.CreateWriter writer
            writeRootObjectUntyped resolver reflectionCache formatter streamingContext pickler value

        /// <summary>Serialize object to the underlying stream using given pickler.</summary>
        /// <param name="pickler">untyped pickler used for serialization.</param>
        /// <param name="writer">target writer.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        member __.Serialize(pickler : Pickler, writer : TextWriter, value : obj, [<O;D(null)>]?streamingContext) : unit =
            let formatter = formatP.CreateWriter writer
            writeRootObjectUntyped resolver reflectionCache formatter streamingContext pickler value

        /// <summary>Deserialize value of given type from the underlying stream.</summary>
        /// <param name="reader">source reader.</param>
        /// <param name="streamingContext">streaming context.</param>
        member __.Deserialize<'T> (reader : TextReader, [<O;D(null)>]?streamingContext) : 'T =
            let pickler = resolver.Resolve<'T> ()
            let formatter = formatP.CreateReader reader
            readRootObject resolver reflectionCache formatter streamingContext pickler

        /// <summary>Deserialize value of given type from the underlying stream, using given pickler.</summary>
        /// <param name="pickler">pickler used for serialization.</param>
        /// <param name="reader">source reader.</param>
        /// <param name="streamingContext">streaming context.</param>
        member __.Deserialize<'T> (pickler : Pickler<'T>, reader : TextReader, [<O;D(null)>]?streamingContext) : 'T =
            let formatter = formatP.CreateReader reader
            readRootObject resolver reflectionCache formatter streamingContext pickler

        /// <summary>Deserialize object of given type from the underlying stream.</summary>
        /// <param name="valueType">anticipated value type.</param>
        /// <param name="reader">source reader.</param>
        /// <param name="streamingContext">streaming context.</param>
        member __.Deserialize (valueType : Type, reader : TextReader, [<O;D(null)>]?streamingContext) : obj =
            let pickler = resolver.Resolve valueType
            let formatter = formatP.CreateReader reader
            readRootObjectUntyped resolver reflectionCache formatter streamingContext pickler

        /// <summary>Deserialize object from the underlying stream using given pickler.</summary>
        /// <param name="pickler">untyped pickler used for deserialization.</param>
        /// <param name="reader">source reader.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <return>number of elements written to the stream.</return>
        member __.Deserialize (pickler : Pickler, reader : TextReader, [<O;D(null)>]?streamingContext) : obj =
            let formatter = formatP.CreateReader reader
            readRootObjectUntyped resolver reflectionCache formatter streamingContext pickler

        /// <summary>Serialize a sequence of objects to the underlying stream.</summary>
        /// <param name="writer">target writer.</param>
        /// <param name="sequence">input sequence.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <return>number of elements written to the stream.</return>
        member __.SerializeSequence<'T>(writer : TextWriter, sequence:seq<'T>, [<O;D(null)>]?streamingContext) : int =
            let pickler = resolver.Resolve<'T> ()
            let formatter = formatP.CreateWriter writer
            writeTopLevelSequence resolver reflectionCache formatter streamingContext pickler sequence

        /// <summary>Serialize a sequence of objects to the underlying stream.</summary>
        /// <param name="elementType">element type used in sequence.</param>
        /// <param name="writer">target writer.</param>
        /// <param name="sequence">input sequence.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <return>number of elements written to the stream.</return>
        member __.SerializeSequence(elementType : Type, writer : TextWriter, sequence : IEnumerable, [<O;D(null)>]?streamingContext) : int =
            let pickler = resolver.Resolve elementType
            let formatter = formatP.CreateWriter writer
            writeTopLevelSequenceUntyped resolver reflectionCache formatter streamingContext pickler sequence

        /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
        /// <param name="reader">source reader.</param>
        /// <param name="length">number of elements to be deserialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
        member __.DeserializeSequence<'T>(reader : TextReader, length : int, [<O;D(null)>]?streamingContext) : IEnumerator<'T> =
            let pickler = resolver.Resolve<'T> ()
            let formatter = formatP.CreateReader reader
            readTopLevelSequence resolver reflectionCache formatter streamingContext pickler length

        /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
        /// <param name="elementType">element type used in sequence.</param>
        /// <param name="reader">source reader.</param>
        /// <param name="length">number of elements to be deserialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
        member __.DeserializeSequence(elementType : Type, reader : TextReader, length : int, [<O;D(null)>]?streamingContext) : IEnumerator =
            let pickler = resolver.Resolve elementType
            let formatter = formatP.CreateReader reader
            readTopLevelSequenceUntyped resolver reflectionCache formatter streamingContext pickler length


        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="pickler">Pickler to use.</param>
        /// <param name="value">Value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.Pickle (pickler : Pickler<'T>, value : 'T, [<O;D(null)>]?streamingContext) : string =
            pickleString (fun m v -> f.Serialize(pickler, m, v, ?streamingContext = streamingContext)) value

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="value">Value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.Pickle (value : 'T, [<O;D(null)>]?streamingContext) : string =
            pickleString (fun m v -> f.Serialize(m, v, ?streamingContext = streamingContext)) value

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="pickler">pickler to use.</param>
        /// <param name="value">value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.Pickle (pickler: Pickler, value : obj, [<O;D(null)>]?streamingContext) : string =
            pickleString (fun m v -> f.Serialize(pickler, m, v, ?streamingContext = streamingContext)) value

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="valueType">type of pickled value.</param>
        /// <param name="value">value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.Pickle (valueType : Type, value : obj, [<O;D(null)>]?streamingContext) : string =
            pickleString (fun m v -> f.Serialize(valueType, m, v, ?streamingContext = streamingContext)) value

        /// <summary>
        ///     Unpickles value using given pickler.
        /// </summary>
        /// <param name="pickler">Pickler to use.</param>
        /// <param name="pickle">Pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.UnPickle (pickler : Pickler<'T>, pickle : string, [<O;D(null)>]?streamingContext) : 'T =
            unpickleString (fun m -> f.Deserialize(pickler, m, ?streamingContext = streamingContext)) pickle

        /// <summary>
        ///     Unpickle value to given type.
        /// </summary>
        /// <param name="pickle">Pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.UnPickle<'T> (pickle : string, [<O;D(null)>]?streamingContext) : 'T =
            unpickleString (fun m -> f.Deserialize<'T>(m, ?streamingContext = streamingContext)) pickle

        /// <summary>
        ///     Unpickle value to given type.
        /// </summary>
        /// <param name="valueType">type of pickled value.</param>
        /// <param name="pickle">Pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.UnPickle (valueType : Type, pickle : string, [<O;D(null)>]?streamingContext) : obj =
            unpickleString (fun m -> f.Deserialize(valueType, m, ?streamingContext = streamingContext)) pickle

        /// <summary>
        ///     Unpickle value to given type.
        /// </summary>
        /// <param name="pickler">pickler to use.</param>
        /// <param name="pickle">Pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.UnPickle (pickler : Pickler, pickle : string, [<O;D(null)>]?streamingContext) : obj =
            unpickleString (fun m -> f.Deserialize(pickler, m, ?streamingContext = streamingContext)) pickle

    type XmlPickler(?tyConv, ?indent) = inherit StringPickler(new XmlPickleFormatProvider(?indent = indent), ?tyConv = tyConv)
    type JsonPickler(?tyConv, ?indent) = inherit StringPickler(new JsonPickleFormatProvider(?indent = indent), ?tyConv = tyConv)