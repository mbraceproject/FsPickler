namespace Nessos.FsPickler
    
    open System
    open System.IO
    open System.Collections
    open System.Collections.Generic
    open System.Runtime.Serialization

    open Nessos.FsPickler.Utils
    open Nessos.FsPickler.Hashing
    open Nessos.FsPickler.PicklerUtils
    open Nessos.FsPickler.TypeCache
    open Nessos.FsPickler.RootObjectSerialization

    type private OAttribute = System.Runtime.InteropServices.OptionalAttribute
    type private DAttribute = System.Runtime.InteropServices.DefaultParameterValueAttribute

    [<Sealed>]
    [<AutoSerializableAttribute(false)>]
    type FsPickler private (formatP : IPickleFormatProvider, ?tyConv) =

        let resolver = PicklerCache.Instance :> IPicklerResolver
        let reflectionCache = new ReflectionCache(?tyConv = tyConv)

        // TODO : tyConv
        static member CreateBinary () = new FsPickler(BinaryPickleFormatProvider())
        static member CreateJson () = new FsPickler(JsonPickleFormatProvider())
        static member CreateXml () = new FsPickler(XmlPickleFormatProvider())

        member __.PickleFormat = formatP.Name

        /// <summary>Serialize value to the underlying stream.</summary>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize<'T>(stream : Stream, value : 'T, [<O;D(null)>]?streamingContext) : unit =
            let pickler = resolver.Resolve<'T> ()
            writeRootObject formatP resolver reflectionCache streamingContext stream pickler value

        /// <summary>Serialize value to the underlying stream using given pickler.</summary>
        /// <param name="pickler">pickler used for serialization.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        member __.Serialize<'T>(pickler : Pickler<'T>, stream : Stream, value : 'T, [<O;D(null)>]?streamingContext) : unit =
            writeRootObject formatP resolver reflectionCache streamingContext stream pickler value

        /// <summary>Serialize object of given type to the underlying stream.</summary>
        /// <param name="valueType">type of the given object.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        member __.Serialize(valueType : Type, stream : Stream, value : obj, [<O;D(null)>]?streamingContext) : unit =
            let pickler = resolver.Resolve valueType
            writeRootObjectUntyped formatP resolver reflectionCache streamingContext stream pickler value

        /// <summary>Serialize object to the underlying stream using given pickler.</summary>
        /// <param name="pickler">untyped pickler used for serialization.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        member __.Serialize(pickler : Pickler, stream : Stream, value : obj, [<O;D(null)>]?streamingContext) : unit =
            writeRootObjectUntyped formatP resolver reflectionCache streamingContext stream pickler value

        /// <summary>Deserialize value of given type from the underlying stream.</summary>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        member __.Deserialize<'T> (stream : Stream, [<O;D(null)>]?streamingContext) : 'T =
            let pickler = resolver.Resolve<'T> ()
            readRootObject formatP resolver reflectionCache streamingContext stream pickler

        /// <summary>Deserialize value of given type from the underlying stream, using given pickler.</summary>
        /// <param name="pickler">pickler used for serialization.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        member __.Deserialize<'T> (pickler : Pickler<'T>, stream : Stream, [<O;D(null)>]?streamingContext) : 'T =
            readRootObject formatP resolver reflectionCache streamingContext stream pickler

        /// <summary>Deserialize object of given type from the underlying stream.</summary>
        /// <param name="valueType">anticipated value type.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        member __.Deserialize (valueType : Type, stream : Stream, [<O;D(null)>]?streamingContext) : obj =
            let pickler = resolver.Resolve valueType
            readRootObjectUntyped formatP resolver reflectionCache streamingContext stream pickler

        /// <summary>Deserialize object from the underlying stream using given pickler.</summary>
        /// <param name="pickler">untyped pickler used for deserialization.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <return>number of elements written to the stream.</return>
        member __.Deserialize (pickler : Pickler, stream : Stream, [<O;D(null)>]?streamingContext) : obj =
            readRootObjectUntyped formatP resolver reflectionCache streamingContext stream pickler

        /// <summary>Serialize a sequence of objects to the underlying stream.</summary>
        /// <param name="stream">target stream.</param>
        /// <param name="sequence">input sequence.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <return>number of elements written to the stream.</return>
        member __.SerializeSequence<'T>(stream : Stream, sequence:seq<'T>, [<O;D(null)>]?streamingContext) : int =
            let pickler = resolver.Resolve<'T> ()
            writeTopLevelSequence formatP resolver reflectionCache streamingContext stream pickler sequence

        /// <summary>Serialize a sequence of objects to the underlying stream.</summary>
        /// <param name="elementType">element type used in sequence.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="sequence">input sequence.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <return>number of elements written to the stream.</return>
        member __.SerializeSequence(elementType : Type, stream : Stream, sequence : IEnumerable, [<O;D(null)>]?streamingContext) : int =
            let pickler = resolver.Resolve elementType
            writeTopLevelSequenceUntyped formatP resolver reflectionCache streamingContext stream pickler sequence

        /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
        /// <param name="stream">source stream.</param>
        /// <param name="length">number of elements to be deserialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
        member __.DeserializeSequence<'T>(stream : Stream, length : int, [<O;D(null)>]?streamingContext) : IEnumerator<'T> =
            let pickler = resolver.Resolve<'T> ()
            readTopLevelSequence formatP resolver reflectionCache streamingContext stream pickler length

        /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
        /// <param name="elementType">element type used in sequence.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="length">number of elements to be deserialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
        member __.DeserializeSequence(elementType : Type, stream : Stream, length : int, [<O;D(null)>]?streamingContext) : IEnumerator =
            let pickler = resolver.Resolve elementType
            readTopLevelSequenceUntyped formatP resolver reflectionCache streamingContext stream pickler length

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="pickler">Pickler to use.</param>
        /// <param name="value">Value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.Pickle (pickler : Pickler<'T>, value : 'T, [<O;D(null)>]?streamingContext) : byte [] =
            pickle (fun m v -> f.Serialize(pickler, m, v, ?streamingContext = streamingContext)) value

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="value">Value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.Pickle (value : 'T, [<O;D(null)>]?streamingContext) : byte [] =
            pickle (fun m v -> f.Serialize(m, v, ?streamingContext = streamingContext)) value

        /// <summary>
        ///     Unpickles value using given pickler.
        /// </summary>
        /// <param name="pickler">Pickler to use.</param>
        /// <param name="data">Pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.UnPickle (pickler : Pickler<'T>, data : byte [], [<O;D(null)>]?streamingContext) : 'T =
            unpickle (fun m -> f.Deserialize(pickler, m, ?streamingContext = streamingContext)) data

        /// <summary>
        ///     Unpickle value to given type.
        /// </summary>
        /// <param name="data">Pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        member f.UnPickle<'T> (data : byte [], [<O;D(null)>]?streamingContext) : 'T =
            unpickle (fun m -> f.Deserialize<'T>(m, ?streamingContext = streamingContext)) data

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