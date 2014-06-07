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

    type internal OAttribute = System.Runtime.InteropServices.OptionalAttribute
    type internal DAttribute = System.Runtime.InteropServices.DefaultParameterValueAttribute

    [<AbstractClass>]
    [<AutoSerializableAttribute(false)>]
    type FsPickler internal (formatP : IPickleFormatProvider, ?tyConv) =

        let resolver = PicklerCache.Instance :> IPicklerResolver
        let reflectionCache = ReflectionCache.Create(?tyConv = tyConv)

        member internal __.Resolver = resolver
        member internal __.ReflectionCache = reflectionCache
        member __.PickleFormat = formatP.Name

        /// <summary>Serialize value to the underlying stream.</summary>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize<'T>(stream : Stream, value : 'T, 
                                    [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : unit =

            let pickler = resolver.Resolve<'T> ()
            use writer = initStreamWriter formatP stream encoding leaveOpen
            writeRootObject resolver reflectionCache writer streamingContext pickler value

        /// <summary>Serialize value to the underlying stream using given pickler.</summary>
        /// <param name="pickler">pickler used for serialization.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize<'T>(pickler : Pickler<'T>, stream : Stream, value : 'T, 
                                    [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : unit =

            use writer = initStreamWriter formatP stream encoding leaveOpen
            writeRootObject resolver reflectionCache writer streamingContext pickler value

        /// <summary>Serialize object of given type to the underlying stream.</summary>
        /// <param name="valueType">type of the given object.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize(valueType : Type, stream : Stream, value : obj, 
                                    [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : unit =

            let pickler = resolver.Resolve valueType
            use writer = initStreamWriter formatP stream encoding leaveOpen
            writeRootObjectUntyped resolver reflectionCache writer streamingContext pickler value

        /// <summary>Serialize object to the underlying stream using given pickler.</summary>
        /// <param name="pickler">untyped pickler used for serialization.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize(pickler : Pickler, stream : Stream, value : obj, 
                                    [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : unit =

            use writer = initStreamWriter formatP stream encoding leaveOpen
            writeRootObjectUntyped resolver reflectionCache writer streamingContext pickler value

        /// <summary>Deserialize value of given type from the underlying stream.</summary>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Deserialize<'T> (stream : Stream, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : 'T =

            let pickler = resolver.Resolve<'T> ()
            use reader = initStreamReader formatP stream encoding leaveOpen
            readRootObject resolver reflectionCache reader streamingContext pickler

        /// <summary>Deserialize value of given type from the underlying stream, using given pickler.</summary>
        /// <param name="pickler">pickler used for serialization.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Deserialize<'T> (pickler : Pickler<'T>, stream : Stream, 
                                            [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : 'T =

            use reader = initStreamReader formatP stream encoding leaveOpen
            readRootObject resolver reflectionCache reader streamingContext pickler

        /// <summary>Deserialize object of given type from the underlying stream.</summary>
        /// <param name="valueType">anticipated value type.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Deserialize (valueType : Type, stream : Stream, 
                                    [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : obj =

            let pickler = resolver.Resolve valueType
            use reader = initStreamReader formatP stream encoding leaveOpen
            readRootObjectUntyped resolver reflectionCache reader streamingContext pickler

        /// <summary>Deserialize object from the underlying stream using given pickler.</summary>
        /// <param name="pickler">untyped pickler used for deserialization.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        /// <return>number of elements written to the stream.</return>
        member __.Deserialize (pickler : Pickler, stream : Stream, 
                                    [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : obj =

            use reader = initStreamReader formatP stream encoding leaveOpen
            readRootObjectUntyped resolver reflectionCache reader streamingContext pickler

        /// <summary>Serialize a sequence of objects to the underlying stream.</summary>
        /// <param name="stream">target stream.</param>
        /// <param name="sequence">input sequence.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        /// <return>number of elements written to the stream.</return>
        member __.SerializeSequence<'T>(stream : Stream, sequence:seq<'T>, 
                                            [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : int =

            let pickler = resolver.Resolve<'T> ()
            use writer = initStreamWriter formatP stream encoding leaveOpen
            writeTopLevelSequence resolver reflectionCache writer streamingContext pickler sequence

        /// <summary>Serialize a sequence of objects to the underlying stream.</summary>
        /// <param name="elementType">element type used in sequence.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="sequence">input sequence.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        /// <return>number of elements written to the stream.</return>
        member __.SerializeSequence(elementType : Type, stream : Stream, sequence : IEnumerable, 
                                            [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : int =

            let pickler = resolver.Resolve elementType
            use writer = initStreamWriter formatP stream encoding leaveOpen
            writeTopLevelSequenceUntyped resolver reflectionCache writer streamingContext pickler sequence

        /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
        /// <param name="stream">source stream.</param>
        /// <param name="length">number of elements to be deserialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
        member __.DeserializeSequence<'T>(stream : Stream, length : int, 
                                            [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : IEnumerator<'T> =

            let pickler = resolver.Resolve<'T> ()
            let reader = initStreamReader formatP stream encoding leaveOpen
            readTopLevelSequence resolver reflectionCache reader streamingContext pickler length

        /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
        /// <param name="elementType">element type used in sequence.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="length">number of elements to be deserialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
        member __.DeserializeSequence(elementType : Type, stream : Stream, length : int, 
                                            [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : IEnumerator =

            let pickler = resolver.Resolve elementType
            let reader = initStreamReader formatP stream encoding leaveOpen
            readTopLevelSequenceUntyped resolver reflectionCache reader streamingContext pickler length

        /// Decides if given type is serializable by FsPickler
        static member IsSerializableType (t : Type) = (PicklerCache.Instance :> IPicklerResolver).IsSerializable t

        /// Decides if given type is serializable by FsPickler
        static member IsSerializableType<'T> () = (PicklerCache.Instance :> IPicklerResolver).IsSerializable<'T> ()

        /// Auto generates a pickler for given type variable
        static member GeneratePickler<'T> () = (PicklerCache.Instance :> IPicklerResolver).Resolve<'T> ()
        
        /// Auto generates a pickler for given type
        static member GeneratePickler (t : Type) = (PicklerCache.Instance :> IPicklerResolver).Resolve t