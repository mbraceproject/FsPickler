namespace FsPickler

    open System
    open System.Runtime.Serialization

    open FsPickler.Utils

    type TypeHash = uint16

    type TypeInfo =
        | Primitive = 0
        | Enum = 1
        | Value = 2
        | Array = 3
        | Sealed = 4
        | NonSealed = 5
        | Abstract = 6

    type PicklerInfo =
        | Atomic = 0
        | ReflectionType = 1
        | ReflectionDerived = 2
        | Array = 3
        | ISerializable = 4
        | FSharpValue = 5 // tuples, records and DUs
        | Delegate = 6
        | Combinator = 7
        | UserDefined = 8

    module internal Header =

        [<Literal>]
        let sequenceCounterResetThreshold = 1000

        /// builds type info enumeration out of reflection info
        let computeTypeInfo (t : Type) =
            if t.IsPrimitive then TypeInfo.Primitive
            elif t.IsEnum then TypeInfo.Enum
            elif t.IsValueType then TypeInfo.Value
            elif t.IsArray then TypeInfo.Array
            elif t.IsSealed then TypeInfo.Sealed
            elif t.IsAbstract then TypeInfo.Abstract
            else TypeInfo.NonSealed

        // build a rudimentary 16-bit hash out of a given type
        // this should be runtime-invariant and not dependent on
        // their qualified names or ITypeNameConverter implementations
        let computeTypeHash (t : Type) : TypeHash =
            let mutable hash = 0us
            
            if t.IsPrimitive then hash <- hash ||| 1us
            if t.IsEnum then hash <- hash ||| 4us
            if t.IsValueType then hash <- hash ||| 2us
            if t.IsArray then hash <- hash ||| 8us
            if t.IsClass then hash <- hash ||| 16us
            if t.IsSealed then hash <- hash ||| 32us
            if t.IsAbstract then hash <- hash ||| 64us
            if t.IsGenericType then hash <- hash ||| 128us
            if t = typeof<obj> then hash <- hash ||| 256us

            if t.Assembly = typeof<int>.Assembly then hash <- hash ||| 512us
            elif t.Assembly = typeof<int option>.Assembly then hash <- hash ||| 1024us

            match t.Namespace with
            | null -> hash <- hash ||| 2048us
            | ns -> 
                if ns.StartsWith("System.Reflection") then hash <- hash ||| 4096us

            if typeof<ISerializable>.IsAssignableFrom t then hash <- hash ||| 8192us

            hash


        // each reference type is serialized with a 32 bit header
        //   1. the first byte is a fixed identifier
        //   2. the next two bytes are the pickler's typehash.
        //   3. the third byte conveys object-specific switches
        //
        module ObjHeader =

            [<Literal>]
            let initByte = 130uy

            // control bits
            [<Literal>]
            let empty               = 0uy
            [<Literal>]
            let isNull              = 1uy
            [<Literal>]
            let isProperSubtype     = 2uy
            [<Literal>]
            let isNewCachedInstance = 4uy
            [<Literal>]
            let isOldCachedInstance = 8uy
            [<Literal>]
            let isCyclicInstance    = 16uy

            let inline hasFlag (h : byte) (flag : byte) = h &&& flag = flag
        
            let inline create (hash : TypeHash) (flags : byte) =
                uint32 initByte ||| (uint32 hash <<< 8) ||| (uint32 flags <<< 24)

            let inline read (t : Type) (hash : TypeHash) (header : uint32) =
                if byte header <> initByte then
                    raise <| new SerializationException ("Stream error: invalid serialization data.")
                elif uint16 (header >>> 8) <> hash then
                    let msg = sprintf "Stream error: next object is of unexpected type (anticipated %O)." t
                    raise <| new SerializationException(msg)
                else 
                    byte (header >>> 24)