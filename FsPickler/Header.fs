namespace FsPickler

    open System
    open System.Reflection
    open System.Runtime.Serialization

    type PicklerFlags = uint16

    type TypeKind =
        | Primitive = 0
        | Enum = 1
        | Value = 2
        | Array = 3
        | Sealed = 4
        | NonSealed = 5
        | Abstract = 6
        | ArrayCompatible = 7

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
        let computeTypeKind (t : Type) =
            if t.IsPrimitive then TypeKind.Primitive
            elif t.IsEnum then TypeKind.Enum
            elif t.IsValueType then TypeKind.Value
            elif t.IsArray then TypeKind.Array
            elif Utils.isAssignableFromArray t then TypeKind.ArrayCompatible
            elif t.IsSealed then TypeKind.Sealed
            elif t.IsAbstract then TypeKind.Abstract
            else TypeKind.NonSealed

        // builds a 16-bit collection of flags that identify the serialized type.
        // these should be runtime invariant and independent of any ITypeNameConverter implementation.
        let computePicklerFlags (pI : PicklerInfo) (tI : TypeKind) cacheByRef useWithSubtypes isCyclic isFixedSize (t : Type) : PicklerFlags =
            
            // the first byte encodes type information
            let mutable byte1 = byte tI // first 4 bits reserved for TypeKind
            
            if t.IsGenericType then byte1 <- byte1 ||| 16uy
            if t = typeof<obj> then byte1 <- byte1 ||| 32uy
            if t.Assembly = typeof<int>.Assembly then byte1 <- byte1 ||| 64uy
            if typeof<MemberInfo>.IsAssignableFrom t then byte1 <- byte1 ||| 128uy

            // the second byte encodes pickler information
            let mutable byte2 = byte pI // first 4 bits reserved for PicklerInfo

            if cacheByRef then byte2 <- byte2 ||| 16uy
            if useWithSubtypes then byte2 <- byte2 ||| 32uy
            if isCyclic then byte2 <- byte2 ||| 64uy
            if isFixedSize then byte2 <- byte2 ||| 128uy

            uint16 byte1 ||| (uint16 byte2 <<< 8)


        // each reference type is serialized with a 32 bit header
        //   1. the first byte is a fixed identifier
        //   2. the next two bytes identify the used pickler
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
            [<Literal>]
            let isSequenceHeader    = 32uy

            let inline hasFlag (h : byte) (flag : byte) = h &&& flag = flag
        
            let inline create (hash : PicklerFlags) (flags : byte) =
                uint32 initByte ||| (uint32 hash <<< 8) ||| (uint32 flags <<< 24)

            let inline read (t : Type) (pflags : PicklerFlags) (header : uint32) =
                if byte header <> initByte then
                    raise <| new SerializationException ("FsPickler: invalid stream data.")
                else 
                    let pflags' = uint16 (header >>> 8)
                    if pflags' <> pflags then
                        if byte pflags <> byte pflags' then
                            let msg = sprintf "FsPickler: next object is of unexpected type (anticipated %O)." t
                            raise <| new SerializationException(msg)
                        else
                            let msg = sprintf "FsPickler: object of type '%O' was serialized with incompatible pickler." t
                            raise <| new SerializationException(msg)
                    else 
                        byte (header >>> 24)