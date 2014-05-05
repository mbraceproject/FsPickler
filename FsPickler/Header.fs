namespace Nessos.FsPickler

    open System
    open System.Reflection
    open System.Runtime.Serialization

    

    type TypeKind =
        | Primitive = 0
        | String = 1
        | Enum = 2
        | Value = 3
        | Array = 4
        | Sealed = 5
        | NonSealed = 6
        | Abstract = 7
        | ArrayCompatible = 8

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
            elif t = typeof<string> then TypeKind.String
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


//        // each reference type is serialized with a 32 bit header
//        //   1. the first byte is a fixed identifier
//        //   2. the next two bytes identify the used pickler
//        //   3. the third byte conveys object-specific switches
//        //
//
//        module ObjHeader =
//
//            [<Literal>]
//            let initByte = 130uy
//
//            // control bits
//            [<Literal>]
//            let empty               = 0uy
//            [<Literal>]
//            let isNull              = 1uy
//            [<Literal>]
//            let isProperSubtype     = 2uy
//            [<Literal>]
//            let isNewCachedInstance = 4uy
//            [<Literal>]
//            let isOldCachedInstance = 8uy
//            [<Literal>]
//            let isCyclicInstance    = 16uy
//            [<Literal>]
//            let isSequenceHeader    = 32uy
//
//            let inline hasFlag (h : byte) (flag : byte) = h &&& flag = flag
//        
