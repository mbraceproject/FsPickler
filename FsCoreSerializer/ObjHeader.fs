namespace FsCoreSerializer

    open System
    open System.Runtime.Serialization

    // each reference type is serialized with a 32 bit header
    //   1. the first byte is a fixed identifier
    //   2. the next two bytes are a truncated hash which identifies the type being serialized
    //   3. the third byte conveys serialization specific switches
    //
    module internal ObjHeader =

        // literals are much faster than enums

        [<Literal>]
        let initByte = 130uy

        // control bits
        [<Literal>]
        let empty               = 0uy
        [<Literal>]
        let isNull              = 0b00000001uy
        [<Literal>]
        let isProperSubtype     = 0b00000010uy
        [<Literal>]
        let isNewInstance       = 0b00000100uy
        [<Literal>]
        let isCachedInstance    = 0b00001000uy

        // build a 16-bit hash out of a given type
        let getTruncatedHash (t : Type) =
            let n = (TypeFormatter.Default.Write t).GetHashCode()
            uint16 n ^^^ uint16 (n >>> 16)

        let inline hasFlag (h : byte) (flag : byte) = h &&& flag = flag
        
        let inline create (hash : uint16) (flags : byte) =
            uint32 initByte ||| (uint32 hash <<< 8) ||| (uint32 flags <<< 24)

        let inline read (hash : uint16) (header : uint32) =
            if byte header <> initByte || uint16 (header >>> 8) <> hash then 
                raise <| new SerializationException("stream error")
            else 
                byte (header >>> 24)