namespace FsCoreSerializer

    open System
    open System.Runtime.Serialization

    // each reference type is serialized with a 32 bit header
    //   1. the first byte is a fixed identifier
    //   2. the next two bytes are a truncated hash which identifies the type being serialized
    //   3. the third byte conveys object-specific switches
    //
    module internal ObjHeader =

        // literals are much faster than enums

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
        let isNewInstance       = 4uy
        [<Literal>]
        let isCachedInstance    = 8uy

        let inline hasFlag (h : byte) (flag : byte) = h &&& flag = flag
        
        let inline create (hash : uint16) (flags : byte) =
            uint32 initByte ||| (uint32 hash <<< 8) ||| (uint32 flags <<< 24)

        let inline read (hash : uint16) (header : uint32) =
            if byte header <> initByte then
                raise <| new SerializationException ("Stream error: expected object header")
            elif uint16 (header >>> 8) <> hash then 
                raise <| new SerializationException("Stream error: invalid object header")
            else 
                byte (header >>> 24)

        // build a rudimentary 16-bit hash out of a given type
        // this should be persistable and runtime-independent
        let getTruncatedHash (t : Type) =
            let aqn = TypeFormatter.TypeNameConverter.ToQualifiedName t
            let mutable hash = 0us
            for i = 0 to aqn.Length / 2 - 1 do
                let pairEnc = uint16 aqn.[i] + (uint16 aqn.[i+1] <<< 8)
                hash <- hash ^^^ pairEnc

            if aqn.Length % 2 <> 0 then
                hash <- hash ^^^ uint16 aqn.[aqn.Length-1]

            hash