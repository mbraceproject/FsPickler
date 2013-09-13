namespace FsCoreSerializer

    open System
    open System.Runtime.Serialization

    // each reference type is serialized with a 32 bit header
    //   1. the first byte is a fixed identifier
    //   2. the next two bytes are a truncated hash which describe the type being used.
    //   3. the third byte conveys object-specific switches
    //
    module internal ObjHeader =

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
        let computeHash (t : Type) =
            let mutable hash = 0us
            
            if t.IsPrimitive then hash <- hash ||| 1us
            if t.IsEnum then hash <- hash ||| 4us
            if t.IsValueType then hash <- hash ||| 2us
            if t.IsArray then hash <- hash ||| 8us
            if t.IsClass then hash <- hash ||| 16us
            if t.IsSealed then hash <- hash ||| 32us
            if t.IsAbstract then hash <- hash ||| 64us
            if t.IsGenericType then hash <- hash ||| 128us

            match t.Namespace with
            | null -> hash <- hash ||| 256us
            | ns -> 
                if ns.StartsWith("System") then hash <- hash ||| 512us
                if ns.StartsWith("System.Reflection") then hash <- hash ||| 1024us
                if ns.StartsWith("Microsoft.FSharp") then hash <- hash ||| 2048us

            if typeof<ISerializable>.IsAssignableFrom t then hash <- hash ||| 4096us
//            if typeof<IFsCoreSerializable>.IsAssignableFrom t then hash <- hash ||| 8192us

            hash