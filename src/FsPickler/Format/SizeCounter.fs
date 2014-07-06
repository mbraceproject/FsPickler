namespace Nessos.FsPickler

    open System

    // Provides a format implementation that serves at computing the rough size of an object in bytes.

    type internal SizeCounter () =
        
        let mutable size = 0L

        member __.Size = size

        interface IPickleFormatWriter with

            member __.BeginWriteRoot _ = ()
            member __.EndWriteRoot () = ()

            member __.SerializeUnionCaseNames = false

            member __.PreferLengthPrefixInSequences = true
            member __.WriteNextSequenceElement _ = ()

            member __.WriteCachedObjectId _ = ()

            member __.BeginWriteObject _ _ = size <- size + 8L
            member __.EndWriteObject () = ()

            member __.WriteBoolean _ _ = size <- size + 1L
            member __.WriteByte _ _ = size <- size + 1L
            member __.WriteSByte _ _ = size <- size + 1L

            member __.WriteInt16 _ _ = size <- size + 2L
            member __.WriteInt32 _ _ = size <- size + 4L
            member __.WriteInt64 _ _ = size <- size + 8L

            member __.WriteUInt16 _ _ = size <- size + 2L
            member __.WriteUInt32 _ _ = size <- size + 4L
            member __.WriteUInt64 _ _ = size <- size + 8L

            member __.WriteSingle _ _ = size <- size + 4L
            member __.WriteDouble _ _ = size <- size + 8L
            member __.WriteDecimal _ _ = size <- size + int64 sizeof<Decimal>

            member __.WriteChar _ _ = size <- size + 2L
            member __.WriteString _ str = size <- int64 <| 2 * str.Length

            member __.WriteDate _ _ = size <- size + int64 sizeof<DateTime>
            member __.WriteTimeSpan _ _ = size <- size + int64 sizeof<TimeSpan>
            member __.WriteGuid _ _ = size <- size + 16L

            member __.WriteBigInteger _ bi = size <- size + int64 (bi.ToByteArray().Length)

            member __.WriteBytes _ bs = size <- size + int64 bs.Length

            member __.IsPrimitiveArraySerializationSupported = true
            member __.WritePrimitiveArray _ array = size <- size + int64 (System.Buffer.ByteLength array)

            member __.Dispose () = ()