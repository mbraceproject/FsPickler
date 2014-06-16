namespace Nessos.FsPickler

    /// A Do-Nothing serialization formatter ; used by the object visitor

    type internal NullPickleWriter () =

        interface IPickleFormatWriter with
            member __.BeginWriteRoot _ = ()
            member __.EndWriteRoot () = ()

            member __.PreferLengthPrefixInSequences = true
            member __.WriteNextSequenceElement _ = ()

            member __.BeginWriteObject _ _ = ()
            member __.EndWriteObject () = ()

            member __.WriteBoolean _ _ = ()
            member __.WriteByte _ _ = ()
            member __.WriteSByte _ _ = ()

            member __.WriteInt16 _ _ = ()
            member __.WriteInt32 _ _ = ()
            member __.WriteInt64 _ _ = ()

            member __.WriteUInt16 _ _ = ()
            member __.WriteUInt32 _ _ = ()
            member __.WriteUInt64 _ _ = ()

            member __.WriteSingle _ _ = ()
            member __.WriteDouble _ _ = ()
            member __.WriteDecimal _ _ = ()

            member __.WriteChar _ _ = ()
            member __.WriteString _ _ = ()

            member __.WriteDate _ _ = ()
            member __.WriteTimeSpan _ _ = ()
            member __.WriteGuid _ _ = ()

            member __.WriteBigInteger _ _ = ()

            member __.WriteBytes _ _ = ()

            member __.IsPrimitiveArraySerializationSupported = true
            member __.WritePrimitiveArray _ _ = ()

            member __.Dispose () = ()