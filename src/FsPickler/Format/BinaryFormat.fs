namespace Nessos.FsPickler

    open System
    open System.IO
    open System.Text
    open System.Runtime.Serialization

    open Microsoft.FSharp.Core.LanguagePrimitives

    open Nessos.FsPickler

    [<AutoOpen>]
    module private BinaryFormatUtils =

        // each object is serialized with a 32 bit header 
        // of which the first 24 are a fixed identifier
        // and the final 8 encode the object flags.
        
        [<Literal>]
        let initValue = 0xf591ce00u

        [<Literal>]
        let initMask  = 0xffffff00u

        [<Literal>]
        let flagMask  = 0x000000ffu

        // this binary format uses Buffer.BlockCopy for performance
        // and thus does little to handle endianness issues.
        // To avoid silent data corruption, record the serializer's
        // endianness setting at the beginning of the serialization stream.
        let isLittleEndian = BitConverter.IsLittleEndian

        let inline createHeader (flags : ObjectFlags) = initValue ||| uint32 flags

        let inline readHeader (header : uint32) =
            if header &&& initMask <> initValue then
                raise <| new InvalidDataException("expected a new object header.")

            flagMask &&& header |> byte |> EnumOfValue<byte, ObjectFlags>
  

    type BinaryPickleWriter internal (stream : Stream) =

        let bw = new Nessos.FsPickler.Binary.BinaryWriter(stream)

        interface IPickleFormatWriter with
            member __.BeginWriteRoot (tag : string) =
                bw.Write initValue
                bw.Write isLittleEndian
                bw.Write tag

            member __.EndWriteRoot () = ()

            member __.BeginWriteBoundedSequence _ length = bw.Write length
            member __.EndWriteBoundedSequence () = ()

            member __.BeginWriteUnBoundedSequence _ = ()
            member __.WriteHasNextElement hasNext = bw.Write hasNext

            member __.BeginWriteObject tag objectFlags =
                let header = createHeader objectFlags
                bw.Write header

            member __.EndWriteObject () = ()

            member __.WriteBoolean _ value = bw.Write value
            member __.WriteByte _ value = bw.Write value
            member __.WriteSByte _ value = bw.Write value

            member __.WriteInt16 _ value = bw.Write value
            member __.WriteInt32 _ value = bw.Write value
            member __.WriteInt64 _ value = bw.Write value

            member __.WriteUInt16 _ value = bw.Write value
            member __.WriteUInt32 _ value = bw.Write value
            member __.WriteUInt64 _ value = bw.Write value

            member __.WriteSingle _ value = bw.Write value
            member __.WriteDouble _ value = bw.Write value
            member __.WriteDecimal _ value = bw.Write value

            member __.WriteChar _ value = bw.Write value
            member __.WriteString _ value = bw.Write value

            member __.WriteDate _ value = bw.Write value.Ticks
            member __.WriteTimeSpan _ value = bw.Write value.Ticks
            member __.WriteGuid _ value = bw.Write value

            member __.WriteBigInteger _ value = 
                let data = value.ToByteArray()
                bw.Write data

            member __.WriteBytes _ value = bw.Write value

            member __.IsPrimitiveArraySerializationSupported = true
            member __.WritePrimitiveArray _ array = bw.Write array

            member __.Dispose () = bw.Dispose()

    and BinaryPickleReader internal (stream : Stream) =
        let br = new Nessos.FsPickler.Binary.BinaryReader(stream)

        interface IPickleFormatReader with
            
            member __.Dispose () = br.Dispose ()

            member __.BeginReadRoot (tag : string) =
                if br.ReadUInt32 () <> initValue then
                    raise <| new InvalidDataException("invalid stream initialization.")

                if br.ReadBoolean () <> isLittleEndian then
                    if isLittleEndian then
                        raise <| new InvalidDataException("serialized data is big-endian.")
                    else
                        raise <| new InvalidDataException("serialized data is little-endian.")

                let sTag = br.ReadString()
                if sTag <> tag then
                    raise <| new InvalidPickleTypeException(tag, sTag)

            member __.EndReadRoot () = ()

            member __.BeginReadObject tag =
                let header = br.ReadUInt32()
                readHeader header

            member __.EndReadObject () = () 

            member __.BeginReadBoundedSequence _ = br.ReadInt32 ()
            member __.EndReadBoundedSequence () = ()

            member __.BeginReadUnBoundedSequence _ = ()
            member __.ReadHasNextElement () = br.ReadBoolean ()

            member __.ReadBoolean _ = br.ReadBoolean()
            member __.ReadByte _ = br.ReadByte()
            member __.ReadSByte _ = br.ReadSByte()

            member __.ReadInt16 _ = br.ReadInt16()
            member __.ReadInt32 _ = br.ReadInt32()
            member __.ReadInt64 _ = br.ReadInt64()

            member __.ReadUInt16 _ = br.ReadUInt16()
            member __.ReadUInt32 _ = br.ReadUInt32()
            member __.ReadUInt64 _ = br.ReadUInt64()

            member __.ReadDecimal _ = br.ReadDecimal()
            member __.ReadSingle _ = br.ReadSingle()
            member __.ReadDouble _ = br.ReadDouble()

            member __.ReadChar _ = br.ReadChar()
            member __.ReadString _ = br.ReadString()

            member __.ReadDate _ = let ticks = br.ReadInt64() in DateTime(ticks)
            member __.ReadTimeSpan _ = let ticks = br.ReadInt64() in TimeSpan(ticks)
            member __.ReadGuid _ = br.ReadGuid ()

            member __.ReadBigInteger _ =
                let data = br.ReadBytes()
                new System.Numerics.BigInteger(data)

            member __.ReadBytes _ = br.ReadBytes()

            member __.IsPrimitiveArraySerializationSupported = true
            member __.ReadPrimitiveArray _ array = br.ReadArray(array)


    and BinaryPickleFormatProvider () =

        interface IBinaryPickleFormatProvider with
            member __.Name = "Binary"

            member __.CreateWriter (stream : Stream, _, _) = new BinaryPickleWriter(stream) :> _
            member __.CreateReader (stream : Stream, _, _) = new BinaryPickleReader(stream) :> _