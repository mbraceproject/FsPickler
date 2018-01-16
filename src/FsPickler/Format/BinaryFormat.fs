namespace MBrace.FsPickler

open System
open System.IO
open System.Text
open System.Threading
open System.Runtime.Serialization

open Microsoft.FSharp.Core.LanguagePrimitives

open MBrace.FsPickler

[<AutoOpen>]
module private BinaryFormatUtils =

    // past format versions

    [<Literal>]
    let formatv0960 = 0960us // As specified in FsPickler v. 0.9.6.0

    [<Literal>]
    let formatv1200 = 1200us // As specified in FsPickler v. 1.2.0.0

    [<Literal>]
    let formatv1400 = 1400us // As specified in FsPickler v. 1.4.0.0

    [<Literal>]
    let formatv2000 = 2000us // As specified in FsPickler v. 2.0.0.0

    [<Literal>]
    let formatv4000 = 4000us // As specified in FsPickler v. 4.0.0.0

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
    let isLittleEndianSystem = BitConverter.IsLittleEndian

    let inline createHeader (flags : ObjectFlags) = initValue ||| uint32 flags

    let inline readHeader (header : uint32) =
        if header &&& initMask <> initValue then
            raise <| new InvalidDataException("expected a new object header.")

        flagMask &&& header |> byte |> EnumOfValue<byte, ObjectFlags>

    [<Literal>]
    let bufferSize = 256    
    let buffer = new ThreadLocal<byte []>(fun () -> Array.zeroCreate<byte> bufferSize)

    /// block copy primitive array to stream
    let blockCopy (source : Array, target : Stream) =

        let buf = buffer.Value
        let mutable bytes = Buffer.ByteLength source
        let mutable i = 0

        while bytes > bufferSize do
            Buffer.BlockCopy(source, i, buf, 0, bufferSize)
            target.Write(buf, 0, bufferSize)
            i <- i + bufferSize
            bytes <- bytes - bufferSize

        if bytes > 0 then
            Buffer.BlockCopy(source, i, buf, 0, bytes)
            target.Write(buf, 0, bytes)

    /// copy stream contents to preallocated array
    let blockRead (source : Stream, target : Array) =
        let buf = buffer.Value
        let inline fillBytes (n : int) =
            let mutable read = 0
            while read < n do
                read <- read + source.Read(buf, read, n - read)
        
        let mutable bytes = Buffer.ByteLength target
        let mutable i = 0

        while bytes > bufferSize do
            do fillBytes bufferSize
            Buffer.BlockCopy(buf, 0, target, i, bufferSize)
            i <- i + bufferSize
            bytes <- bytes - bufferSize

        if bytes > 0 then
            do fillBytes bytes
            Buffer.BlockCopy(buf, 0, target, i, bytes)
  
// force little endian : by default, the writer uses Buffer.BlockCopy when serializing arrays.
// this is performant, but it does not respect endianness.
// forcing little endian makes ALL data pass through BinaryWriter, but is less efficient

/// <summary>
///     Binary format serializer.
/// </summary>
[<AutoSerializable(false)>]
type BinaryPickleWriter internal (stream : Stream, encoding : Encoding, leaveOpen : bool, forceLittleEndian : bool) =

    let bw = new BinaryWriter(stream, encoding, leaveOpen)

    interface IPickleFormatWriter with
        member __.Flush () = ()
        member __.BeginWriteRoot (tag : string) =
            bw.Write initValue
            bw.Write formatv4000
            bw.Write encoding.CodePage

            if forceLittleEndian then 
                bw.Write 0uy
            elif isLittleEndianSystem then
                bw.Write 1uy
            else
                bw.Write 2uy

            bw.Write tag

        member __.EndWriteRoot () = ()

        member __.SerializeUnionCaseNames = false
        member __.UseNamedEnumSerialization = false

        member __.PreferLengthPrefixInSequences = true
        member __.WriteNextSequenceElement hasNext = bw.Write hasNext

        member __.WriteCachedObjectId id = bw.Write id

        member __.BeginWriteObject _ objectFlags =
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
        member __.WriteString _ value = 
            if isNull value then bw.Write true
            else
                bw.Write false
                bw.Write value

        member __.WriteDateTime _ value = 
            bw.Write (byte value.Kind)
            bw.Write value.Ticks
            if value.Kind = DateTimeKind.Local then
                let offset = TimeZoneInfo.Local.GetUtcOffset(value)
                bw.Write(offset.Ticks)

        member __.WriteDateTimeOffset _ value =
            bw.Write value.Ticks
            bw.Write value.Offset.Ticks

        member __.WriteTimeSpan _ value = bw.Write value.Ticks
        member __.WriteGuid _ value = bw.Write (value.ToByteArray())

        member __.WriteBigInteger _ value = 
            let data = value.ToByteArray()
            bw.Write data.Length
            bw.Write data

        member __.WriteBytes _ value = 
            if isNull value then bw.Write -1
            else
                bw.Write value.Length
                bw.Write value

        // if forced little endian, primitive arrays are to be serialized
        // in an element-by-element basis. This ensures that all serialization
        // is passed through BinaryWriter that always uses little endian.
        member __.IsPrimitiveArraySerializationSupported = not forceLittleEndian
        member __.WritePrimitiveArray _ array = blockCopy(array, stream)

        member __.Dispose () = bw.Dispose()

/// <summary>
///     Binary format deserializer.
/// </summary>
[<AutoSerializable(false)>]
type BinaryPickleReader internal (stream : Stream, encoding : Encoding, leaveOpen : bool) =

    let br = new BinaryReader(stream, encoding, leaveOpen)

    let mutable isForcedLittleEndianStream = false

    interface IPickleFormatReader with
            
        member __.Dispose () = br.Dispose ()

        member __.BeginReadRoot (tag : string) =
            if br.ReadUInt32 () <> initValue then
                raise <| new InvalidDataException("invalid stream initialization bytes.")

            let version = br.ReadUInt16()
            if version <> formatv4000 then
                if version = formatv0960 then
                    raise <| new FormatException("FsPickler Binary format version 0.9.6.0 no longer supported.")
                elif version = formatv1200 then
                    raise <| new FormatException("FsPickler Binary format version 1.2.0.0 no longer supported.")
                elif version = formatv1400 then
                    raise <| new FormatException("FsPickler Binary format version 1.4.0.0 no longer supported.")
                elif version = formatv2000 then
                    raise <| new FormatException("FsPickler Binary format version 2.0.0.0 no longer supported.")
                else
                    raise <| new FormatException(sprintf "unsupported binary format version '%d'." version)

            let codePage = br.ReadInt32()
            if codePage <> encoding.CodePage then
                raise <| new FormatException(sprintf "invalid code page '%d' (expected %d)." codePage encoding.CodePage)

            match br.ReadByte () with
            | 0uy -> isForcedLittleEndianStream <- true
            | b ->
                let isLittleEndianStream = (b = 1uy)
                if isLittleEndianStream <> isLittleEndianSystem then
                    if isLittleEndianStream then
                        raise <| new FormatException("serialized data is little-endian.")
                    else
                        raise <| new FormatException("serialized data is big-endian.")

            let sTag = br.ReadString()
            if sTag <> tag then
                raise <| new InvalidPickleTypeException(tag, sTag)

        member __.EndReadRoot () = ()

        member __.BeginReadObject _ =
            let header = br.ReadUInt32()
            readHeader header

        member __.EndReadObject () = () 

        member __.SerializeUnionCaseNames = false
        member __.UseNamedEnumSerialization = false

        member __.PreferLengthPrefixInSequences = true
        member __.ReadNextSequenceElement () = br.ReadBoolean()

        member __.ReadCachedObjectId () = br.ReadInt64()

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
        member __.ReadString _ = 
            if br.ReadBoolean() then null
            else
                br.ReadString()

        member __.ReadDateTime _ =
            let kind = br.ReadByte() |> int |> enum<DateTimeKind>
            let ticks = br.ReadInt64()
            if kind = DateTimeKind.Local then 
                let offsetTicks = br.ReadInt64()
                let dto = new DateTimeOffset(ticks, TimeSpan(offsetTicks))
                dto.LocalDateTime
            else
                new DateTime(ticks, kind)

        member __.ReadDateTimeOffset _ =
            let ticks = br.ReadInt64()
            let offsetTicks = br.ReadInt64()
            let offset = new TimeSpan(offsetTicks)
            DateTimeOffset(ticks, offset)

        member __.ReadTimeSpan _ = let ticks = br.ReadInt64() in TimeSpan(ticks)
        member __.ReadGuid _ = let bytes = br.ReadBytes(16) in Guid(bytes)

        member __.ReadBigInteger _ =
            let length = br.ReadInt32()
            let data = br.ReadBytes(length)
            new System.Numerics.BigInteger(data)

        member __.ReadBytes _ = 
            let length = br.ReadInt32() 
            if length < 0 then null
            else br.ReadBytes(length)

        member __.IsPrimitiveArraySerializationSupported = not isForcedLittleEndianStream
        member __.ReadPrimitiveArray _ array = blockRead(stream, array)

/// <summary>
///     Factory methods for the binary serialization format.
/// </summary>
[<AutoSerializable(false)>]
type BinaryPickleFormatProvider (forceLittleEndian : bool) =

    member val ForceLittleEndian = forceLittleEndian with get, set

    interface IPickleFormatProvider with
        member __.Name = "Binary"

        // UTF8 is the optimal encoding for string serialization by BinaryWriter/Reader
        member __.DefaultEncoding = Encoding.UTF8

        member __.CreateWriter (stream, encoding, _, leaveOpen) = new BinaryPickleWriter(stream, encoding, leaveOpen, __.ForceLittleEndian) :> _
        member __.CreateReader (stream, encoding, _, leaveOpen) = new BinaryPickleReader(stream, encoding, leaveOpen) :> _