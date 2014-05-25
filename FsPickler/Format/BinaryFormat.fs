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
        //
        //   1. the first byte is a fixed identifier
        //   2. the second byte contains TypeInfo
        //   3. the third byte contains PicklerInfo
        //   3. the fourth byte contains ObjectFlags
        
        [<Literal>]
        let initByte = 130uy

        let binarySerializePrimitiveArrays = BitConverter.IsLittleEndian

        let inline createHeader (typeInfo : TypeInfo) (picklerInfo : PicklerInfo) (flags : ObjectFlags) =
            uint32 initByte 
            ||| (uint32 typeInfo <<< 8) 
            ||| (uint32 picklerInfo <<< 16) 
            ||| (uint32 flags <<< 24)

        let inline readHeader (typeInfo : TypeInfo) (picklerInfo : PicklerInfo) (header : uint32) =
            if byte header <> initByte then
                raise <| new InvalidDataException("invalid stream data.")

            let streamTypeInfo = header >>> 8 |> byte |> EnumOfValue<byte, TypeInfo>
            if streamTypeInfo <> typeInfo then
                let message = sprintf "expected '%O', was '%O'." typeInfo streamTypeInfo
                raise <| new SerializationException(message)

            let streamPicklerInfo = header >>> 16 |> byte |> EnumOfValue<byte, PicklerInfo>
            if streamPicklerInfo <> picklerInfo then
                let message = sprintf "expected '%O, was '%O'." picklerInfo streamPicklerInfo
                raise <| new SerializationException(message)

            header >>> 24 |> byte |> EnumOfValue<byte, ObjectFlags>
  

    type BinaryPickleWriter (stream : Stream) =

        let bw = new Nessos.FsPickler.Binary.BinaryWriter(stream)

        interface IPickleFormatWriter with
            member __.BeginWriteRoot (id : string) =
                bw.Write initByte
                bw.Write id

            member __.EndWriteRoot () = ()

            member __.BeginWriteObject typeFlags picklerFlags tag objectFlags =
                let header = createHeader typeFlags picklerFlags objectFlags
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
                if obj.ReferenceEquals(value, null) then
                    bw.Write true
                else
                    bw.Write false
                    bw.Write value

            member __.WriteBytes _ value = 
                bw.Write value.Length
                bw.Write value

            member __.WriteBytesFixed _ value = bw.Write value

            member __.IsPrimitiveArraySerializationSupported = binarySerializePrimitiveArrays
            member __.WritePrimitiveArray _ array = bw.Write array

            member __.Dispose () = bw.Dispose()

    and BinaryPickleReader (stream : Stream) =
        let br = new Nessos.FsPickler.Binary.BinaryReader(stream)

        interface IPickleFormatReader with
            
            member __.Dispose () = br.Dispose ()

            member __.BeginReadRoot (tag : string) =
                if br.ReadByte () <> initByte then
                    raise <| new SerializationException("stream error.")
                let streamTag = br.ReadString()
                if streamTag <> tag then
                    let msg = sprintf "Expected '%s', got '%s'." tag streamTag
                    raise <| new SerializationException(msg)

            member __.EndReadRoot () = ()

            member __.BeginReadObject typeFlags picklerFlags tag =
                let header = br.ReadUInt32()
                readHeader typeFlags picklerFlags header

            member __.EndReadObject () = () 

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

            member __.ReadBytes _ = let length = br.ReadInt32() in br.ReadBytes(length)
            member __.ReadBytesFixed _ length = br.ReadBytes(length)

            member __.IsPrimitiveArraySerializationSupported = binarySerializePrimitiveArrays
            member __.ReadPrimitiveArray _ array = br.ReadArray(array)


    and BinaryFormatProvider () =
        interface IPickleFormatProvider with
            member __.CreateWriter (stream : Stream) = new BinaryPickleWriter(stream) :> _
            member __.CreateReader (stream : Stream) = new BinaryPickleReader(stream) :> _