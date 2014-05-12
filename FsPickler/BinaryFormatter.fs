namespace Nessos.FsPickler

    open System
    open System.IO
    open System.Text
    open System.Runtime.Serialization

    open Nessos.FsPickler.Utils

    [<AutoOpen>]
    module private BinaryFormatUtils =
        
        [<Literal>]
        let initByte = 130uy

        let inline createHeader (pickler : Pickler) (flags : ObjectFlags) =
            uint32 initByte ||| (uint32 pickler.PicklerFlags <<< 8) ||| (uint32 flags <<< 24)

        let inline readHeader (pickler : Pickler) (header : uint32) =
            if byte header <> initByte then
                raise <| new SerializationException ("invalid stream data.")
            else
                let flags = uint16 (header >>> 8)
                if flags <> pickler.PicklerFlags then
                    if byte flags <> byte pickler.PicklerFlags then
                        let msg = sprintf "FsPickler: next object is of unexpected type (anticipated %O)." pickler.Type
                        raise <| new SerializationException(msg)
                    else
                        let msg = sprintf "FsPickler: object of type '%O' was serialized with incompatible pickler." pickler.Type
                        raise <| new SerializationException(msg)
                else 
                    Core.LanguagePrimitives.EnumOfValue<byte, ObjectFlags> (byte (header >>> 24))
                    

    type BinaryPickleWriter (stream : Stream) =

        let bw = new Nessos.FsPickler.Binary.BinaryWriter(stream)

        interface IPickleFormatWriter with
            member __.BeginWriteRoot (id : string) =
                bw.Write initByte
                bw.Write id

            member __.EndWriteRoot () = ()

            member __.BeginWriteObject pickler _ objFlags =
                let header = createHeader pickler objFlags
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
            member __.WritePrimitiveArray _ array = bw.Write array

            member __.Dispose () = bw.Dispose()

    and BinaryPickleReader (stream : Stream) =
        let br = new Nessos.FsPickler.Binary.BinaryReader(stream)

        interface IPickleFormatReader with
            
            member __.Dispose () = br.Dispose ()

            member __.BeginReadRoot () =
                if br.ReadByte () <> initByte then
                    raise <| new SerializationException("stream error.")
                br.ReadString()

            member __.EndReadRoot () = ()

            member __.BeginReadObject (pickler : Pickler) _ =
                let header = br.ReadUInt32()
                readHeader pickler header

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
            member __.ReadToPrimitiveArray _ array = br.ReadArray(array)


    and BinaryFormatProvider () =
        interface IPickleFormatProvider with
            member __.CreateWriter (stream : Stream) = new BinaryPickleWriter(stream) :> _
            member __.CreateReader (stream : Stream) = new BinaryPickleReader(stream) :> _
//
//        abstract ReadDecimal : tag:string -> decimal
//        abstract ReadSingle : tag:string -> float32
//        abstract ReadDouble : tag:string -> float
//
//        abstract ReadChar : tag:string -> char
//        abstract ReadChars : tag:string -> char []
//        abstract ReadString : tag:string -> string
//
//        abstract ReadBytes : tag:string -> byte []
//        abstract ReadBytesFixed : tag:string -> length:int -> byte []
//        abstract ReadToPrimitiveArray<'Element, 'Array when 'Array :> Array> : tag:string -> 'Array -> unit
//
//    and IPickleFormatProvider =
//        abstract CreateWriter : Stream -> IPickleFormatWriter
//        abstract CreateReader : Stream -> IPickleFormatReader