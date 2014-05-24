namespace Nessos.FsPickler

    open System
    open System.IO
    open System.Text
    open System.Xml

//    type XmlPickleWriter(stream : Stream, encoding : Encoding) =
//
//        let settings = new XmlWriterSettings()
//        do settings.Encoding <- encoding
//        let writer = XmlWriter.Create(stream)
//        let mutable currentIsPrimitive = false
//        
//        interface IPickleFormatWriter with
//
//            member __.BeginWriteRoot (id : string) = 
//                writer.WriteStartDocument()
//                writer.WriteStartElement("FsPickler")
//                writer.WriteAttributeString("version", AssemblyVersionInformation.Version)
//                writer.WriteAttributeString("rootObject", id)
//
//            member __.EndWriteRoot () = 
//                writer.WriteEndElement()
//                writer.WriteEndDocument()
//
//            member __.BeginWriteObject (pickler : Pickler) (tag : string) (flags : ObjectFlags) =
////                if ObjectFlags.hasFlag flags ObjectFlags.IsPrimitive then
////                    currentIsPrimitive <- true
////                else
//                writer.WriteStartElement(tag)
//                writer.WriteAttributeString("flags", string (int flags))
//
//            member __.EndWriteObject () =
////                if currentIsPrimitive then
////                    currentIsPrimitive <- false
////                else
//                writer.WriteEndElement()
//
//            member __.WriteBoolean (tag : string) value = writer.WriteValue(value)
//            member __.WriteByte (tag : string) value = writer.WriteValue(int value)
//            member __.WriteSByte (tag : string) value = writer.WriteValue(int value)
//
//            member __.WriteInt16 (tag : string) value = writer.WriteValue(int value)
//            member __.WriteInt32 (tag : string) value = 
//                writer.WriteStartElement(tag)
//                writer.WriteValue(value)
//                writer.WriteEndElement()
//
//            member __.WriteInt64 (tag : string) value = writer.WriteValue(value)
//
//            member __.WriteUInt16 (tag : string) value = writer.WriteValue(int value)
//            member __.WriteUInt32 (tag : string) value = writer.WriteValue(int value)
//            member __.WriteUInt64 (tag : string) value = writer.WriteValue(int64 value)
//
//            member __.WriteSingle (tag : string) value = writer.WriteValue(value)
//            member __.WriteDouble (tag : string) value = writer.WriteValue(value)
//            member __.WriteDecimal (tag : string) value = writer.WriteValue(value)
//
//            member __.WriteChar (tag : string) value = writer.WriteValue(string value)
//            member __.WriteString (tag : string) value = writer.WriteValue(value)
//
//            member __.WriteBytes (tag : string) value = 
//                writer.WriteStartElement(tag)
//                writer.WriteStartAttribute("length", string value.Length)
//                writer.WriteBase64(value, 0, value.Length)
//                writer.WriteEndElement()
//
//            member __.WriteBytesFixed (tag : string) value = writer.WriteBase64(value, 0, value.Length)
//            member __.WritePrimitiveArray (tag : string) values =
//                // todo : improve
//                writer.WriteStartElement(tag)
//                writer.WriteAttributeString("length", string values.Length)
//                for v in values do
//                    writer.WriteValue v
//
//                writer.WriteEndElement()
//
//            member __.Dispose () = writer.Flush () ; writer.Dispose()
//
//
//    type XmlPickleReader(stream : Stream, encoding : Encoding) =
//
//        let settings = new XmlWriterSettings()
//        do settings.Encoding <- encoding
//        let reader = XmlReader.Create(stream)
//
//        interface IPickleFormatReader with
//            
//            member __.BeginReadRoot () =
//                if reader.ReadToFollowing("FsPickler") then
//
//                    let version = reader.["version"]
//                    if version <> AssemblyVersionInformation.Version then
//                        let msg = sprintf "Invalid FsPickler version %s (expected %s)." version AssemblyVersionInformation.Version
//                        raise <| new InvalidDataException(msg)
//
//                    reader.["rootObject"]
//                else
//                    raise <| new EndOfStreamException("Stream ended prematurely.")
//
//            member __.EndReadRoot () = ()
//
//            member __.BeginReadObject (pickler : Pickler) (tag : string) =
//                if reader.Read() then
//                    if reader.Name <> tag then
//                        let msg = sprintf "Expected element '%s', got '%s'" tag reader.Name
//                        raise <| new InvalidDataException(msg)
//
//                    Core.LanguagePrimitives.EnumOfValue<byte, ObjectFlags> (byte reader.["flags"])
//                else
//                    raise <| new EndOfStreamException("Stream ended prematurely.")
//
//
//            member __.EndReadObject() = ()
//
//            member __.ReadBoolean _ = reader.ReadContentAsBoolean()
//            member __.ReadByte _ = byte <| reader.ReadContentAsInt()
//            member __.ReadSByte _ = sbyte <| reader.ReadContentAsInt()
//
//            member __.ReadInt16 _ = int16 <| reader.ReadContentAsInt()
//            member __.ReadInt32 _ = reader.ReadContentAsInt()
//            member __.ReadInt64 _ = reader.ReadElementContentAsLong()
//
//            member __.ReadUInt16 _ = uint16 <| reader.ReadContentAsInt()
//            member __.ReadUInt32 _ = uint32 <| reader.ReadContentAsInt()
//            member __.ReadUInt64 _ = uint64 <| reader.ReadElementContentAsLong()
//
//            member __.ReadDecimal _ = reader.ReadContentAsDecimal()
//            member __.ReadSingle _ = reader.ReadContentAsFloat()
//            member __.ReadDouble _ = reader.ReadContentAsDouble()
//
//            member __.ReadChar _ = reader.ReadContentAsString().[0]
//            member __.ReadString _ = reader.ReadContentAsString()
//
//            member __.ReadBytes tag =
//                if reader.Read() then
//                    if reader.Name <> tag then
//                        let msg = sprintf "Expected element '%s', got '%s'" tag reader.Name
//                        raise <| new InvalidDataException(msg)
//
//                    let length = int <| reader.["length"]
//                    let bytes = Array.zeroCreate length
//                    let n = reader.ReadElementContentAsBase64(bytes, 0, length)
//                    if n < length then
//                        raise <| new EndOfStreamException()
//
//                    bytes
//                else
//                    raise <| new EndOfStreamException("Stream ended prematurely.")
//
//            member __.ReadBytesFixed _ length =
//                let bytes = Array.zeroCreate length
//                let n = reader.ReadElementContentAsBase64(bytes,0,length)
//                if n < length then
//                    raise <| new EndOfStreamException()
//
//                bytes
//
//            member __.ReadToPrimitiveArray (tag : string) (array : Array) =
//                if reader.Read() then
//                    if reader.Name <> tag then
//                        let msg = sprintf "Expected element '%s', got '%s'" tag reader.Name
//                        raise <| new InvalidDataException(msg)
//
//                    let length = int <| reader.["length"]
//
//                    for i = 0 to length - 1 do
//                        let e = reader.ReadContentAsObject()
//                        array.SetValue(e, int64 i)
//                else
//                    raise <| new EndOfStreamException("Stream ended prematurely.")
//
//
//            member __.Dispose () = reader.Dispose()
//
//
//        type XmlPickleFormatProvider(encoding : Encoding) =
//            
//            interface IPickleFormatProvider with
//                member __.CreateWriter(stream) = new XmlPickleWriter(stream, encoding) :> _
//                member __.CreateReader(stream) = new XmlPickleReader(stream, encoding) :> _