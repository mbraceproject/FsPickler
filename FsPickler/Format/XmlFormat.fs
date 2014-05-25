namespace Nessos.FsPickler

    open System
    open System.IO
    open System.Text
    open System.Xml
    open System.Runtime.Serialization

    open Microsoft.FSharp.Core.LanguagePrimitives

    [<AutoOpen>]
    module private XmlUtils =

        let inline writePrimitive (w : ^XmlWriter) (tag : string) (value : ^T) =
            (^XmlWriter : (member WriteStartElement : string -> unit) (w, tag))
            (^XmlWriter : (member WriteValue : ^T -> unit) (w, value))
            (^XmlWriter : (member WriteEndElement : unit -> unit) w)

        let (*inline*) readElementName (r : XmlReader) (tag : string) =
            while r.NodeType <> XmlNodeType.Element && r.Read() do ()

            if r.NodeType <> XmlNodeType.Element then
                raise <| new InvalidDataException()

            elif r.Name <> tag then
                let msg = sprintf "expected '%s', was '%s'" tag r.Name
                raise <| new InvalidDataException(msg)


    type XmlPickleWriter(stream : Stream, encoding : Encoding, indent : bool) =

        let settings = new XmlWriterSettings()
        do 
            settings.Encoding <- encoding
            settings.Indent <- indent

        let writer = XmlWriter.Create(stream, settings)
        
        interface IPickleFormatWriter with

            member __.BeginWriteRoot (tag : string) = 
                writer.WriteStartDocument()
                writer.WriteStartElement("FsPickler")
                writer.WriteAttributeString("version", AssemblyVersionInformation.Version)
                writer.WriteAttributeString("id", tag)

            member __.EndWriteRoot () = 
                writer.WriteEndElement()
                writer.WriteEndDocument()

            member __.BeginWriteObject (typeInfo : TypeInfo) (picklerInfo : PicklerInfo) (tag : string) (objectFlags : ObjectFlags) =
                writer.WriteStartElement(tag)
                writer.WriteAttributeString("typeInfo", string << int <| typeInfo)
                writer.WriteAttributeString("picklerInfo", string << int <| picklerInfo)
                writer.WriteAttributeString("objectFlags", string << int <| objectFlags)

            member __.EndWriteObject () = writer.WriteEndElement()

            member __.WriteBoolean (tag : string) value = writePrimitive writer tag value
            member __.WriteByte (tag : string) value = writePrimitive writer tag (int value)
            member __.WriteSByte (tag : string) value = writePrimitive writer tag (int value)

            member __.WriteInt16 (tag : string) value = writePrimitive writer tag (int value)
            member __.WriteInt32 (tag : string) value = writePrimitive writer tag value
            member __.WriteInt64 (tag : string) value = writePrimitive writer tag value

            member __.WriteUInt16 (tag : string) value = writePrimitive writer tag (int value)
            member __.WriteUInt32 (tag : string) value = writePrimitive writer tag (int value)
            member __.WriteUInt64 (tag : string) value = writePrimitive writer tag (int64 value)

            member __.WriteSingle (tag : string) value = writePrimitive writer tag value
            member __.WriteDouble (tag : string) value = writePrimitive writer tag value
            member __.WriteDecimal (tag : string) value = writePrimitive writer tag value

            member __.WriteChar (tag : string) value = writePrimitive writer tag (string value)
            member __.WriteString (tag : string) value = writePrimitive writer tag value

            member __.WriteBytes (tag : string) value = 
                writer.WriteStartElement(tag)
                writer.WriteAttributeString("length", string value.Length)
                writer.WriteBase64(value, 0, value.Length)
                writer.WriteEndElement()

            member __.WriteBytesFixed (tag : string) value = 
                writer.WriteStartElement(tag)
                writer.WriteBase64(value, 0, value.Length)
                writer.WriteEndElement()

            member __.IsPrimitiveArraySerializationSupported = false
            member __.WritePrimitiveArray _ _ = raise <| new NotSupportedException()

            member __.Dispose () = writer.Flush () ; writer.Dispose()


    type XmlPickleReader(stream : Stream, encoding : Encoding) =

        let settings = new XmlReaderSettings()
        do
            settings.IgnoreWhitespace <- true

        let reader = XmlReader.Create(stream, settings)

        interface IPickleFormatReader with
            
            member __.BeginReadRoot (tag : string) =
                do readElementName reader "FsPickler"

                let version = reader.["version"]
                if version <> AssemblyVersionInformation.Version then
                    let msg = sprintf "Invalid FsPickler version %s (expected %s)." version AssemblyVersionInformation.Version
                    raise <| new InvalidDataException(msg)

                let sTag = reader.["id"]
                if sTag <> tag then
                    let msg = sprintf "Expected type '%s' but was '%s'." tag sTag
                    raise <| new InvalidDataException(msg)

                if not <| reader.Read() then
                    raise <| new EndOfStreamException()

            member __.EndReadRoot () = reader.ReadEndElement()

            member __.BeginReadObject (typeInfo : TypeInfo) (picklerInfo : PicklerInfo) (tag : string) =
                do readElementName reader tag

                let sTypeInfo = reader.GetAttribute("typeInfo") |> byte |> EnumOfValue<byte, TypeInfo>
                let sPicklerInfo = reader.GetAttribute("picklerInfo") |> byte |> EnumOfValue<byte, PicklerInfo>
                let objectFlags = reader.GetAttribute("objectFlags") |> byte |> EnumOfValue<byte, ObjectFlags>

                if sTypeInfo <> typeInfo then   
                    let message = sprintf "expected '%O', got '%O'." typeInfo sTypeInfo
                    raise <| new SerializationException(message)
                elif sPicklerInfo <> picklerInfo then
                    let message = sprintf "expected '%O', got '%O'." picklerInfo sPicklerInfo
                    raise <| new SerializationException(message)

                if not reader.IsEmptyElement then
                    if not <| reader.Read() then
                        raise <| new EndOfStreamException()

                objectFlags

            member __.EndReadObject() = 
                if reader.IsEmptyElement then
                    let _ = reader.Read() in ()
                else
                    reader.ReadEndElement()

            member __.ReadBoolean tag = readElementName reader tag ; reader.ReadElementContentAsBoolean()

            member __.ReadByte tag = readElementName reader tag ; reader.ReadElementContentAsInt() |> byte
            member __.ReadSByte tag = readElementName reader tag ; reader.ReadElementContentAsInt() |> sbyte

            member __.ReadInt16 tag = readElementName reader tag ; reader.ReadElementContentAsInt() |> int16
            member __.ReadInt32 tag = readElementName reader tag ; reader.ReadElementContentAsInt()
            member __.ReadInt64 tag = readElementName reader tag ; reader.ReadElementContentAsLong()

            member __.ReadUInt16 tag = readElementName reader tag ; reader.ReadElementContentAsInt() |> uint16
            member __.ReadUInt32 tag = readElementName reader tag ; reader.ReadElementContentAsInt() |> uint32
            member __.ReadUInt64 tag = readElementName reader tag ; reader.ReadElementContentAsLong() |> uint64

            member __.ReadDecimal tag = readElementName reader tag ; reader.ReadElementContentAsDecimal()
            member __.ReadSingle tag = readElementName reader tag ; reader.ReadElementContentAsFloat()
            member __.ReadDouble tag = readElementName reader tag ; reader.ReadElementContentAsDouble()

            member __.ReadChar tag = readElementName reader tag ; reader.ReadElementContentAsString().[0]
            member __.ReadString tag = readElementName reader tag ; reader.ReadElementContentAsString()

            member __.ReadBytes tag =
                do readElementName reader tag
                let length = reader.GetAttribute("length") |> int
                let bytes = Array.zeroCreate<byte> length
                do reader.Read() |> ignore
                let n = reader.ReadContentAsBase64(bytes, 0, length)
                if n < length then
                    raise <| new EndOfStreamException()

                if reader.NodeType = XmlNodeType.Text then
                    reader.Read() |> ignore

                bytes

            member __.ReadBytesFixed tag length =
                do readElementName reader tag
                let bytes = Array.zeroCreate<byte> length
                do reader.Read() |> ignore
                let n = reader.ReadContentAsBase64(bytes, 0, length)
                if n < length then
                    raise <| new EndOfStreamException()

                if reader.NodeType = XmlNodeType.Text then
                    reader.Read() |> ignore

                bytes

            member __.IsPrimitiveArraySerializationSupported = false
            member __.ReadPrimitiveArray _ _ = raise <| new NotImplementedException()

            member __.Dispose () = reader.Dispose()


        type XmlPickleFormatProvider(encoding : Encoding, ?indent) =
            let indent = defaultArg indent false
            
            interface IPickleFormatProvider with
                member __.CreateWriter(stream) = new XmlPickleWriter(stream, encoding, indent) :> _
                member __.CreateReader(stream) = new XmlPickleReader(stream, encoding) :> _