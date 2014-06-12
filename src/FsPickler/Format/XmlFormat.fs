namespace Nessos.FsPickler

    open System
    open System.IO
    open System.Text
    open System.Xml
    open System.Security

    open Microsoft.FSharp.Core.LanguagePrimitives

    module private XmlUtils =

        let inline writePrimitive (w : ^XmlWriter) (tag : string) (value : ^T) =
            (^XmlWriter : (member WriteStartElement : string -> unit) (w, tag))
            (^XmlWriter : (member WriteValue : ^T -> unit) (w, value))
            (^XmlWriter : (member WriteEndElement : unit -> unit) w)

        let inline readElementName (r : XmlReader) (tag : string) =
            while r.NodeType <> XmlNodeType.Element && r.Read() do ()

            if r.NodeType <> XmlNodeType.Element then
                raise <| new InvalidDataException()

            elif r.Name <> tag then
                let msg = sprintf "expected type '%s' but was '%s'" tag r.Name
                raise <| new FsPicklerException(msg)

        let inline escapeString (value : string) = SecurityElement.Escape value
        let inline unEscapeString (value : string) =
            let e = new SecurityElement("", value)
            e.Text

        let inline mkFlagCsv (flags : ObjectFlags) =
            let tokens = new ResizeArray<string>()

            if ObjectFlags.hasFlag flags ObjectFlags.IsNull then
                tokens.Add "null"

            if ObjectFlags.hasFlag flags ObjectFlags.IsCachedInstance then
                tokens.Add "cached"

            if ObjectFlags.hasFlag flags ObjectFlags.IsCyclicInstance then
                tokens.Add "cyclic"

            if ObjectFlags.hasFlag flags ObjectFlags.IsProperSubtype then
                tokens.Add "subtype"

            String.concat "," tokens

        let inline parseFlagCsv (csv : string) =
            if String.IsNullOrEmpty csv then ObjectFlags.None else

            let mutable flags = ObjectFlags.None
            let tokens = csv.Split(',')
            
            for t in tokens do
                match t with
                | "null" -> flags <- flags ||| ObjectFlags.IsNull
                | "cached" -> flags <- flags ||| ObjectFlags.IsCachedInstance
                | "cyclic" -> flags <- flags ||| ObjectFlags.IsCyclicInstance
                | "subtype" -> flags <- flags ||| ObjectFlags.IsProperSubtype
                | _ -> raise <| new InvalidDataException(sprintf "invalid pickle flag '%s'." t)

            flags


    open XmlUtils


    type XmlPickleWriter internal (textWriter : TextWriter, indent : bool, leaveOpen) =

        let settings = new XmlWriterSettings()
        do 
            settings.NewLineHandling <- NewLineHandling.Entitize
            settings.CloseOutput <- not leaveOpen
            settings.Indent <- indent
            settings.CheckCharacters <- false

        let writer = XmlWriter.Create(textWriter, settings)
        
        interface IPickleFormatWriter with

            member __.BeginWriteRoot (tag : string) = 
                writer.WriteStartDocument()
                writer.WriteStartElement("FsPickler")
                writer.WriteAttributeString("version", AssemblyVersionInformation.Version)
                writer.WriteAttributeString("type", tag)

            member __.EndWriteRoot () = 
                writer.WriteEndElement()
                writer.WriteEndDocument()

            member __.BeginWriteObject (tag : string) (flags : ObjectFlags) =
                writer.WriteStartElement(tag)

                if flags <> ObjectFlags.None then
                    writer.WriteAttributeString("flags", mkFlagCsv flags)

            member __.EndWriteObject () = writer.WriteEndElement()

            member __.BeginWriteBoundedSequence tag (length : int) =
                writer.WriteStartElement tag
                writer.WriteAttributeString("length", string length)

            member __.EndWriteBoundedSequence () =
                writer.WriteEndElement ()

            member __.BeginWriteUnBoundedSequence tag =
                writer.WriteStartElement tag

            member __.WriteHasNextElement hasNext = if not hasNext then writer.WriteEndElement()

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

            member __.WriteChar (tag : string) value = writePrimitive writer tag <| escapeString (string value)

            member __.WriteString (tag : string) value = 
                if obj.ReferenceEquals(value, null) then
                    writer.WriteStartElement(tag)
                    writer.WriteAttributeString("null", "true")
                    writer.WriteEndElement()
                else
                    writePrimitive writer tag <| escapeString value

            member __.WriteBigInteger (tag : string) value = writePrimitive writer tag (value.ToString())

            member __.WriteGuid (tag : string) value = writePrimitive writer tag (value.ToString())
            member __.WriteDate (tag : string) value = writePrimitive writer tag value
            member __.WriteTimeSpan (tag : string) value = writePrimitive writer tag (value.ToString())

            member __.WriteBytes (tag : string) value = 
                writer.WriteStartElement(tag)
                if obj.ReferenceEquals(value, null) then
                    writer.WriteAttributeString("null", "true")
                else
                    writer.WriteAttributeString("length", string value.Length)
                    writer.WriteBase64(value, 0, value.Length)
                writer.WriteEndElement()

            member __.IsPrimitiveArraySerializationSupported = false
            member __.WritePrimitiveArray _ _ = raise <| new NotSupportedException()

            member __.Dispose () = writer.Flush () ; textWriter.Flush () ; writer.Dispose()


    type XmlPickleReader internal (textReader : TextReader, leaveOpen) =

        let settings = new XmlReaderSettings()
        do
            settings.IgnoreWhitespace <- false
            settings.CloseInput <- not leaveOpen
            settings.CheckCharacters <- false

        let reader = XmlReader.Create(textReader, settings)

        interface IPickleFormatReader with
            
            member __.BeginReadRoot (tag : string) =
                do readElementName reader "FsPickler"

                let version = reader.["version"]
                if version <> AssemblyVersionInformation.Version then
                    let msg = sprintf "Invalid FsPickler version %s (expected %s)." version AssemblyVersionInformation.Version
                    raise <| new InvalidDataException(msg)

                let sTag = reader.["type"]
                if sTag <> tag then
                    raise <| new InvalidPickleTypeException(tag, sTag)

                if reader.IsEmptyElement || reader.Read () then ()
                else
                    raise <| new EndOfStreamException()

            member __.EndReadRoot () =
                if reader.IsEmptyElement then
                    let _ = reader.Read() in ()
                else
                    reader.ReadEndElement()

            member __.BeginReadObject (tag : string) =
                do readElementName reader tag

                let flags = parseFlagCsv <| reader.["flags"]

                if reader.IsEmptyElement || reader.Read() then ()
                else
                    raise <| new EndOfStreamException()

                flags

            member __.EndReadObject() = 
                if reader.IsEmptyElement then
                    let _ = reader.Read() in ()
                else
                    reader.ReadEndElement()

            member __.BeginReadBoundedSequence tag =
                do readElementName reader tag
                let length = reader.GetAttribute("length") |> int

                if not reader.IsEmptyElement then
                    if not <| reader.Read() then
                        raise <| new EndOfStreamException()

                length

            member __.EndReadBoundedSequence () =
                if reader.IsEmptyElement then
                    let _ = reader.Read() in ()
                else
                    reader.ReadEndElement()

            member __.BeginReadUnBoundedSequence tag =
                do readElementName reader tag

                if not reader.IsEmptyElement then
                    if not <| reader.Read() then
                        raise <| new EndOfStreamException()

            member __.ReadHasNextElement () =
                if reader.NodeType <> XmlNodeType.EndElement then true
                else
                    reader.ReadEndElement() ; false

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

            member __.ReadChar tag = readElementName reader tag ; reader.ReadElementContentAsString() |> unEscapeString |> char
            member __.ReadBigInteger tag = readElementName reader tag ; reader.ReadElementContentAsString() |> System.Numerics.BigInteger.Parse
            member __.ReadString tag = 
                readElementName reader tag 
                if reader.GetAttribute("null") = "true" then
                    reader.Read() |> ignore
                    null
                else
                    reader.ReadElementContentAsString() |> unEscapeString

            member __.ReadGuid tag = readElementName reader tag ; reader.ReadElementContentAsString() |> Guid.Parse
            member __.ReadDate tag = readElementName reader tag ; reader.ReadElementContentAsDateTime()
            member __.ReadTimeSpan tag = readElementName reader tag ; reader.ReadElementContentAsString () |> TimeSpan.Parse

            member __.ReadBytes tag =
                do readElementName reader tag
                if reader.GetAttribute("null") = "true" then 
                    reader.Read() |> ignore
                    null
                else
                    let length = reader.GetAttribute("length") |> int
                    let bytes = Array.zeroCreate<byte> length
                    do reader.Read() |> ignore
                    if length > 0 then
                        let n = reader.ReadContentAsBase64(bytes, 0, length)
                        if n < length then
                            raise <| new EndOfStreamException()

                        if reader.NodeType = XmlNodeType.Text then
                            reader.Read() |> ignore

                    bytes

            member __.IsPrimitiveArraySerializationSupported = false
            member __.ReadPrimitiveArray _ _ = raise <| new NotImplementedException()

            member __.Dispose () = reader.Dispose()


    type XmlPickleFormatProvider(indent) =

        static let defaultEnc e = match e with None -> Encoding.UTF8 | Some e -> e
        
        member val Indent = indent with get, set
            
        interface ITextPickleFormatProvider with
            member __.Name = "Xml"

            member __.DefaultEncoding = Encoding.UTF8

            member __.CreateWriter (stream, encoding, leaveOpen) =
                let sw = new StreamWriter(stream, encoding, 1024, leaveOpen)
                new XmlPickleWriter(sw, __.Indent, leaveOpen) :> _

            member __.CreateReader (stream, encoding, leaveOpen) =
                let sr = new StreamReader(stream, encoding, true, 1024, leaveOpen)
                new XmlPickleReader(sr, leaveOpen) :> _

            member __.CreateWriter (textWriter, leaveOpen) = new XmlPickleWriter(textWriter, __.Indent, leaveOpen) :> _
            member __.CreateReader (textReader, leaveOpen) = new XmlPickleReader(textReader, leaveOpen) :> _