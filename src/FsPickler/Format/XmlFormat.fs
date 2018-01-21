namespace MBrace.FsPickler

open System
open System.IO
open System.Text
open System.Xml
open System.Security

open Microsoft.FSharp.Core.LanguagePrimitives

module private XmlUtils =

    // past format versions

    [<Literal>]
    let Formatv0960 = "0.9.6.0" // as defined in FsPickler 0.9.6.0

    [<Literal>]
    let Formatv1200 = "1.2.0.0" // as defined in FsPickler 1.2.0.0 

    [<Literal>]
    let Formatv1400 = "1.4.0.0" // as defined in FsPickler 1.4.0.0

    [<Literal>]
    let Formatv2000 = "2.0.0.0" // as defined in FsPickler 2.0.0.0 

    [<Literal>]
    let Formatv4000 = "4.0.0.0" // as defined in FsPickler 4.0.0.0 

    let inline escapeString (value : string) = SecurityElement.Escape value
    let inline unEscapeString (value : string) =
        let e = new SecurityElement("", value)
        e.Text

    let inline mkFlagCsv (flags : ObjectFlags) =
        let tokens = new ResizeArray<string>(2)

        if Enum.hasFlag flags ObjectFlags.IsNull then
            tokens.Add "null"

        if Enum.hasFlag flags ObjectFlags.IsCachedInstance then
            tokens.Add "cached"

        if Enum.hasFlag flags ObjectFlags.IsCyclicInstance then
            tokens.Add "cyclic"

        if Enum.hasFlag flags ObjectFlags.IsProperSubtype then
            tokens.Add "subtype"

        if Enum.hasFlag flags ObjectFlags.IsSequenceHeader then
            tokens.Add "sequence"

        if Enum.hasFlag flags ObjectFlags.IsSiftedValue then
            tokens.Add "hole"

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
            | "sequence" -> flags <- flags ||| ObjectFlags.IsSequenceHeader
            | "hole" -> flags <- flags ||| ObjectFlags.IsSiftedValue
            | _ -> raise <| new FormatException(sprintf "invalid pickle flag '%s'." t)

        flags

    let inline writePrimitive (w : ^XmlWriter) (tag : string) (value : ^T) =
        (^XmlWriter : (member WriteStartElement : string -> unit) (w, tag))
        (^XmlWriter : (member WriteValue : ^T -> unit) (w, value))
        (^XmlWriter : (member WriteEndElement : unit -> unit) w)

    type XmlReader with
        member inline r.MoveNext () =
            if r.Read() then ()
            else
                raise <| new FormatException("Xml document ended prematurely.")

        member inline r.SkipWhitespace() =
            while
                (match r.NodeType with
                | XmlNodeType.Whitespace
                | XmlNodeType.SignificantWhitespace
                | XmlNodeType.Comment -> true
                | _ -> false) 
                    
                do r.MoveNext()

        member inline r.ReadElementName (tag : string) =
            do r.SkipWhitespace()

            if r.NodeType <> XmlNodeType.Element then
                let msg = sprintf "expected token '%O' but was '%O'." XmlNodeType.Element r.NodeType
                raise <| new FormatException(msg)

            elif r.Name <> tag then
                let msg = sprintf "expected element '%s' but was '%s'." tag r.Name
                raise <| new FormatException(msg)


open XmlUtils

/// <summary>
///     Xml format serializer.
/// </summary>
type XmlPickleWriter internal (textWriter : TextWriter, indent : bool, leaveOpen : bool) =

    let settings = new XmlWriterSettings()
    do 
        settings.NewLineHandling <- NewLineHandling.Entitize
        settings.CloseOutput <- not leaveOpen
        settings.Indent <- indent
        settings.CheckCharacters <- false
        settings.ConformanceLevel <- ConformanceLevel.Auto

    let writer = XmlWriter.Create(textWriter, settings)
        
    interface IPickleFormatWriter with
        member __.Flush () = writer.Flush()

        member __.BeginWriteRoot (tag : string) = 
            writer.WriteStartDocument()
            writer.WriteStartElement("FsPickler")
            writer.WriteAttributeString("version", Formatv4000)
            writer.WriteAttributeString("type", tag)

        member __.EndWriteRoot () = 
            writer.WriteEndElement()
            writer.WriteEndDocument()

        member __.BeginWriteObject (tag : string) (flags : ObjectFlags) =
            writer.WriteStartElement(tag)

            if flags <> ObjectFlags.None then
                writer.WriteAttributeString("flags", mkFlagCsv flags)

        member __.EndWriteObject () = writer.WriteEndElement()

        member __.SerializeUnionCaseNames = true
        member __.UseNamedEnumSerialization = true

        member __.PreferLengthPrefixInSequences = false
        member __.WriteNextSequenceElement _ = ()

        member __.WriteCachedObjectId id = writer.WriteAttributeString("id", string id)

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
            if isNull value then
                writer.WriteStartElement(tag)
                writer.WriteAttributeString("null", "true")
                writer.WriteEndElement()
            else
                writePrimitive writer tag <| escapeString value

        member __.WriteBigInteger (tag : string) value = writePrimitive writer tag (value.ToString())

        member __.WriteGuid (tag : string) value = writePrimitive writer tag (value.ToString())
        member __.WriteDateTime (tag : string) value = 
            writer.WriteStartElement tag
            writer.WriteString(XmlConvert.ToString(value, XmlDateTimeSerializationMode.RoundtripKind))
            writer.WriteEndElement()

        member __.WriteDateTimeOffset (tag : string) value = writePrimitive writer tag value
        member __.WriteTimeSpan (tag : string) value = writePrimitive writer tag (value.ToString())

        member __.WriteBytes (tag : string) value = 
            writer.WriteStartElement(tag)
            if isNull value then
                writer.WriteAttributeString("null", "true")
            else
                writer.WriteAttributeString("size", string value.Length)
                if value.Length > 0 then writer.WriteBase64(value, 0, value.Length)

            writer.WriteEndElement()

        member __.IsPrimitiveArraySerializationSupported = false
        member __.WritePrimitiveArray _ _ = raise <| new NotSupportedException()

        member __.Dispose () = writer.Dispose()

/// <summary>
///     Xml format deserializer.
/// </summary>
type XmlPickleReader internal (textReader : TextReader, leaveOpen) =

    let settings = new XmlReaderSettings()
    do
        settings.IgnoreWhitespace <- false
        settings.CloseInput <- not leaveOpen
        settings.CheckCharacters <- false

    let reader = XmlReader.Create(textReader, settings)

    let mutable isAtEmptySequenceHeader = false
    let mutable cachedObjectId = 0L

    interface IPickleFormatReader with
            
        member __.BeginReadRoot (tag : string) =
            do reader.MoveNext() // initial read
            do reader.SkipWhitespace()
            if reader.NodeType = XmlNodeType.XmlDeclaration then
                reader.MoveNext ()

            reader.ReadElementName "FsPickler"

            let version = reader.["version"]
            if version <> Formatv4000 then
                let v = Version(version)
                if version = Formatv0960 || version = Formatv1200 || version = Formatv2000 then
                    let msg = sprintf "XML format version %O no longer supported." version
                    raise <| new FormatException(msg)
                else
                    let msg = sprintf "Unrecognized XML format version %O." v
                    raise <| new FormatException(msg)

            let sTag = reader.["type"]
            if sTag <> tag then
                raise <| new InvalidPickleTypeException(tag, sTag)

            if reader.IsEmptyElement || reader.Read () then ()
            else
                raise <| new EndOfStreamException()

        member __.EndReadRoot () =
            if reader.IsEmptyElement then
                reader.MoveNext()
            else
                reader.ReadEndElement()

        member __.BeginReadObject (tag : string) =
            do reader.ReadElementName tag

            let flags = parseFlagCsv <| reader.["flags"]

            if Enum.hasFlag flags ObjectFlags.IsSequenceHeader then
                isAtEmptySequenceHeader <- reader.IsEmptyElement

            match reader.["id"] with
            | null -> ()
            | id -> cachedObjectId <- int64 id

            if reader.IsEmptyElement || reader.Read () then ()
            else
                raise <| new EndOfStreamException()

            flags

        member __.EndReadObject() =
            if reader.IsEmptyElement then
                reader.MoveNext()
                isAtEmptySequenceHeader <- false
            else
                reader.ReadEndElement()

        member __.ReadCachedObjectId () = cachedObjectId

        member __.SerializeUnionCaseNames = true
        member __.UseNamedEnumSerialization = true

        member __.PreferLengthPrefixInSequences = false
        member __.ReadNextSequenceElement () =
            reader.SkipWhitespace ()
            if isAtEmptySequenceHeader then
                isAtEmptySequenceHeader <- false
                false
            else
                reader.NodeType <> XmlNodeType.EndElement

        member __.ReadBoolean tag = reader.ReadElementName tag ; reader.ReadElementContentAsBoolean()
        member __.ReadByte tag = reader.ReadElementName tag ; reader.ReadElementContentAsInt() |> byte
        member __.ReadSByte tag = reader.ReadElementName tag ; reader.ReadElementContentAsInt() |> sbyte

        member __.ReadInt16 tag = reader.ReadElementName tag ; reader.ReadElementContentAsInt() |> int16
        member __.ReadInt32 tag = reader.ReadElementName tag ; reader.ReadElementContentAsInt()
        member __.ReadInt64 tag = reader.ReadElementName tag ; reader.ReadElementContentAsLong()

        member __.ReadUInt16 tag = reader.ReadElementName tag ; reader.ReadElementContentAsInt() |> uint16
        member __.ReadUInt32 tag = reader.ReadElementName tag ; reader.ReadElementContentAsInt() |> uint32
        member __.ReadUInt64 tag = reader.ReadElementName tag ; reader.ReadElementContentAsLong() |> uint64

        member __.ReadDecimal tag = reader.ReadElementName tag ; reader.ReadElementContentAsDecimal()
        member __.ReadSingle tag = reader.ReadElementName tag ; reader.ReadElementContentAsFloat()
        member __.ReadDouble tag = reader.ReadElementName tag ; reader.ReadElementContentAsDouble()

        member __.ReadChar tag = reader.ReadElementName tag ; reader.ReadElementContentAsString() |> unEscapeString |> char

        member __.ReadBigInteger tag = reader.ReadElementName tag ; reader.ReadElementContentAsString() |> System.Numerics.BigInteger.Parse

        member __.ReadString tag = 
            reader.ReadElementName tag 
            if reader.GetAttribute("null") = "true" then
                reader.MoveNext()
                null
            else
                reader.ReadElementContentAsString() |> unEscapeString

        member __.ReadGuid tag = reader.ReadElementName tag ; let textGuid = reader.ReadElementContentAsString() in new Guid(textGuid)
        member __.ReadDateTime tag = 
            reader.ReadElementName tag 
            let dateTimeString = reader.ReadElementContentAsString()
            XmlConvert.ToDateTime(dateTimeString, XmlDateTimeSerializationMode.RoundtripKind)

        member __.ReadDateTimeOffset tag = reader.ReadElementName tag ; reader.ReadElementContentAsString() |> DateTimeOffset.Parse
        member __.ReadTimeSpan tag = reader.ReadElementName tag ; reader.ReadElementContentAsString () |> TimeSpan.Parse

        member __.ReadBytes tag =
            do reader.ReadElementName tag
            if reader.GetAttribute("null") = "true" then 
                reader.MoveNext()
                null
            else
                let length = reader.GetAttribute("size") |> int
                let bytes = Array.zeroCreate<byte> length
                do reader.MoveNext()
                if length > 0 then
                    let n = reader.ReadContentAsBase64(bytes, 0, length)
                    if n < length then
                        raise <| new InvalidDataException()

                    if reader.NodeType = XmlNodeType.Text then
                        reader.MoveNext()

                bytes

        member __.IsPrimitiveArraySerializationSupported = false
        member __.ReadPrimitiveArray _ _ = raise <| new NotImplementedException()

        member __.Dispose () = reader.Dispose()

/// <summary>
///     Factory methods for the Xml serialization format.
/// </summary>
type XmlPickleFormatProvider(indent) =
        
    member val Indent = indent with get, set
            
    interface ITextPickleFormatProvider with
        member __.Name = "Xml"

        member __.DefaultEncoding = Encoding.UTF8

        member __.CreateWriter (stream, encoding, _, leaveOpen) =
            let sw = new StreamWriter(stream, encoding, 1024, leaveOpen)
            new XmlPickleWriter(sw, __.Indent, leaveOpen) :> _

        member __.CreateReader (stream, encoding, _, leaveOpen) =
            let sr = new StreamReader(stream, encoding, true, 1024, leaveOpen)
            new XmlPickleReader(sr, leaveOpen) :> _

        member __.CreateWriter (textWriter, _, leaveOpen) = new XmlPickleWriter(textWriter, __.Indent, leaveOpen) :> _
        member __.CreateReader (textReader, _, leaveOpen) = new XmlPickleReader(textReader, leaveOpen) :> _