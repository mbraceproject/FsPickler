namespace Nessos.FsPickler

    open System
    open System.Collections.Generic
    open System.Globalization
    open System.IO
    open System.Numerics
    open System.Text

    open Newtonsoft.Json

    module private JsonUtils =

        let inline mkFlagCsv (flags : ObjectFlags) =
            let tokens = new ResizeArray<string>()
            if ObjectFlags.hasFlag flags ObjectFlags.IsCachedInstance then
                tokens.Add "cached"

            if ObjectFlags.hasFlag flags ObjectFlags.IsCyclicInstance then
                tokens.Add "cyclic"

            if ObjectFlags.hasFlag flags ObjectFlags.IsProperSubtype then
                tokens.Add "subtype"

            String.concat "," tokens

        let inline parseFlagCsv (csv : string) =
            let mutable flags = ObjectFlags.None
            let tokens = csv.Split(',')
            for t in tokens do
                match t with
                | "cached" -> flags <- flags ||| ObjectFlags.IsCachedInstance
                | "cyclic" -> flags <- flags ||| ObjectFlags.IsCyclicInstance
                | "subtype" -> flags <- flags ||| ObjectFlags.IsProperSubtype
                | _ -> raise <| new InvalidDataException(sprintf "invalid pickle flag '%s'." t)

            flags
                

        let inline invalidFormat () = raise <| new InvalidDataException("invalid json format.")

        let inline writePrimitive (jsonWriter : ^JsonWriter) ignoreName (name : string) (value : ^T) =
            if not ignoreName then
                ( ^JsonWriter : (member WritePropertyName : string -> unit) (jsonWriter, name))
            ( ^JsonWriter : (member WriteValue : ^T -> unit) (jsonWriter, value))

        type JsonReader with
            member inline jsonReader.ReadProperty (name : string) =
                if jsonReader.TokenType = JsonToken.PropertyName then
                    let jsonName = jsonReader.Value |> fastUnbox<string>
                    if name <> jsonName then
                        let msg = sprintf "expected property '%s' but was '%s'." name jsonName
                        raise <| new InvalidDataException(msg)
                else
                    let msg = sprintf "expected token '%O' but was '%O'." JsonToken.PropertyName jsonReader.TokenType
                    raise <| new InvalidDataException(msg)

            member inline jsonReader.ValueAs<'T> () = jsonReader.Value |> fastUnbox<'T>

            member inline jsonReader.ReadPrimitiveAs<'T> ignoreName (name : string) =
                if not ignoreName then
                    jsonReader.ReadProperty name
                    jsonReader.Read() |> ignore
                
                let v = jsonReader.ValueAs<'T> ()
                jsonReader.Read() |> ignore
                v
            
            member inline jsonReader.MoveNext () = 
                if jsonReader.Read() then ()
                else
                    raise <| new EndOfStreamException()

            /// returns true iff null token
            member inline jsonReader.ReadStartObject () =
                match jsonReader.TokenType with
                | JsonToken.Null ->
                    jsonReader.Read() |> ignore
                    true
                | JsonToken.StartObject ->
                    jsonReader.Read() |> ignore
                    false
                | _ ->
                    invalidFormat ()

            member inline jsonReader.ReadEndObject () =
                if jsonReader.Read() && jsonReader.TokenType = JsonToken.EndObject then ()
                else
                    invalidFormat ()

    open JsonUtils

    type JsonPickleWriter internal (textWriter : TextWriter, indented, leaveOpen) =
        
        let jsonWriter = new JsonTextWriter(textWriter) :> JsonWriter
        do 
            jsonWriter.Formatting <- if indented then Formatting.Indented else Formatting.None
            jsonWriter.CloseOutput <- not leaveOpen

        let mutable currentValueIsNull = false

        let mutable depth = 0
        let arrayStack = new Stack<int> ()
        do arrayStack.Push Int32.MinValue
        let isArrayElement () =
            if arrayStack.Peek() = depth - 1 then true
            else
                false

        interface IPickleFormatWriter with
            
            member __.BeginWriteRoot (tag : string) =
                jsonWriter.WriteStartObject()
                writePrimitive jsonWriter false "FsPickler" AssemblyVersionInformation.Version
                writePrimitive jsonWriter false "type" tag

            member __.EndWriteRoot () = jsonWriter.WriteEnd()

            member __.BeginWriteObject (tag : string) (flags : ObjectFlags) =

                if not <| isArrayElement () then
                    jsonWriter.WritePropertyName tag

                if ObjectFlags.hasFlag flags ObjectFlags.IsNull then
                    currentValueIsNull <- true
                    jsonWriter.WriteNull()
                else
                    jsonWriter.WriteStartObject()
                    depth <- depth + 1

                    if flags = ObjectFlags.None then ()
                    else
                        let flagCsv = mkFlagCsv flags
                        writePrimitive jsonWriter false "pickle flags" flagCsv

            member __.EndWriteObject () = 
                if currentValueIsNull then 
                    currentValueIsNull <- false
                else
                    depth <- depth - 1
                    jsonWriter.WriteEndObject()

            member __.BeginWriteBoundedSequence (tag : string) (length : int) =
                arrayStack.Push depth
                depth <- depth + 1

                writePrimitive jsonWriter false "length" length
                jsonWriter.WritePropertyName tag
                
                jsonWriter.WriteStartArray()

            member __.EndWriteBoundedSequence () =
                depth <- depth - 1
                arrayStack.Pop () |> ignore
                jsonWriter.WriteEndArray ()

            member __.BeginWriteUnBoundedSequence (tag : string) =
                if not <| isArrayElement () then
                    jsonWriter.WritePropertyName tag

                arrayStack.Push depth
                depth <- depth + 1

                jsonWriter.WriteStartArray()

            member __.WriteHasNextElement hasNext =
                if not hasNext then 
                    arrayStack.Pop () |> ignore
                    depth <- depth - 1
                    jsonWriter.WriteEndArray ()

            member __.WriteBoolean (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag value
            member __.WriteByte (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag value
            member __.WriteSByte (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag value

            member __.WriteInt16 (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag value
            member __.WriteInt32 (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag value
            member __.WriteInt64 (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag value

            member __.WriteUInt16 (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag value
            member __.WriteUInt32 (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag value
            member __.WriteUInt64 (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag value

            member __.WriteSingle (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag value
            member __.WriteDouble (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag value
            member __.WriteDecimal (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag <| string value

            member __.WriteChar (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag value
            member __.WriteString (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag value
            member __.WriteBigInteger (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag (value.ToString())

            member __.WriteGuid (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag value
            member __.WriteDate (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag value
            member __.WriteTimeSpan (tag : string) value = writePrimitive jsonWriter (isArrayElement ()) tag <| value.ToString()

            member __.WriteBytes (tag : string) (value : byte []) = writePrimitive jsonWriter (isArrayElement ()) tag value

            member __.IsPrimitiveArraySerializationSupported = false
            member __.WritePrimitiveArray _ _ = raise <| NotSupportedException()

            member __.Dispose () = 
                jsonWriter.Flush () ; textWriter.Flush () ; (jsonWriter :> IDisposable).Dispose()


    type JsonPickleReader internal (textReader : TextReader, leaveOpen) =

        let jsonReader = new JsonTextReader(textReader) :> JsonReader
        do
            jsonReader.CloseInput <- not leaveOpen

        let mutable currentValueIsNull = false

        let mutable depth = 0
        let arrayStack = new Stack<int> ()
        do arrayStack.Push Int32.MinValue
        let isArrayElement () =
            if arrayStack.Peek() = depth - 1 then true
            else
                false

        interface IPickleFormatReader with
            
            member __.BeginReadRoot () =
                do jsonReader.MoveNext()

                if jsonReader.ReadStartObject () then raise <| new InvalidDataException("invalid json root object.")
                else
                    let version = jsonReader.ReadPrimitiveAs<string> false "FsPickler"
                    if version <> AssemblyVersionInformation.Version then
                        raise <| new InvalidDataException(sprintf "Invalid FsPickler version %s." version)

                    jsonReader.ReadPrimitiveAs<string> false "type"

            member __.EndReadRoot () = jsonReader.Read() |> ignore

            member __.BeginReadObject (tag : string) =
                
                if not <| isArrayElement () then
                    jsonReader.ReadProperty tag
                    jsonReader.MoveNext ()

                depth <- depth + 1

                if jsonReader.ReadStartObject () then 
                    currentValueIsNull <- true
                    ObjectFlags.IsNull
                else
                    if jsonReader.ValueAs<string> () = "pickle flags" then
                        jsonReader.MoveNext()
                        let csvFlags = jsonReader.ValueAs<string>()
                        jsonReader.MoveNext()
                        parseFlagCsv csvFlags
                    else
                        ObjectFlags.None

            member __.EndReadObject () =
                depth <- depth - 1

                if currentValueIsNull then 
                    currentValueIsNull <- false
                else 
                    jsonReader.MoveNext()

            member __.BeginReadBoundedSequence tag =
                arrayStack.Push depth
                depth <- depth + 1

                let length = jsonReader.ReadPrimitiveAs<int64> false "length"
                jsonReader.ReadProperty tag
                jsonReader.MoveNext()

                if jsonReader.TokenType = JsonToken.StartArray then
                    jsonReader.MoveNext()
                    int length
                else
                    raise <| new InvalidDataException("expected array.")

            member __.EndReadBoundedSequence () =
                if jsonReader.TokenType = JsonToken.EndArray && jsonReader.Read () then
                    arrayStack.Pop () |> ignore
                    depth <- depth - 1
                else
                    raise <| InvalidDataException("expected end of array.")

            member __.BeginReadUnBoundedSequence tag =
                arrayStack.Push depth
                depth <- depth + 1

                jsonReader.ReadProperty tag

                if jsonReader.Read () && jsonReader.TokenType = JsonToken.StartArray then
                    jsonReader.MoveNext()
                else
                    raise <| new InvalidDataException("expected array.")

            member __.ReadHasNextElement () =
                if jsonReader.TokenType = JsonToken.EndArray && jsonReader.Read () then
                    arrayStack.Pop () |> ignore
                    depth <- depth - 1
                    false
                else
                    true

            member __.ReadBoolean tag = jsonReader.ReadPrimitiveAs<bool> (isArrayElement ()) tag

            member __.ReadByte tag = jsonReader.ReadPrimitiveAs<int64> (isArrayElement ()) tag |> byte
            member __.ReadSByte tag = jsonReader.ReadPrimitiveAs<int64> (isArrayElement ()) tag |> sbyte

            member __.ReadInt16 tag = jsonReader.ReadPrimitiveAs<int64> (isArrayElement ()) tag |> int16
            member __.ReadInt32 tag = jsonReader.ReadPrimitiveAs<int64> (isArrayElement ()) tag |> int
            member __.ReadInt64 tag = jsonReader.ReadPrimitiveAs<int64> (isArrayElement ()) tag

            member __.ReadUInt16 tag = jsonReader.ReadPrimitiveAs<int64> (isArrayElement ()) tag |> uint16
            member __.ReadUInt32 tag = jsonReader.ReadPrimitiveAs<int64> (isArrayElement ()) tag |> uint32
            member __.ReadUInt64 tag = jsonReader.ReadPrimitiveAs<int64> (isArrayElement ()) tag |> uint64

            member __.ReadSingle tag =
                if not <| isArrayElement () then
                    jsonReader.ReadProperty tag
                    jsonReader.MoveNext()

                let value =
                    match jsonReader.TokenType with
                    | JsonToken.Float -> jsonReader.ValueAs<double> () |> single
                    | JsonToken.String -> Single.Parse(jsonReader.ValueAs<string>(), CultureInfo.InvariantCulture)
                    | _ -> raise <| new InvalidDataException("not a float.")
                jsonReader.MoveNext()
                value
                
            member __.ReadDouble tag = 
                if not <| isArrayElement () then
                    jsonReader.ReadProperty tag
                    jsonReader.MoveNext()

                let value =
                    match jsonReader.TokenType with
                    | JsonToken.Float -> jsonReader.ValueAs<double> ()
                    | JsonToken.String -> Double.Parse(jsonReader.ValueAs<string>(), CultureInfo.InvariantCulture)
                    | _ -> raise <| new InvalidDataException("not a float.")
                jsonReader.MoveNext()
                value

            member __.ReadChar tag = let value = jsonReader.ReadPrimitiveAs<string> (isArrayElement ()) tag in value.[0]
            member __.ReadString tag = jsonReader.ReadPrimitiveAs<string> (isArrayElement ()) tag
            member __.ReadBigInteger tag = jsonReader.ReadPrimitiveAs<string> (isArrayElement ()) tag |> BigInteger.Parse

            member __.ReadGuid tag = jsonReader.ReadPrimitiveAs<string> (isArrayElement ()) tag |> Guid.Parse
            member __.ReadTimeSpan tag = jsonReader.ReadPrimitiveAs<string> (isArrayElement ()) tag |> TimeSpan.Parse
            member __.ReadDate tag = jsonReader.ReadPrimitiveAs<DateTime> (isArrayElement ()) tag

            member __.ReadDecimal tag = jsonReader.ReadPrimitiveAs<string> (isArrayElement ()) tag |> decimal

            member __.ReadBytes tag = 
                match jsonReader.ReadPrimitiveAs<string> (isArrayElement ()) tag with
                | null -> null
                | value -> Convert.FromBase64String value

            member __.IsPrimitiveArraySerializationSupported = false
            member __.ReadPrimitiveArray _ _ = raise <| new NotImplementedException()

            member __.Dispose () = (jsonReader :> IDisposable).Dispose()

    type JsonPickleFormatProvider (?indent) =
        let indent = defaultArg indent false

        interface IStringPickleFormatProvider with
            member __.Name = "Json"

            member __.CreateWriter (stream, encoding, leaveOpen) =
                let sw = new StreamWriter(stream, encoding, 1024, leaveOpen)
                new JsonPickleWriter(sw, indent, leaveOpen) :> _

            member __.CreateReader (stream, encoding, leaveOpen) =
                let sr = new StreamReader(stream, encoding, true, 1024, leaveOpen)
                new JsonPickleReader(sr, leaveOpen) :> _

            member __.CreateWriter (textWriter, leaveOpen) = new JsonPickleWriter(textWriter, indent, leaveOpen) :> _
            member __.CreateReader (textReader, leaveOpen) = new JsonPickleReader(textReader, leaveOpen) :> _