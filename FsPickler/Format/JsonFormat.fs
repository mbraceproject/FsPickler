namespace Nessos.FsPickler

    open System
    open System.IO
    open System.Text

    open Newtonsoft.Json

    module private JsonUtils =

        let inline writePrimitive (jsonWriter : ^JsonWriter) (name : string) (value : ^T) =
            ( ^JsonWriter : (member WritePropertyName : string -> unit) (jsonWriter, name))
            ( ^JsonWriter : (member WriteValue : ^T -> unit) (jsonWriter, value))

        type JsonReader with
            member inline jsonReader.ReadProperty (name : string) =
                if jsonReader.TokenType = JsonToken.PropertyName then
                    let jsonName = jsonReader.Value |> fastUnbox<string>
                    if name <> jsonName then
                        let msg = sprintf "expected '%s' but was '%s'." name jsonName
                        raise <| new InvalidDataException(msg)
                else
                    let msg = sprintf "expected '%O' but was '%O'." JsonToken.PropertyName jsonReader.TokenType
                    raise <| new InvalidDataException(msg)

            member inline jsonReader.ValueAs<'T> () = jsonReader.Value |> fastUnbox<'T>

            member inline jsonReader.ReadStartObject () =
                if jsonReader.Read() && jsonReader.TokenType = JsonToken.StartObject then
                    jsonReader.Read() |> ignore
                else
                    raise <| new InvalidDataException("invalid json format.")

            member inline jsonReader.ReadEndObject () =
                if jsonReader.Read() && jsonReader.TokenType = JsonToken.EndObject then ()
                else
                    raise <| new InvalidDataException("invalid json format.")

            member inline jsonReader.ReadAs<'T> () = 
                jsonReader.Read() |> ignore
                let value = jsonReader.Value |> fastUnbox<'T>
                jsonReader.Read() |> ignore
                value

    open JsonUtils

    type JsonPickleWriter internal (stream : Stream, encoding : Encoding, indented, leaveOpen) =
        
        let sw = new StreamWriter(stream, encoding, 1024, leaveOpen)
        let jsonWriter = new JsonTextWriter(sw) :> JsonWriter
        do jsonWriter.Formatting <- if indented then Formatting.Indented else Formatting.None

        interface IPickleFormatWriter with
            
            member __.BeginWriteRoot (tag : string) =
                jsonWriter.WriteStartObject()
                writePrimitive jsonWriter "FsPickler" AssemblyVersionInformation.Version
                writePrimitive jsonWriter "type" tag

            member __.EndWriteRoot () = jsonWriter.WriteEnd()

            member __.BeginWriteObject (_ : TypeInfo) (_ : PicklerInfo) (tag : string) (flags : ObjectFlags) =

                jsonWriter.WritePropertyName tag
                jsonWriter.WriteStartObject()

                if ObjectFlags.hasFlag flags ObjectFlags.IsNull then 
                    writePrimitive jsonWriter "null" true
                if ObjectFlags.hasFlag flags ObjectFlags.IsProperSubtype then 
                    writePrimitive jsonWriter "isSubtype" true
                if ObjectFlags.hasFlag flags ObjectFlags.IsCachedInstance then 
                    writePrimitive jsonWriter "cached" true
                if ObjectFlags.hasFlag flags ObjectFlags.IsCyclicInstance then 
                    writePrimitive jsonWriter "cyclic" true
                if ObjectFlags.hasFlag flags ObjectFlags.IsSequenceHeader then 
                    writePrimitive jsonWriter "sequence" true

            member __.EndWriteObject () = jsonWriter.WriteEnd()

            member __.WriteBoolean (tag : string) value = writePrimitive jsonWriter tag value
            member __.WriteByte (tag : string) value = writePrimitive jsonWriter tag value
            member __.WriteSByte (tag : string) value = writePrimitive jsonWriter tag value

            member __.WriteInt16 (tag : string) value = writePrimitive jsonWriter tag value
            member __.WriteInt32 (tag : string) value = writePrimitive jsonWriter tag value
            member __.WriteInt64 (tag : string) value = writePrimitive jsonWriter tag value

            member __.WriteUInt16 (tag : string) value = writePrimitive jsonWriter tag value
            member __.WriteUInt32 (tag : string) value = writePrimitive jsonWriter tag value
            member __.WriteUInt64 (tag : string) value = writePrimitive jsonWriter tag value

            member __.WriteSingle (tag : string) value = writePrimitive jsonWriter tag value
            member __.WriteDouble (tag : string) value = writePrimitive jsonWriter tag value
            member __.WriteDecimal (tag : string) value = writePrimitive jsonWriter tag value

            member __.WriteChar (tag : string) value = writePrimitive jsonWriter tag value
            member __.WriteString (tag : string) value = writePrimitive jsonWriter tag value

            member __.WriteBytes (tag : string) (value : byte []) = writePrimitive jsonWriter tag value
            member __.WriteBytesFixed (tag : string) (value : byte []) = writePrimitive jsonWriter tag value

            member __.IsPrimitiveArraySerializationSupported = false
            member __.WritePrimitiveArray _ _ = raise <| NotSupportedException()

            member __.Dispose () = 
                jsonWriter.Flush () ; (jsonWriter :> IDisposable).Dispose()


    type JsonPickleReader internal (stream : Stream, encoding : Encoding, leaveOpen) =
        
        let sr = new StreamReader(stream, encoding, true, 1024, leaveOpen)
        let jsonReader = new JsonTextReader(sr) :> JsonReader

        interface IPickleFormatReader with
            
            member __.BeginReadRoot (tag : string) =
                do jsonReader.ReadStartObject ()

                jsonReader.ReadProperty "FsPickler"
                let version = jsonReader.ReadAs<string> ()
                if version <> AssemblyVersionInformation.Version then
                    raise <| new InvalidDataException(sprintf "Invalid FsPickler version %s." version)

                jsonReader.ReadProperty "type"
                let id = jsonReader.ReadAs<string> ()
                if id <> tag then
                    let msg = sprintf "expected '%s' but was '%s'." tag id
                    raise <| new InvalidDataException()

            member __.EndReadRoot () = jsonReader.Read() |> ignore

            member __.BeginReadObject (_ : TypeInfo) (_ : PicklerInfo) (tag : string) =
                jsonReader.ReadProperty tag
                jsonReader.ReadStartObject ()
                    
                let mutable objectFlags = ObjectFlags.None
                // temp solution ; this is wrong
                let mutable complete = false

                while not complete do
                    match jsonReader.ValueAs<string> () with
                    | "null" -> 
                        if jsonReader.ReadAs<bool> () then
                            objectFlags <- objectFlags ||| ObjectFlags.IsNull
                    | "isSubtype" ->
                        if jsonReader.ReadAs<bool> () then
                            objectFlags <- objectFlags ||| ObjectFlags.IsProperSubtype
                    | "cached" ->
                        if jsonReader.ReadAs<bool> () then
                            objectFlags <- objectFlags ||| ObjectFlags.IsCachedInstance
                    | "cyclic" ->
                        if jsonReader.ReadAs<bool> () then
                            objectFlags <- objectFlags ||| ObjectFlags.IsCyclicInstance
                    | "sequence" ->
                        if jsonReader.ReadAs<bool> () then
                            objectFlags <- objectFlags ||| ObjectFlags.IsSequenceHeader
                    | _ -> 
                        complete <- true

                objectFlags

            member __.EndReadObject () = jsonReader.Read () |> ignore

            member __.ReadBoolean tag = jsonReader.ReadProperty tag ; jsonReader.ReadAs<bool> ()

            member __.ReadByte tag = jsonReader.ReadProperty tag ; jsonReader.ReadAs<int64> () |> byte
            member __.ReadSByte tag = jsonReader.ReadProperty tag ; jsonReader.ReadAs<int64> () |> sbyte

            member __.ReadInt16 tag = jsonReader.ReadProperty tag ; jsonReader.ReadAs<int64> () |> int16
            member __.ReadInt32 tag = jsonReader.ReadProperty tag ; jsonReader.ReadAs<int64> () |> int
            member __.ReadInt64 tag = jsonReader.ReadProperty tag ; jsonReader.ReadAs<int64> ()

            member __.ReadUInt16 tag = jsonReader.ReadProperty tag ; jsonReader.ReadAs<int64> () |> uint16
            member __.ReadUInt32 tag = jsonReader.ReadProperty tag ; jsonReader.ReadAs<int64> () |> uint32
            member __.ReadUInt64 tag = jsonReader.ReadProperty tag ; jsonReader.ReadAs<int64> () |> uint64

            member __.ReadSingle tag = jsonReader.ReadProperty tag ; jsonReader.ReadAs<double> () |> single
            member __.ReadDouble tag = jsonReader.ReadProperty tag ; jsonReader.ReadAs<double> ()

            member __.ReadChar tag = jsonReader.ReadProperty tag ; jsonReader.ReadAs<string>().[0]
            member __.ReadString tag = jsonReader.ReadProperty tag ; jsonReader.ReadAs<string>()

            member __.ReadDecimal tag = 
                jsonReader.ReadProperty tag
                let d = jsonReader.ReadAsDecimal().Value
                jsonReader.Read() |> ignore
                d

            member __.ReadBytes tag = 
                jsonReader.ReadProperty tag 
                let bytes = jsonReader.ReadAsBytes() 
                jsonReader.Read() |> ignore 
                bytes

            member __.ReadBytesFixed tag _ = 
                jsonReader.ReadProperty tag
                let bytes = jsonReader.ReadAsBytes()
                jsonReader.Read() |> ignore
                bytes

            member __.IsPrimitiveArraySerializationSupported = false
            member __.ReadPrimitiveArray _ _ = raise <| new NotImplementedException()

            member __.Dispose () = (jsonReader :> IDisposable).Dispose() ; sr.Dispose()

    type JsonPickleFormatProvider (?encoding : Encoding, ?indented, ?leaveOpen) =
        let encoding = defaultArg encoding Encoding.UTF8
        let leaveOpen = defaultArg leaveOpen true
        let indented = defaultArg indented false

        interface IPickleFormatProvider with
            member __.CreateWriter(stream) = new JsonPickleWriter(stream, encoding, indented, leaveOpen) :> _
            member __.CreateReader(stream) = new JsonPickleReader(stream, encoding, leaveOpen) :> _