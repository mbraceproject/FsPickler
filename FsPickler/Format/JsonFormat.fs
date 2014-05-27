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
            member (*inline*) jsonReader.ReadProperty (name : string) =
                if jsonReader.Read() then
                    if jsonReader.TokenType = JsonToken.PropertyName then
                        let jsonName = jsonReader.Value |> fastUnbox<string>
                        if name <> jsonName then
                            let msg = sprintf "expected '%s' but was '%s'." name jsonName
                            raise <| new InvalidDataException(msg)

                        jsonReader.Read () |> ignore
                    else
                        let msg = sprintf "expected '%O' but was '%O'." JsonToken.PropertyName jsonReader.TokenType
                        raise <| new InvalidDataException(msg)
                else
                    raise <| new InvalidDataException("invalid json format.")

            member inline jsonReader.ValueAs<'T> () = jsonReader.Value |> fastUnbox<'T>
                

    open JsonUtils

    type JsonPickleWriter (stream : Stream, encoding : Encoding, indented) =
        
        let sw = new StreamWriter(stream, encoding)
        let jsonWriter = new JsonTextWriter(sw) :> JsonWriter
        do jsonWriter.Formatting <- if indented then Formatting.Indented else Formatting.None

        interface IPickleFormatWriter with
            
            member __.BeginWriteRoot (tag : string) =
                jsonWriter.WriteStartObject()
                writePrimitive jsonWriter "FsPickler" AssemblyVersionInformation.Version
                writePrimitive jsonWriter "id" tag

            member __.EndWriteRoot () = jsonWriter.WriteEnd()

            member __.BeginWriteObject (_ : TypeInfo) (_ : PicklerInfo) (tag : string) (flags : ObjectFlags) =

                jsonWriter.WritePropertyName tag
                jsonWriter.WriteStartObject()

                if ObjectFlags.hasFlag flags ObjectFlags.IsNull then 
                    writePrimitive jsonWriter "null" true
                if ObjectFlags.hasFlag flags ObjectFlags.IsProperSubtype then 
                    writePrimitive jsonWriter "subtype" true
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


    type JsonPickleReader (stream : Stream, encoding : Encoding) =
        
        let sr = new StreamReader(stream, encoding)
        let jsonReader = new JsonTextReader(sr) :> JsonReader

        interface IPickleFormatReader with
            
            member __.BeginReadRoot (tag : string) =
                if jsonReader.Read() && jsonReader.TokenType = JsonToken.StartObject then
                    
                    do jsonReader.ReadProperty "FsPickler"
                    let version = jsonReader.ValueAs<string> ()
                    if version <> AssemblyVersionInformation.Version then
                        raise <| new InvalidDataException(sprintf "Invalid FsPickler version %s." version)

                    do jsonReader.ReadProperty "id"
                    let id = jsonReader.ValueAs<string> ()
                    if id <> tag then
                        let msg = sprintf "expected '%s' but was '%s'." tag id
                        raise <| new InvalidDataException()

                else raise <| new InvalidDataException("invalid json format.")

            member __.EndReadRoot () = jsonReader.Read () |> ignore

            member __.BeginReadObject (_ : TypeInfo) (_ : PicklerInfo) (tag : string) =
                do jsonReader.ReadProperty tag

                if jsonReader.TokenType = JsonToken.StartObject then
                    
                    let mutable objectFlags = ObjectFlags.None
                    // temp solution ; this is wrong
                    let mutable complete = false

                    while not complete && jsonReader.Read () && jsonReader.TokenType = JsonToken.PropertyName do
                        let value = jsonReader.ValueAs<string> ()
                        let _ = jsonReader.Read()
                        match value with
                        | "null" -> 
                            if jsonReader.ValueAs<bool> () then
                                objectFlags <- objectFlags ||| ObjectFlags.IsNull
                        | "subtype" ->
                            if jsonReader.ValueAs<bool> () then
                                objectFlags <- objectFlags ||| ObjectFlags.IsProperSubtype
                        | "cached" ->
                            if jsonReader.ValueAs<bool> () then
                                objectFlags <- objectFlags ||| ObjectFlags.IsCachedInstance
                        | "cyclic" ->
                            if jsonReader.ValueAs<bool> () then
                                objectFlags <- objectFlags ||| ObjectFlags.IsCyclicInstance
                        | "sequence" ->
                            if jsonReader.ValueAs<bool> () then
                                objectFlags <- objectFlags ||| ObjectFlags.IsSequenceHeader
                        | _ -> 
                            complete <- false
//                            let msg = sprintf "invalid property '%s'." value
//                            raise <| new InvalidDataException(msg)

                    objectFlags

                else
                    raise <| new InvalidDataException("invalid json format.")

            member __.EndReadObject () = jsonReader.Read () |> ignore

            member __.ReadBoolean tag = jsonReader.ReadProperty tag ; jsonReader.ValueAs<bool> ()

            member __.ReadByte tag = jsonReader.ReadProperty tag ; jsonReader.ValueAs<byte> ()
            member __.ReadSByte tag = jsonReader.ReadProperty tag ; jsonReader.ValueAs<sbyte> ()

            member __.ReadInt16 tag = jsonReader.ReadProperty tag ; jsonReader.ValueAs<int16> ()
            member __.ReadInt32 tag = jsonReader.ReadProperty tag ; jsonReader.ValueAs<int32> ()
            member __.ReadInt64 tag = jsonReader.ReadProperty tag ; jsonReader.ValueAs<int64> ()

            member __.ReadUInt16 tag = jsonReader.ReadProperty tag ; jsonReader.ValueAs<uint16> ()
            member __.ReadUInt32 tag = jsonReader.ReadProperty tag ; jsonReader.ValueAs<uint32> ()
            member __.ReadUInt64 tag = jsonReader.ReadProperty tag ; jsonReader.ValueAs<uint64> ()

            member __.ReadDecimal tag = jsonReader.ReadProperty tag ; jsonReader.ReadAsDecimal().Value
            member __.ReadSingle tag = jsonReader.ReadProperty tag ; jsonReader.ValueAs<single> ()
            member __.ReadDouble tag = jsonReader.ReadProperty tag ; jsonReader.ValueAs<double> ()

            member __.ReadChar tag = jsonReader.ReadProperty tag ; jsonReader.ValueAs<char> ()
            member __.ReadString tag = jsonReader.ReadProperty tag ; jsonReader.ReadAsString()

            member __.ReadBytes tag = jsonReader.ReadProperty tag ; jsonReader.ReadAsBytes()
            member __.ReadBytesFixed tag _ = jsonReader.ReadProperty tag ; jsonReader.ReadAsBytes()

            member __.IsPrimitiveArraySerializationSupported = false
            member __.ReadPrimitiveArray _ _ = raise <| new NotImplementedException()

            member __.Dispose () = (jsonReader :> IDisposable).Dispose() ; sr.Dispose()

    type JsonPickleFormatProvider (?encoding : Encoding, ?indented) =
        let encoding = defaultArg encoding Encoding.UTF8
        let indented = defaultArg indented false

        interface IPickleFormatProvider with
            member __.CreateWriter(stream) = new JsonPickleWriter(stream, encoding, indented) :> _
            member __.CreateReader(stream) = new JsonPickleReader(stream, encoding) :> _