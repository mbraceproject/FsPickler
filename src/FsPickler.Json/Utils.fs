namespace Nessos.FsPickler

    open System
    open System.IO

    open Newtonsoft.Json

    #nowarn "1204"

    module internal Utils =

        let inline fastUnbox<'T> (x : obj) = 
            Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.UnboxFast<'T> x

        let inline mkFlagCsv (flags : ObjectFlags) =
            let tokens = new ResizeArray<string>()
            if flags.HasFlag ObjectFlags.IsCachedInstance then
                tokens.Add "cached"

            if flags.HasFlag ObjectFlags.IsCyclicInstance then
                tokens.Add "cyclic"

            if flags.HasFlag ObjectFlags.IsProperSubtype then
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

        let inline invalidJsonFormat () = raise <| new InvalidDataException("invalid json format.")

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
                    invalidJsonFormat ()

            member inline jsonReader.ReadEndObject () =
                if jsonReader.Read() && jsonReader.TokenType = JsonToken.EndObject then ()
                else
                    invalidJsonFormat ()