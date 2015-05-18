namespace Nessos.FsPickler.Json

open System
open System.IO

open Newtonsoft.Json

open Nessos.FsPickler

#nowarn "1204"

[<AutoOpen>]
module internal Common =

    [<Literal>]
    let formatv0960 = "0.9.6"

    [<Literal>]
    let formatv1200 = "1.2.0"

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

        if flags.HasFlag ObjectFlags.IsSiftedValue then
            tokens.Add "hole"

        String.concat "," tokens

    let inline parseFlagCsv (csv : string) =
        let mutable flags = ObjectFlags.None
        let tokens = csv.Split(',')
        for t in tokens do
            match t with
            | "cached" -> flags <- flags ||| ObjectFlags.IsCachedInstance
            | "cyclic" -> flags <- flags ||| ObjectFlags.IsCyclicInstance
            | "subtype" -> flags <- flags ||| ObjectFlags.IsProperSubtype
            | "hole" -> flags <- flags ||| ObjectFlags.IsSiftedValue
            | _ -> raise <| new FormatException(sprintf "invalid pickle flag '%s'." t)

        flags

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
                    raise <| new FormatException(msg)
            else
                let msg = sprintf "expected token '%O' but was '%O'." JsonToken.PropertyName jsonReader.TokenType
                raise <| new FormatException(msg)

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
                raise <| new FormatException("Json document ended prematurely.")

        member inline jsonReader.ReadEndObject () =
            if jsonReader.Read() && jsonReader.TokenType = JsonToken.EndObject then ()
            else
                raise <| new FormatException(sprintf "Expected end of Json object but was '%O'." jsonReader.TokenType)