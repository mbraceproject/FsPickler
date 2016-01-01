#I "../../bin/"
#r "FsPickler.dll"
#r "FsPickler.Json.dll"
#r "Newtonsoft.Json.dll"

fsi.PrintLength <- 1000
fsi.PrintWidth <- 1000

open System
open System.IO
open Nessos.FsPickler.Json
open Newtonsoft.Json

let o = """{
"s": "2013-10-30T16:00:00"
}"""

let inline fastUnbox<'T> (x : obj) = 
    Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.UnboxFast<'T> x

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

    member inline jsonReader.ReadPrimitiveAsString ignoreName (name : string) =
        if not ignoreName then
            jsonReader.ReadProperty name
            jsonReader.ReadAsString() |> ignore
            
        let v = jsonReader.ValueAs<string> ()
        jsonReader.Read() |> ignore
        v

    member inline jsonReader.ReadPrimitiveAs<'T> ignoreName (name : string) =
        if not ignoreName then
            jsonReader.ReadProperty name
            jsonReader.Read() |> ignore
            
        let v = jsonReader.ValueAs<'T> ()
        jsonReader.Read() |> ignore
        v

let reader = new JsonTextReader(new StringReader(o))
reader.Read()
reader.Value
reader.Read()
reader.Value
reader.Read()
reader.Value
//val it : obj = 30/10/2013 4:00:00 PM

let reader' = new JsonTextReader(new StringReader(o))
reader'.Read()
reader'.Value
reader'.Read()
reader'.Value
reader'.ReadAsString()
reader'.Value
//val it : obj = "2013-10-30T16:00:00"

let reader'' = new JsonTextReader(new StringReader(o))
reader''.Read()
reader''.Value
reader''.Read()
reader''.Value
// The following line fails.
//reader''.ReadPrimitiveAs<string> false "s"
//System.InvalidCastException: Unable to cast object of type 'System.DateTime' to type 'System.String'.

let reader''' = new JsonTextReader(new StringReader(o))
reader'''.Read()
reader'''.Value
reader'''.Read()
reader'''.Value
reader'''.ReadPrimitiveAsString false "s"
//val it : string = "2013-10-30T16:00:00"

let printJsonAsRead s =
    let reader = new JsonTextReader(new StringReader(s))

    while (reader.Read())
        do
            printfn "value: %A, type: %A" reader.Value reader.ValueType

let json = FsPickler.CreateJsonSerializer(indent = true)

type GenericRecord<'T> = { GValue : 'T }
type GenericRecordAsString = GenericRecord<string>

let a0 = json.PickleToString<string>("a")
let a0' = json.UnPickleOfString<string>(a0)

let a1 = json.PickleToString<GenericRecordAsString>({ GValue = "a" })
let a1' = json.UnPickleOfString<GenericRecordAsString>(a1)

let b0 = json.PickleToString<string>("2013-10-30T16:00:00")
let b1 = json.PickleToString<GenericRecordAsString>({ GValue = "2013-10-30T16:00:00" })

printJsonAsRead b0
printJsonAsRead b1

// The following two lines fail.
//let b0' = json.UnPickleOfString<string>(b0)
//let b1' = json.UnPickleOfString<GenericRecordAsString>(b1)