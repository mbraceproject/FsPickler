#I "../../bin/"
#r "FsPickler.dll"
#r "FsPickler.Json.dll"

fsi.PrintLength <- 1000
fsi.PrintWidth <- 1000

open System
open Nessos.FsPickler.Json

let json = FsPickler.CreateJsonSerializer(indent = true)

type GenericRecord<'T> = { GValue : 'T }
type GenericRecordAsString = GenericRecord<string>

let a0 = json.PickleToString<string>("a")
let a0' = json.UnPickleOfString<string>(a0)

let a1 = json.PickleToString<GenericRecordAsString>({ GValue = "a" })
let a1' = json.UnPickleOfString<GenericRecordAsString>(a1)

let b0 = json.PickleToString<string>("2013-10-30T16:00:00")
let b0' = json.UnPickleOfString<string>(b0)

let b1 = json.PickleToString<GenericRecordAsString>({ GValue = "2013-10-30T16:00:00" })
let b1' = json.UnPickleOfString<GenericRecordAsString>(b1)