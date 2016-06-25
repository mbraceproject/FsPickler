#I "../../bin/"
#r "FsPickler.dll"
#r "FsPickler.Json.dll"

open Nessos.FsPickler
open Nessos.FsPickler.Json
open Nessos.FsPickler.Combinators

let jsp = FsPickler.CreateJsonSerializer(indent = true, omitHeader = true)
let bsp = FsPickler.CreateBsonSerializer(omitHeader = true)


type Record = { Name : string ; Age : int }

jsp.PickleToString { Name = "me" ; Age = 12 }

type Union = A of int | B of string * int | C

jsp.PickleToString [A 42 ; B("test", 0) ; C]

let pickle = jsp.PickleToString <@ 1 + 1 @>
jsp.UnPickleOfString<Quotations.Expr<int>> pickle
let buffer = bsp.Pickle { Name = "me" ; Age = 12 }
buffer |> Array.map (fun b -> char b)

let len1 = System.BitConverter.ToInt32(buffer, 0)
buffer |> Array.length

System.Text.Encoding.UTF8.GetBytes "me"