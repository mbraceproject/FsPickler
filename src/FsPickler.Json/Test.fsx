#I "../../bin/"
#r "FsPickler.dll"
#r "FsPickler.Json.dll"

open Nessos.FsPickler
open Nessos.FsPickler.Json
open Nessos.FsPickler.Combinators

let jsp = FsPickler.CreateJsonSerializer(indent = true, omitHeader = true)
let bsp = FsPickler.CreateBsonSerializer()

type Record = { Name : string ; Age : int }

jsp.PickleToString { Name = "me" ; Age = 12 }

type Union = A of int | B of string * int | C

jsp.PickleToString [A 42 ; B("test", 0) ; C]

let pickle = jsp.PickleToString <@ 1 + 1 @>
jsp.UnPickleOfString<Quotations.Expr<int>> pickle