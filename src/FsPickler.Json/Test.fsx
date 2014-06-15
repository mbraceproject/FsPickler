#I "../../bin/"
#r "FsPickler.dll"
#r "FsPickler.Json.dll"

open Nessos.FsPickler
open Nessos.FsPickler.Combinators

let jsp = FsPickler.CreateJson(indent = true, omitHeader = true)

type Record = { Name : string ; Age : int }

jsp.PickleToString { Name = "me" ; Age = 12 }

let pickle = jsp.PickleToString <@ 1 + 1 @>
jsp.UnPickleOfString<Quotations.Expr<int>> pickle


let list = Pickler.seq Pickler.int |> Pickler.wrap Seq.toList Seq.ofList

Json.pickle list [1..100] |> Json.unpickle list

Json.pickle Pickler.auto [box 42; box 12; box 42]