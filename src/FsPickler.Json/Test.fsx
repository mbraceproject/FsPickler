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