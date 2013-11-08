#r "bin/Debug/FsPickler.dll"

open FsPickler
open FsPickler.Combinators

let fsp = new FsPickler()

let p = fsp.GeneratePickler<int * string []> ()

pickle p (42, [| "test" ; "" ; null |]) |> unpickle p


type Peano = Zero | Succ of Peano ref

let p = Pickler.auto<Peano>
let d = dict [(1,2)]

fsp.Pickle d