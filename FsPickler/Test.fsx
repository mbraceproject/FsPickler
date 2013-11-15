#r "bin/Release/FsPickler.dll"

open FsPickler
open FsPickler.Combinators

let fsp = new FsPickler()
let fnv = FsPickler.Hashing.FNV1aStreamFactory()

let value = [1..10000] |> List.map (fun i -> Some (string i, Some i))

#time

fsp.Pickle value
fsp.ComputeSize value
fsp.ComputeHash value
fsp.ComputeHash(value, fnv)
value.GetHashCode()

type Peano = Zero | Succ of Peano
Pickler.auto<Quotations.Expr>