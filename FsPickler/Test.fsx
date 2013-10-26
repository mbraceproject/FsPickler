#r "bin/Release/FsPickler.dll"

open FsPickler
open FsPickler.Combinators

let fsp = new FsPickler()

let p = fsp.GeneratePickler<int * string []> ()

pickle p (42, [| "test" ; "" ; null |]) |> unpickle p

open FsPickler.Hashing


let fnv = new FNV1aStreamFactory()

#time

let value = [1..10000] |> List.map (fun i -> string i, i)

for i = 0 to 100 do
    fsp.ComputeHash(value, fnv) |> ignore // ~ 129

for i = 0 to 100 do
    fsp.ComputeHash value |> ignore // ~380

for i = 0 to 100 do
    fsp.Pickle value |> ignore // ~ 450

for i = 0 to 100 do
    fsp.ComputeSize value |> ignore

for i = 0 to 100 do
    value.GetHashCode() |> ignore


let x = [| 1 .. 10000000 |]

let rand = new System.Random()

let d = new System.Collections.Generic.HashSet<int> ()

d.Add 2

fsp.ComputeHash x
fsp.ComputeHash (x, fnv)