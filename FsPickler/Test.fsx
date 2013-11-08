#r "bin/Release/FsPickler.dll"

open FsPickler
open FsPickler.Combinators

let fsp = new FsPickler()

open FsPickler.Hashing


let fnv = new FNV1aStreamFactory()

#time

let value = [1..10000] |> List.map (fun i -> string i, i)

for i = 0 to 100 do
    fsp.ComputeHash(value, fnv) |> ignore // ~ 129

for i = 0 to 100 do
    fsp.ComputeHash value |> ignore // ~433

for i = 0 to 100 do
    fsp.Pickle value |> ignore // ~ 450

for i = 0 to 100 do
    fsp.ComputeSize value |> ignore

for i = 0 to 100 do
    value.GetHashCode() |> ignore


let x = [| 1 .. 10000000 |]

let rand = new System.Random()

let d = new System.Collections.Generic.HashSet<byte []> ()

for k = 1 to 100000 do
    let i = rand.Next(0,10000000)
    let j = rand.Next()
    x.[i] <- j
    if d.Add((fsp.ComputeHash x).Hash) then ()
    else
        failwithf "colision at iteration %d" k

//d.Add 2
//
//fsp.ComputeHash x
//fsp.ComputeHash (x, fnv)

