#r "bin/Release/FsPickler.dll"

open FsPickler
open FsPickler.Combinators

let fsp = new FsPickler()
let fnv = FsPickler.Hashing.FNV1aStreamFactory()

let value = [1..100000] |> List.map (fun i -> Some (string i, Some i))

#time

for i = 0 to 100 do
    fsp.Pickle value |> ignore
for i = 0 to 100 do
    fsp.ComputeSize value |> ignore
for i = 0 to 100 do
    fsp.ComputeHash value |> ignore
for i = 0 to 100 do
    fsp.ComputeHash(value, hashFactory = fnv) |> ignore
for i = 0 to 100 do
    value.GetHashCode() |> ignore


open System
open System.Reflection

let t = typeof<int * string option list []>

let tI = t.GetTypeInfo()

#time

for i = 0 to 1000000 do
    let aqn = t.AssemblyQualifiedName in ()

for i = 0 to 1000000 do
    let aqn = tI.AssemblyQualifiedName in ()
