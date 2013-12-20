#r "bin/Release/FsPickler.dll"

open FsPickler
open FsPickler.Combinators

let fsp = new FsPickler()
let fnv = FsPickler.Hashing.FNV1aStreamFactory()

let value = [1..100000] |> List.map (fun i -> Some (string i, Some i))


let xs = fsp.Pickle([1..10000])

System.IO.File.WriteAllBytes("/mbrace/foo.txt", xs)
let bytes = System.IO.File.ReadAllBytes("/mbrace/foo.txt")

let xs' = fsp.UnPickle<int list> bytes

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