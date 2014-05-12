#r "bin/Debug/FsPickler.dll"

open Nessos.FsPickler
open Nessos.FsPickler.Combinators

let fsp = new FsPickler()
let fnv = Nessos.FsPickler.Hashing.FNV1aStreamFactory()

let value = [1..100000] |> List.map (fun i -> Some (string i, Some i))

let loop(x : 'T) =
    let bytes = fsp.Pickle x
    fsp.UnPickle<'T> bytes

let toFile (value : 'T) =
    use fs = System.IO.File.OpenWrite("/Users/eirik/Desktop/foo.xml")
    fsp.Serialize(fs, value)

let ofFile<'T> =
    use fs = System.IO.File.OpenRead("/Users/eirik/Desktop/foo.xml")
    fsp.Deserialize<'T>(fs)

toFile 1

ofFile<int>

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