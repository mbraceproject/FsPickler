#r "bin/Debug/FsPickler.dll"

open Nessos.FsPickler
open Nessos.FsPickler.Combinators

let fsp = new FsPickler()
let fnv = Nessos.FsPickler.Hashing.FNV1aStreamFactory()

let value = [1..100000] |> List.map (fun i -> Some (string i, Some i))

let loop(x : 'T) =
    let bytes = fsp.Pickle x
    fsp.UnPickle<'T> bytes

open System.IO

let file = "/Users/eirik/Desktop/foo.xml"

let toFile (value : 'T) =
    use fs = new FileStream(file, FileMode.Create, FileAccess.Write, FileShare.ReadWrite)
    fsp.Serialize(fs, value)

let ofFile<'T> =
    use fs = File.OpenRead file
    fsp.Deserialize<'T>(fs)

let x = obj()

toFile <| ([Some (42,"12") ; None])

ofFile<(string * int option) option list>


loop 

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