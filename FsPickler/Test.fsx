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

let file = "C:/Users/eirik/Desktop/"

let toFile<'T> name (value : 'T) =
    use fs = new FileStream(file + name + ".json", FileMode.Create, FileAccess.Write, FileShare.ReadWrite)
    fsp.Serialize(fs, value)

let ofFile<'T> name =
    use fs = File.OpenRead (file + name + ".json")
    fsp.Deserialize<'T>(fs)

toFile<int list> "test" [1..100]
toFile "a" <@ 1 + 1 @>

ofFile<int> "test"

ofFile<Quotations.Expr<int>> "b"

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