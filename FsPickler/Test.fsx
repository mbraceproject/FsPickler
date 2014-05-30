#r "bin/Debug/FsPickler.dll"

open Nessos.FsPickler
open Nessos.FsPickler.Combinators

let fsp = new FsPickler()
let fnv = Nessos.FsPickler.Hashing.FNV1aStreamFactory()

let value = [1..100000] |> List.map (fun i -> Some (string i, Some i))

let loop(x : 'T) =
    let bytes = fsp.Pickle x
    fsp.UnPickle<'T> bytes

open System
open System.IO

let file = "C:/Users/eirik/Desktop/"

let toFile<'T> name (value : 'T) =
    use fs = new FileStream(file + name + ".json", FileMode.Create, FileAccess.Write, FileShare.ReadWrite)
    fsp.Serialize(fs, value)

let ofFile<'T> name (value : 'T) =
    use fs = File.OpenRead (file + name + ".json")
    fsp.Deserialize<'T>(fs)

let c = Activator.CreateInstance<System.Globalization.JapaneseLunisolarCalendar>()

toFile "test" c
ofFile "test" c

toFile "a" <@ 1 + 1 @>
ofFile "a" <@ 1 + 1 @>

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