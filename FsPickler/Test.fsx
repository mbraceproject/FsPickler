#r "bin/Debug/FsPickler.dll"

open Nessos.FsPickler
open Nessos.FsPickler.Combinators

let fsp = new FsPickler()
let fnv = Nessos.FsPickler.Hashing.FNV1aStreamFactory()

//let value = [1..100000] |> List.map (fun i -> Some (string i, Some i))

let loop(x : 'T) =
    let bytes = fsp.Pickle x
    fsp.UnPickle<'T> bytes

open System.IO

let file = "C:/Users/eirik/Desktop/"

let toFile name (value : 'T) =
    use fs = new FileStream(file + name + ".json", FileMode.Create, FileAccess.Write, FileShare.ReadWrite)
    fsp.Serialize(fs, value)

let ofFile<'T> name =
    use fs = File.OpenRead (file + name + ".json")
    fsp.Deserialize<'T>(fs)


toFile "test" 1

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

#r @"..\packages\Newtonsoft.Json.6.0.3\lib\net45\Newtonsoft.Json.dll"

open System.IO
open Newtonsoft.Json

let sw = new StringWriter()
let jsonW = new JsonTextWriter(sw) :> JsonWriter

jsonW.WriteStartObject()
jsonW.WritePropertyName("FsPickler")
jsonW.WriteValue("1.1")
jsonW.WriteStartObject()
jsonW.WritePropertyName("poutses")
jsonW.WriteValue(true)
jsonW.WriteEnd()
jsonW.WriteEnd()

jsonW.Flush()
let text = sw.ToString()

let sr = new StringReader(text)
let jsonR = new JsonTextReader(sr) :> JsonReader

jsonR.Read()
jsonR.TokenType
jsonR.Value

jsonR.Rea

jsonR.Read()
jsonR.TokenType
jsonR.ReadAsString()
jsonR.Value
jsonR.ReadAsString()