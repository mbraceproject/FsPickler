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

toFile <| value

ofFile<(string * int option) option list>


loop ([Some (42,"12") ; None])

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


#r "System.Xml"

open System.IO
open System.Xml

let fs = File.OpenRead "C:/Users/eirik/Desktop/foo.xml"
fs.Position <- 0L
let reader = XmlReader.Create(fs)

/// ROOT ELEMENT
reader.Read()
reader.NodeType // XmlDeclaration
reader.Read()

reader.NodeType // Element
reader.Name // FsPickler
reader.GetAttribute("version") // version
reader.Read()

// tuple "root"
// BeginReadObject
reader.NodeType // Element
reader.GetAttribute(0) // object flags
reader.Name // root
reader.Value
reader.Read()

reader.ReadContentAsBase64([|1uy..10uy|], 0, 10)
reader.ReadElementContentAsBase64([|1uy..10uy|], 0, 10)
reader.NodeType
reader.Read()

reader

// read item1
reader.NodeType // Element
reader.Name // item1
reader.ReadElementContentAsInt() // 1
//reader.Read()
//reader.NodeType // Text
//reader.ReadContentAsInt() // 1
//reader.ReadEndElement()

reader.Read()
reader.ReadEndElement()
reader

reader.NodeType // Element
reader.Name // item2
reader.ReadElementContentAsInt() // 1

reader.NodeType // EndElement
reader.Name // root
reader.ReadEndElement()

reader.NodeType // EndElement
reader.Name // FsPickler
reader.ReadEndElement()