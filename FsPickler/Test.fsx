#r "bin/Debug/FsPickler.dll"

open Nessos.FsPickler
open Nessos.FsPickler.Combinators

let fsp = new FsPickler()
let fnv = Nessos.FsPickler.Hashing.FNV1aStreamFactory()

let value = [1..100000] |> List.map (fun i -> Some (string i, Some i))

let loop(x : 'T) =
    let bytes = fsp.Pickle x
    fsp.UnPickle<'T> bytes

let t = System.Type.GetType "System.__Canon"
let i = System.Activator.CreateInstance t

loop i

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



open System
open System.Reflection
open System.Globalization

type TypeInfo =
    {
        Name : string
        AssemblyInfo : AssemblyInfo
    }

and AssemblyInfo =
    {
        Name : string
        Version : string
        Culture : string
        PublicKeyToken : byte []
    }
with
    static member Copy (aI : AssemblyInfo) = { aI with Name = if true then aI.Name else "" }
    static member OfAssemblyName(an : AssemblyName) =
        {
            Name = an.Name
            Version = an.Version.ToString()
            Culture = an.CultureInfo.Name
            PublicKeyToken = an.GetPublicKeyToken()
        }

    static member ToAssemblyName(aI : AssemblyInfo) =
        let an = new AssemblyName()

        an.Name <- aI.Name

        match aI.Version with
        | null | "" -> ()
        | version -> an.Version <- new Version(version)
                
        match aI.Culture with
        | null -> ()
        | culture -> an.CultureInfo <- new CultureInfo(culture)

        match aI.PublicKeyToken with
        | null -> ()
        | pkt -> an.SetPublicKeyToken(pkt)

        an

#time

let aI = AssemblyInfo.OfAssemblyName <| typeof<int>.Assembly.GetName()

for i = 1 to 10000 do
    aI |> AssemblyInfo.Copy |> AssemblyInfo.Copy |> ignore

for i = 1 to 10000 do
    aI |> AssemblyInfo.ToAssemblyName |> AssemblyInfo.OfAssemblyName |> ignore

open System

type Foo =
    static member Test<'T>() = ()
    static member Test<'T>(t : 'T option list) = t.ToString()

let ms =
    typeof<Foo>.GetMethods() 
    |> Array.filter (fun m -> m.Name = "Test" && m.IsGenericMethodDefinition && m.GetGenericArguments().[0].Name = "T")
    |> Array.sortBy (fun m -> m.ToString())

ms.[0].ToString()

type FooAttribute(t : Type) =
    inherit Attribute()

[<Foo(typeof<int>)>]
type Bar() = class end