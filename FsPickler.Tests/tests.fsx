#r "bin/Release/Newtonsoft.Json.dll"
#r "bin/Release/PerfUtil.dll"
#r "bin/Release/ProtoBuf-Net.dll"
#r "bin/Release/ServiceStack.Text.dll"
#r "bin/Release/nunit.framework.dll"
#r "bin/Release/FsPickler.dll"
#r "bin/Release/FsPickler.Tests.exe"

open System

open PerfUtil

open FsPickler
open FsPickler.Tests

open ProtoBuf


module PerfTests =

    [<ProtoContract(ImplicitFields = ImplicitFields.AllFields)>]
    type Entry(id : int, name : string, surname : string, age : int, data : byte [], date : DateTime) =

        new () = new Entry(0, "John", "Smith", 42, [| 1uy .. 100uy |], DateTime.Now)
        member __.Id = id
        member __.Name = name
        member __.Surname = surname
        member __.Age = age
        member __.Data = data
        member __.Date = date
        

    let mkEntry (id : int) = new Entry (id, "John", "Smith", 42, [| 1uy .. 100uy |], DateTime.Now)

    let entry = mkEntry 0

    let entries = dict [ for i in 1 .. 10000 -> i,  mkEntry i ]

    [<PerfTest>]
    let ``Roundtrip one table 100,000 times`` (s : ISerializer) =
        Serializer.roundtrips 100000 entry s

    [<PerfTest>]
    let ``Roundtrip dictionary with 10,000 entries 10 times`` (s : ISerializer) =
        Serializer.roundtrips 10 entries s


let tests = PerfTest<ISerializer>.OfModuleMarker<PerfTests.Entry> ()

let fsp = TestTypes.testSerializer :> ISerializer
let bfs = new BinaryFormatterSerializer() :> ISerializer
let ndc = new NetDataContractSerializer() :> ISerializer
let jdn = new JsonDotNetSerializer() :> ISerializer
let pbn = new ProtoBufSerializer() :> ISerializer
let ssj = new ServiceStackJsonSerializer() :> ISerializer
let sst = new ServiceStackTypeSerializer() :> ISerializer

let mkTester () = new ImplemantationComparer<ISerializer>(fsp, [bfs;ndc;jdn;pbn;ssj;sst]) :> PerformanceTester<ISerializer>

let results = PerfTest.run mkTester tests