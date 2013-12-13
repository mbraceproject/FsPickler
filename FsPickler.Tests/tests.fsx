#r "bin/Release/Newtonsoft.Json.dll"
#r "bin/Release/PerfUtil.dll"
#r "bin/Release/ProtoBuf-Net.dll"
#r "bin/Release/ServiceStack.Text.dll"
#r "bin/Release/nunit.framework.dll"
#r "bin/Release/FsPickler.dll"
#r "bin/Release/FsPickler.Tests.exe"

#load "../packages/FSharp.Charting.0.90.5/FSharp.Charting.fsx"

open System
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

open PerfUtil

open FsPickler
open FsPickler.Tests

open FSharp.Charting

open ProtoBuf


[<AutoOpen>]
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

    [<Sealed>]
    [<ProtoContract(ImplicitFields = ImplicitFields.AllFields)>]
    type Tree<'T> (value : 'T, children : Tree<'T> []) =
        
        new () = Tree<'T>(Unchecked.defaultof<'T>, [||])
        
        member __.Value = value
        member __.Children = children

        member __.NodeCount =
            1 + (children |> Array.sumBy(fun c -> c.NodeCount))

    let mkEntry (id : int) = new Entry(id, "John", "Smith", 42, [| 1uy .. 100uy |], DateTime.Now)

    let entry = mkEntry 0

    let entries = new System.Collections.Generic.Dictionary<int, _> ()
    
    for i in 1 .. 1000 do
        entries.Add(i, mkEntry i)

    let rec mkTree (size : int) =
        if size = 0 then Tree(entry,[||])
        else
            let children = Array.init 3 (fun _ -> mkTree (size - 1))
            Tree(entry,children)

    let largeTree = mkTree 8

    [<PerfTest>]
    let ``Record Class`` (s : ISerializer) =
        Serializer.roundtrips 100000 entry s

    [<PerfTest>]
    let ``Dictionary of Record Classes`` (s : ISerializer) =
        Serializer.roundtrips 100 entries s

    [<PerfTest>]
    let ``Binary tree of depth 10`` (s : ISerializer) =
        Serializer.roundtrips 100 largeTree s

    let tuple = (1,"lorem ipsum",[|1..100|],4, (1,42), System.Guid.NewGuid())

    [<PerfTest>]
    let ``System.Tuple`` s = Serializer.roundtrips 1000 tuple s

    let list = [1..2000]

    [<PerfTest>]
    let ``F# List`` s = Serializer.roundtrips 1000 list s

    [<PerfTest>]
    let ``F# Quotation`` s = 
        let q' = Serializer.roundtrip PerformanceTests.quotationLarge s
        if PerformanceTests.quotationLarge.ToString() <> q'.ToString() then
            failwithf "'%s' produces invalid roundtrip" s.Name
        Serializer.roundtrips 100 PerformanceTests.quotationLarge s

    let iserializable = new TestTypes.SerializableClass<_>(42, "lorem ipsum", [|1..1000|])

    [<PerfTest>]
    let ``ISerializable Class`` s = Serializer.roundtrips 1000 iserializable s

    let fsharpBin = TestTypes.mkTree 10

    [<PerfTest>]
    let ``F# Binary Tree`` s = Serializer.roundtrips 100 fsharpBin s

    let forest = TestTypes.nForest 5 5 

    [<PerfTest>]
    let ``F# mutual recursive types`` s = Serializer.roundtrips 100 forest s

    let kvArr = [|1..10000|] |> Array.map (fun i -> (i,string i))

    [<PerfTest>]
    let ``Key-Value array`` s = Serializer.roundtrips 100 kvArr s
        


let tests = PerfTest<ISerializer>.OfModuleMarker<PerfTests.Entry> ()

let fsp = TestTypes.testSerializer :> ISerializer
let bfs = new BinaryFormatterSerializer() :> ISerializer
let ndc = new NetDataContractSerializer() :> ISerializer
let jdn = new JsonDotNetSerializer() :> ISerializer
let pbn = new ProtoBufSerializer() :> ISerializer
let ssj = new ServiceStackJsonSerializer() :> ISerializer
let sst = new ServiceStackTypeSerializer() :> ISerializer

let mkTester () = new ImplemantationComparer<ISerializer>(fsp, [bfs;ndc;jdn;pbn;ssj;sst]) :> PerformanceTester<ISerializer>


let dashGrid = ChartTypes.Grid(LineColor = Color.Gainsboro, LineDashStyle = ChartDashStyle.Dash)

let plot yaxis (metric : PerfResult -> float) (results : PerfResult list) =
    let values = results |> List.choose (fun r -> if r.HasFailed then None else Some (r.SessionId, metric r))
    let name = results |> List.tryPick (fun r -> let i = r.TestId.IndexOf('.') in Some <| r.TestId.[i+1..])
    Chart.Bar(values, ?Name = name, ?Title = name, YTitle = yaxis)
    |> Chart.WithYAxis(MajorGrid = dashGrid) 
    |> Chart.WithXAxis(MajorGrid = dashGrid) 
    |> fun ch -> ch.ShowChart()

let plotMS (results : TestSession list) = 
    results 
    |> TestSession.groupByTest
    |> Map.iter (fun _ rs -> plot "milliseconds" (fun r -> r.Elapsed.TotalMilliseconds) rs)


let results = PerfTest.run mkTester tests

TestSession.toFile "/mbrace/perftests.xml" results

plotMS results