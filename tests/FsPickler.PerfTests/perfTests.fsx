#I "../../bin"

#r "Newtonsoft.Json.dll"
#r "PerfUtil.dll"
#r "ProtoBuf-Net.dll"
#r "ServiceStack.Text.dll"
#r "nunit.framework.dll"
#r "FsPickler.dll"
#r "FsPickler.PerfTests.dll"

#load "../packages/FSharp.Charting.0.90.6/FSharp.Charting.fsx"

open System
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

open PerfUtil

open Nessos.FsPickler
open Nessos.FsPickler.Tests

open FSharp.Charting

open ProtoBuf


[<AutoOpen>]
module PerfTests =

    type Marker = class end

    let entries = new System.Collections.Generic.Dictionary<int, _> ()
    
    for i in 1 .. 1000 do
        entries.Add(i, new Entry())

    let largeTree = mkTree 8

    [<PerfTest>]
    let ``Generic Class`` (s : ISerializer) =
        Serializer.roundtrips 100000 (Entry()) s

    [<PerfTest>]
    let ``Dictionary of Generic Classes`` (s : ISerializer) =
        Serializer.roundtrips 100 entries s

    [<PerfTest>]
    let ``Balanced Binary tree of depth 10`` (s : ISerializer) =
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
    let ``Array of tuples`` s = Serializer.roundtrips 100 kvArr s

    let tyArray = Array.init 10 (fun i -> if i % 2 = 0 then typeof<int> else typeof<int * string option []>)

    [<PerfTest>]
    let ``System.Type`` s = Serializer.roundtrips 10000 tyArray s

    let array = Array3D.init 200 200 200 (fun i j k -> float <| i + 1000 * j + 1000000 * k)

    [<PerfTest>]
    let ``200 x 200 x 200 double array`` s = Serializer.roundtrips 1 array s

module RandomGraph =
    type Marker = class end

    let graph = Nessos.FsPickler.Tests.TestTypes.createRandomGraph 0.2 500

    [<PerfTest>]
    let ``Random object graph (n=500,P=20%)`` s = Serializer.roundtrips 1 graph s



let tests = PerfTest<ISerializer>.OfModuleMarker<PerfTests.Marker> ()
let cyclic = PerfTest<ISerializer>.OfModuleMarker<RandomGraph.Marker> ()

let fspBinary = FsPickler.initBinary()
let fspXml = FsPickler.initXml()
let fspJson = FsPickler.initJson()
let bfs = new BinaryFormatterSerializer() :> ISerializer
let ndc = new NetDataContractSerializer() :> ISerializer
let jdn = new JsonDotNetSerializer() :> ISerializer
let pbn = new ProtoBufSerializer() :> ISerializer
let ssj = new ServiceStackJsonSerializer() :> ISerializer
let sst = new ServiceStackTypeSerializer() :> ISerializer

let allSerializers = [fspXml;fspJson;bfs;ndc;jdn;pbn;ssj;sst]
let cyclicOnly = [fspXml;fspJson;bfs]

let mkTester () = new ImplementationComparer<ISerializer>(fspBinary, allSerializers) :> PerformanceTester<ISerializer>
let mkCyclicGraphTester () = new ImplementationComparer<ISerializer>(fspBinary, cyclicOnly) :> PerformanceTester<ISerializer>


let dashGrid = ChartTypes.Grid(LineColor = Color.Gainsboro, LineDashStyle = ChartDashStyle.Dash)

let plot yaxis (metric : PerfResult -> float) (results : PerfResult list) =
    let values = results |> List.choose (fun r -> if r.HasFailed then None else Some (r.SessionId, metric r))
    let name = results |> List.tryPick (fun r -> let i = r.TestId.IndexOf('.') in Some <| r.TestId.[i+1..])
    Chart.Bar(values, ?Name = name, ?Title = None, YTitle = yaxis)
    |> Chart.WithYAxis(MajorGrid = dashGrid) 
    |> Chart.WithXAxis(MajorGrid = dashGrid)
    |> fun ch -> ch.ShowChart()

let plotTime (results : TestSession list) = 
    results 
    |> TestSession.groupByTest
    |> Map.iter (fun _ rs -> plot "milliseconds" (fun r -> r.Elapsed.TotalMilliseconds) rs)

let plotGC (results : TestSession list) =
    results
    |> TestSession.groupByTest
    |> Map.iter (fun _ rs -> plot "GC Collections (gen0)" (fun r -> float r.GcDelta.[0]) rs)


let results = PerfTest.run mkTester tests
let cyclicResults = PerfTest.run mkCyclicGraphTester cyclic

TestSession.toFile "/mbrace/perftests.xml" results
TestSession.toFile "/mbrace/perftests-cyclic.xml" cyclicResults

plotTime results
plotGC results
plotTime cyclicResults
plotGC cyclicResults