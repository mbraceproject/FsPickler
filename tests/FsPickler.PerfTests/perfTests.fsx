#I "../../bin"
//#I "../../bin/NoEmit"

#r "Newtonsoft.Json.dll"
#r "PerfUtil.dll"
#r "ProtoBuf-Net.dll"
#r "Wire.dll"
#r "nunit.framework.dll"
#r "FsPickler.dll"
#r "FsPickler.PerfTests.dll"

#load "../../packages/FSharp.Charting/FSharp.Charting.fsx"

open System
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

open PerfUtil

open Nessos.FsPickler
open Nessos.FsPickler.Tests

open FSharp.Charting


[<AutoOpen>]
module PerfTests =

    type Marker = class end
    
    let entry = Entry()

    let entries = new System.Collections.Generic.Dictionary<int, _> ()
    
    for i in 1 .. 1000 do
        entries.Add(i, new Entry())

    let largeTree = mkTree 8

    [<PerfTest(100000)>]
    let ``Generic Class`` (s : Serializer) = Serializer.roundtrip entry s

    [<PerfTest(100)>]
    let ``Dictionary of Generic Classes`` (s : Serializer) = Serializer.roundtrip entries s

    [<PerfTest(100)>]
    let ``Balanced Binary tree of depth 10`` (s : Serializer) = Serializer.roundtrip largeTree s

    let tuple = (1,"lorem ipsum",[|1..100|],4, (1,42), System.Guid.NewGuid())

    [<PerfTest(1000)>]
    let ``System.Tuple`` s = Serializer.roundtrip tuple s

    let list = [1..1000]

    [<PerfTest(1000)>]
    let ``F# List`` s = Serializer.roundtrip list s

    let tupleList = [1..1000] |> List.map (fun i -> string i,i)

    [<PerfTest(1000)>]
    let ``F# List of Pairs`` s = Serializer.roundtrip tupleList s

    [<PerfTest(100)>]
    let ``F# Quotation`` s = Serializer.roundtrip PerformanceTests.quotationLarge s

    let iserializable = new TestTypes.SerializableClass<_>(42, "lorem ipsum", [|1..1000|])

    [<PerfTest(1000)>]
    let ``ISerializable Class`` s = Serializer.roundtrip iserializable s

    let fsharpBin = TestTypes.mkTree 10

    [<PerfTest(100)>]
    let ``F# Binary Tree`` s = Serializer.roundtrip fsharpBin s

    let forest = TestTypes.nForest 5 5 

    [<PerfTest(100)>]
    let ``F# mutual recursive types`` s = Serializer.roundtrip forest s

    let kvArr = [|1..10000|] |> Array.map (fun i -> (i,string i))

    [<PerfTest(100)>]
    let ``Array of tuples`` s = Serializer.roundtrip kvArr s

    let tyArray = Array.init 10 (fun i -> if i % 2 = 0 then typeof<int> else typeof<int * string option []>)

    [<PerfTest(10000)>]
    let ``System.Type`` s = Serializer.roundtrip tyArray s

    let array = Array3D.init 200 200 200 (fun i j k -> float <| i + 1000 * j + 1000000 * k)

    [<PerfTest>]
    let ``200 x 200 x 200 double array`` s = Serializer.roundtrip array s

module RandomGraph =
    type Marker = class end

    let graph = Nessos.FsPickler.Tests.TestTypes.createRandomGraph 0.2 500

    [<PerfTest>]
    let ``Random object graph (n=500,P=20%)`` s = Serializer.roundtrip graph s



let tests = PerfTest<Serializer>.OfModuleMarker<PerfTests.Marker> ()
let cyclic = PerfTest<Serializer>.OfModuleMarker<RandomGraph.Marker> ()

let fspBinary = FsPickler.initBinary()
let fspXml = FsPickler.initXml()
let fspJson = FsPickler.initJson()
let bfs = new BinaryFormatterSerializer() :> Serializer
let ndc = new NetDataContractSerializer() :> Serializer
let jdn = new JsonDotNetSerializer() :> Serializer
let pbn = new ProtoBufSerializer() :> Serializer
let wire = new WireSerializer() :> Serializer

let allSerializers = [fspXml;fspJson;bfs;ndc;jdn;pbn;wire]
let cyclicOnly = [fspXml;fspJson;bfs]

let mkTester () = new ImplementationComparer<Serializer>(fspBinary, allSerializers, warmup = true) :> PerformanceTester<Serializer>
let mkCyclicGraphTester () = new ImplementationComparer<Serializer>(fspBinary, cyclicOnly, warmup = true) :> PerformanceTester<Serializer>


let dashGrid = ChartTypes.Grid(LineColor = Color.Gainsboro, LineDashStyle = ChartDashStyle.Dash)

let plot yaxis (metric : PerfResult -> float) (results : PerfResult list) =
    let values = results |> List.choose (fun r -> if r.HasFailed then None else Some (r.SessionId, metric r))
    let name = results |> List.tryPick (fun r -> let i = r.TestId.IndexOf('.') in Some <| r.TestId.[i+1..])
    Chart.Bar(values, ?Name = name, ?Title = None, YTitle = yaxis)
    |> Chart.WithYAxis(MajorGrid = dashGrid) 
    |> Chart.WithXAxis(MajorGrid = dashGrid)
    |> fun ch -> ch.ShowChart()
    |> ignore

let plotTime (results : TestSession list) = 
    results 
    |> TestSession.groupByTest
    |> Map.iter (fun _ rs -> plot "milliseconds" (fun r -> r.Elapsed.TotalMilliseconds) rs)

let plotGC (results : TestSession list) =
    results
    |> TestSession.groupByTest
    |> Map.iter (fun _ rs -> plot "GC Collections (gen0)" (fun r -> float r.GcDelta.[0]) rs)

//
//  Run the tests
//

let results = PerfTest.run mkTester tests
let cyclicResults = PerfTest.run mkCyclicGraphTester cyclic

let desktop = Environment.GetFolderPath(Environment.SpecialFolder.Desktop)
TestSession.toFile (desktop + "/perftests.xml") results
TestSession.toFile (desktop + "/perftests-cyclic.xml") cyclicResults

plotTime results
plotGC results
plotTime cyclicResults
plotGC cyclicResults