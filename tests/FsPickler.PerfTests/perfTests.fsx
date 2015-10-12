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
    
    let entry = SimplePoco.Create()

    let entries = new System.Collections.Generic.Dictionary<int, _> ()
    for i in 1 .. 1000 do entries.Add(i, SimplePoco.Create())

    let largeTree = mkTree 8

    [<PerfTest(100000)>]
    let ``Simple POCO`` (s : Serializer) = Serializer.roundtrip entry s

    let dataContract = new DataContractClass("John", "Smith", age = 42)

    [<PerfTest(100000)>]
    let ``DataContract Object`` (s : Serializer) = Serializer.roundtrip dataContract s

    [<PerfTest(100)>]
    let ``BCL Dictionary containing POCOs (count = 1000)`` (s : Serializer) = Serializer.roundtrip entries s

    [<PerfTest(100)>]
    let ``Balanced Binary tree (depth = 10)`` (s : Serializer) = Serializer.roundtrip largeTree s

    let tuple = (1, "lorem ipsum", [|1..100|], (1,42), System.Guid.NewGuid())

    [<PerfTest(1000)>]
    let ``System.Tuple<int, string, byte[], int * int64, Guid>`` s = Serializer.roundtrip tuple s

    let list = [1..1000]

    [<PerfTest(1000)>]
    let ``F# List<int> (length = 1000)`` s = Serializer.roundtrip list s

    let tupleList = [1..1000] |> List.map (fun i -> string i,i)

    [<PerfTest(1000)>]
    let ``F# List<string * int> (length = 1000)`` s = Serializer.roundtrip tupleList s

    [<PerfTest(100)>]
    let ``Large F# Quotation`` s = Serializer.roundtrip PerformanceTests.quotationLarge s

    let iserializable = new TestTypes.SerializableClass<_>(42, "lorem ipsum", [|1..1000|])

    [<PerfTest(1000)>]
    let ``ISerializable Object`` s = Serializer.roundtrip iserializable s

    let exn = mkExceptionWithStackTrace()

    [<PerfTest(100)>]
    let ``Exception with stacktrace`` s = Serializer.roundtrip exn s

    let fsharpBin = TestTypes.mkTree 10

    [<PerfTest(100)>]
    let ``F# Binary Tree (depth = 10)`` s = Serializer.roundtrip fsharpBin s

    let forest = TestTypes.nForest 5 5 

    [<PerfTest(100)>]
    let ``F# mutual recursive type (depth = 5)`` s = Serializer.roundtrip forest s

    let kvArr = [|1..10000|] |> Array.map (fun i -> (i,string i))

    [<PerfTest(100)>]
    let ``Array of Tuple<int * string> (10000 elements)`` s = Serializer.roundtrip kvArr s

    let types = 
        [| 
            typeof<int> ; typeof<string> ; typeof<bool> ; typeof<byte> ; typeof<uint32> ; typeof<bigint> ;
            typeof<unit> ; typeof<obj> ; typeof<int list> ; typeof<byte []> ; typeof<System.DateTime> ; typeof<System.Guid>
            typeof<(string * int []) option> ; typeof<string list> ; typeof<Choice<int,int * System.Type,string>> ; 
            typeof<(int * int option) * string [] * string option> ; typeof<int ref>
            typedefof<Choice<_,_>> ; typedefof<_ * _ * _>
        |]

    let tyArray = Array.init 100 (fun i -> types.[i % types.Length])

    [<PerfTest(10000)>]
    let ``Array of System.Type (100 elements)`` s = Serializer.roundtrip tyArray s

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

let getChartName (results : PerfResult list) = results |> List.tryPick (fun r -> let i = r.TestId.IndexOf('.') in Some <| r.TestId.[i+1..])

let show (chart : ChartTypes.GenericChart) = chart.ShowChart() |> ignore

let plot name (metric : PerfResult -> float) (results : PerfResult list) =
    let values = 
        results 
        |> List.choose (fun r -> if r.HasFailed then None else Some (r.SessionId, metric r))
        |> List.sortBy fst
        |> List.rev

    Chart.Bar(values, ?Name = name)
    |> Chart.WithYAxis(MajorGrid = dashGrid) 
    |> Chart.WithXAxis(MajorGrid = dashGrid)

let plotTime (results : TestSession list) = 
    results 
    |> TestSession.groupByTest
    |> Map.iter (fun _ rs -> 
        plot None (fun r -> r.Elapsed.TotalMilliseconds / float r.Repeat) rs
        |> Chart.WithTitle(?Text = getChartName rs, InsideArea = false)
        |> Chart.WithYAxis(Enabled = true, Title = "milliseconds")
        |> show)

let plotGC (results : TestSession list) =
    results
    |> TestSession.groupByTest
    |> Map.iter (fun _ rs -> 
        let g0 = plot (Some "gen0") (fun r -> float r.GcDelta.[0]) rs
        let g1 = plot (Some "gen1") (fun r -> float r.GcDelta.[1]) rs
        let g2 = plot (Some "gen2") (fun r -> float r.GcDelta.[2]) rs
        Chart.Combine [g0;g1;g2]
        |> Chart.WithTitle (?Text = getChartName rs, InsideArea = false)
        |> Chart.WithLegend(Enabled = true)
        |> Chart.WithYAxis(Enabled = true, Title = "GC Collections")
        |> show)

//
//  Run the tests
//

let results = PerfTest.run mkTester tests
let cyclicResults = PerfTest.run mkCyclicGraphTester cyclic

//
//  Persist test results
//

let desktop = Environment.GetFolderPath(Environment.SpecialFolder.Desktop)
let perfFile = desktop + "/perftests.xml"
let cyclFile = desktop + "/perftests-cyclic.xml"

TestSession.toFile perfFile results
TestSession.toFile cyclFile cyclicResults

//let results = TestSession.ofFile perfFile
//let cyclicResults = TestSession.ofFile cyclFile

//
//  Plot test results
//

plotTime results
plotGC results
plotTime cyclicResults
plotGC cyclicResults