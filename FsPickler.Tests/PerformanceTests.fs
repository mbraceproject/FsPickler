namespace FsPickler.Tests

    open System

    open NUnit.Framework
    open PerfUtil

    open FsPickler
    open FsPickler.Tests.Serializer
    open FsPickler.Tests.TestTypes


    module PerformanceTests =

        type Marker = class end

        [<PerfTest>]
        let ``Value: Int`` s = roundtrips 10000 42 s

        [<PerfTest>]
        let ``Value: Boolean`` s = roundtrips 10000 true s

        [<PerfTest>]
        let ``Value: Float`` s = roundtrips 10000 3.14 s

        [<PerfTest>]
        let ``Value: Decimal`` s = roundtrips 10000 1231231.121M s

        [<PerfTest>]
        let ``Value: Guid`` s = roundtrips 1000 (Guid.NewGuid()) s

        [<PerfTest>]
        let ``Value: DateTime`` s = roundtrips 1000 DateTime.Now s

        [<PerfTest>]
        let ``Value: String`` s = roundtrips 10000 stringValue s

        [<PerfTest>]
        let ``Class: Simple F# Class`` s =
            let c = new Class(42, stringValue)
            roundtrips 10000 c s

        [<PerfTest>]
        let ``Class: ISerializable`` s =
            let c = new SerializableClass<int list>(42, stringValue, [1..1000])
            roundtrips 1000 c s

        [<PerfTest>]
        let ``Array: Float`` s =
            let bigFlt = Array.init 100000 (fun i -> float i)
            roundtrips 10 bigFlt s

        [<PerfTest>]
        let ``Array: Int`` s =
            let bigFlt = Array.init 100000 id
            roundtrips 10 bigFlt s

        
        [<PerfTest>]
        let ``Array: String`` s =
            let bigArr = Array.init 10000 (fun i -> stringValue + string i)
            roundtrips 100 bigArr s

        [<PerfTest>]
        let ``Array: Key-Value Pairs`` s =
            let kvarr = [|1..10000|] |> Array.map (fun i -> i, string i)
            roundtrips 100 kvarr s

        [<PerfTest>]
        let ``Array: Discriminated Unions`` s = 
            roundtrips 100 [| for i in 1 .. 10000 -> (Something ("asdasdasdas", i)) |] s

        [<PerfTest>]
        let ``Array: Objects`` s =
            let array = 
                [| 
                    box 2; box 3; box "hello" ; box <| Some 3; box(2,3) ; 
                    box <| new Class(2, stringValue) ; box <| new SerializableClass<int option>(2, stringValue, Some 12); 
                    box stringValue 
                |]

            roundtrips 1000 array s

        [<PerfTest>]
        let ``Array: Rank-3 Float`` s =
            let arr = Array3D.init 100 100 100 (fun i j k -> float (i * j + k))
            roundtrips 10 arr s

        [<PerfTest>]
        let ``.NET Dictionary`` s =
            let dict = new System.Collections.Generic.Dictionary<string, int>()
            for i = 0 to 1000 do dict.Add(string i, i)
            roundtrips 100 dict s

        [<PerfTest>]
        let ``.NET Stack`` s =
            let stack = new System.Collections.Generic.Stack<string> ()
            for i = 0 to 1000 do stack.Push <| string i
            roundtrips 100 stack s

        [<PerfTest>]
        let ``.NET List`` s =
            let list = new System.Collections.Generic.List<string * int>()
            for i = 0 to 1000 do list.Add (string i, i)
            roundtrips 100 list s

        [<PerfTest>]
        let ``.NET Set`` s =
            let set = new System.Collections.Generic.SortedSet<string> ()
            for i = 0 to 1000 do set.Add (string i) |> ignore
            roundtrips 100 set s

        [<PerfTest>]
        let ``FSharp: Tuple Small`` s = roundtrips 10000 (1, DateTime.Now,"hello") s

        [<PerfTest>]
        let ``FSharp: Tuple Large`` s =
            let tuple = (stringValue, 1, 2, 3, true, "", Some(3.14, [2]), 3, 2, 1, stringValue)
            roundtrips 10000 tuple s

        [<PerfTest>]
        let ``FSharp: List Int`` s = roundtrips 1000 [1..1000] s

        [<PerfTest>]
        let ``FSharp: List String`` s =
            let smallLst = [ for i in 1 .. 1000 -> stringValue + string i ]
            roundtrips 1000 smallLst s

        [<PerfTest>]
        let ``FSharp: List Key-Value`` s =
            roundtrips 1000 [ for i in 1 .. 1000 -> (string i, i) ] s

        [<PerfTest>]
        let ``FSharp: List Nested`` s =
            let nestedLst = let n = [1..1000] in [for _ in 1 .. 100 -> n]
            roundtrips 1000 nestedLst s

        [<PerfTest>]
        let ``FSharp: Union`` s =
            let u = SomethingElse(stringValue, 42, box (Some 42))
            roundtrips 10000 u s

        [<PerfTest>]
        let ``FSharp: Record`` s =
            let r = { Int = 42 ; String = stringValue ; Tuple = (13, "") }
            roundtrips 10000 r s

        [<PerfTest>]
        let ``FSharp: Peano Rectype`` s =
            let int2Peano n =
                let rec aux pred = 
                    function
                    | 0 -> pred
                    | n -> aux (Succ pred) (n-1)

                aux Zero n

            roundtrips 100 (int2Peano 100) s

        [<PerfTest>]
        let ``FSharp: Curried Function`` s =
            let clo = (@) [ Some([1..100], Set.ofList [1..100]) ]
            roundtrips 1000 clo s

        [<PerfTest>]
        let ``FSharp: Binary Tree`` s =
            let rec mkTree = 
                function
                | 0 -> Leaf
                | n ->
                    Node(string n, mkTree (n-1), mkTree (n-1))

            roundtrips 100 (mkTree 10) s

        [<PerfTest>]
        let ``FSharp: Set`` s = roundtrips 1000 (Set.ofList [1..1000]) s

        [<PerfTest>]
        let ``FSharp: Map`` s = 
            let map = [1..1000] |> Seq.map (fun i -> (string i,i)) |> Map.ofSeq
            roundtrips 1000 map s

        [<PerfTest>]
        let ``FSharp: Quotation Small`` s = roundtrips 10000 <@ fun x -> pown 2 x @> s

        [<PerfTest>]
        let ``FSharp: Quotation Large`` s =
            let quotation =
                <@
                    async {
                        let rec fibAsync n =
                            async {
                                match n with
                                | _ when n < 0 -> return invalidArg "negative" "n"
                                | _ when n < 2 -> return n
                                | n ->
                                    let! fn = fibAsync (n-1)
                                    let! fnn = fibAsync (n-2)
                                    return fn + fnn
                            }

                        let! values = [1..100] |> Seq.map fibAsync |> Async.Parallel
                        return Seq.sum values
                    }
                @>

            roundtrips 1000 quotation s


    type ``Serializer Comparison`` () =
        inherit NUnitPerf<ISerializer>()

        let fsp = testSerializer :> ISerializer
        let bfs = new BinaryFormatterSerializer() :> ISerializer
        let ndc = new NetDataContractSerializer() :> ISerializer
        let jdn = new JsonDotNetSerializer() :> ISerializer
        let ssj = new ServiceStackJsonSerializer() :> ISerializer
        let sst = new ServiceStackTypeSerializer() :> ISerializer

        let comparer = new MeanComparer(spaceFactor = 0.2, leastAcceptableImprovementFactor = 1.)

        let tester = new ImplemantationComparer<_>(fsp, [bfs;ndc;jdn;ssj;sst], throwOnError = true, comparer = comparer)
        let tests = PerfTest.OfModuleMarker<PerformanceTests.Marker> ()

        override __.PerfTester = tester :> _
        override __.PerfTests = tests


    type ``Past FsPickler Versions Comparison`` () =
        inherit NUnitPerf<ISerializer> ()

        let persistResults = true
        let persistenceFile = "fspPerf.xml"

        let fsp = testSerializer :> ISerializer
        let version = typeof<FsPickler>.Assembly.GetName().Version
        let comparer = new MeanComparer(spaceFactor = 0.2, leastAcceptableImprovementFactor = 0.8)
        let tests = PerfTest.OfModuleMarker<PerformanceTests.Marker> ()
        let tester = 
            new PastImplementationComparer<ISerializer>(
                fsp, version, historyFile = persistenceFile, throwOnError = true, comparer = comparer)

        override __.PerfTester = tester :> _
        override __.PerfTests = tests

        [<TestFixtureTearDown>]
        member __.Persist() =
            if persistResults then tester.PersistCurrentResults ()