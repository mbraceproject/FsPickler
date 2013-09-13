namespace FsCoreSerializer.Tests

    module PerformanceTests =

        open System
        open System.IO
        open System.Diagnostics
        open System.Runtime.Serialization

        open NUnit.Framework
        open FsCoreSerializer

        open TestTypes

        let bfs = new TestBinaryFormatter () :> ISerializer
        let ndc = new TestNetDataContractSerializer () :> ISerializer

        let leastAcceptableImprovementFactor = 1.

        let testPerf iterations (input : 'T) =
            let runBenchmark (s : ISerializer) =
                try Choice1Of2 (benchmark (fun () -> Serializer.loop s iterations input))
                with e -> Choice2Of2 e

            // returns a floating point improvement factor
            let compareResults fscResult (other : ISerializer) =
                let getTimeMetric (r : BenchmarkResults<_>) = r.Elapsed.Ticks |> decimal

                let getSpaceMetric (r : BenchmarkResults<_>) =
                    r.GcDelta |> Seq.mapi (fun i g -> g * pown 10 i) |> Seq.sum |> float
                
                function
                | Choice1Of2 otherResult ->
                    let time = getTimeMetric otherResult / getTimeMetric fscResult |> float
                    let space = 
                        let otherM = getSpaceMetric otherResult 
                        let fscM = getSpaceMetric fscResult
                        // avoid NaN and Infinity results
                        if fscM = 0. then
                            if otherM = 0. then 1.
                            else otherM
                        else otherM / fscM

                    printfn "%s is %.2fx faster and %.2fx more memory efficient than %s." testSerializer.Name time space other.Name
                    // measure combined performance benefit with an 80% bias to time results
                    (4. * time + space) / 5.
                | Choice2Of2 e ->
                    printfn "%s failed during test." other.Name
                    Double.PositiveInfinity

            printfn "Running %d iterations on type %O:" iterations typeof<'T>

            let fscResults = runBenchmark TestTypes.testSerializer
            let ndcResults = runBenchmark ndc
            let bfsResults = runBenchmark bfs
                
            match fscResults with
            | Choice2Of2 e -> raise e
            | Choice1Of2 r ->
                let bfsMetric = compareResults r bfs bfsResults
                let ndcMetric = compareResults r ndc ndcResults

                let checkMetric (tested : ISerializer) (comparedTo : ISerializer) (metric : float) =
                    if metric < leastAcceptableImprovementFactor then
                        let msg = sprintf "%s scored a subpar %.1f%% improvement factor against %s." tested.Name (metric * 100.) comparedTo.Name
                        raise <| new AssertionException(msg)

                checkMetric testSerializer bfs bfsMetric
                checkMetric testSerializer ndc ndcMetric


        [<Test>]
        let ``Value: Int`` () = testPerf 10000 42

        [<Test>]
        let ``Value: Boolean`` () = testPerf 10000 true

        [<Test>]
        let ``Value: Float`` () = testPerf 10000 3.14

        [<Test>]
        let ``Value: Decimal`` () = testPerf 10000 1231231.121M

        [<Test>]
        let ``Value: Guid`` () = testPerf 1000 <| Guid.NewGuid()

        [<Test>]
        let ``Value: DateTime`` () = testPerf 1000 <| DateTime.Now

        [<Test>]
        let ``Value: String`` () = testPerf 10000 stringValue

        [<Test>]
        let ``Class: Simple F# Class`` () =
            let c = new Class(42, stringValue)
            testPerf 10000 c

        [<Test>]
        let ``Class: ISerializable`` () =
            let c = new SerializableClass<int list>(42, stringValue, [1..1000])
            testPerf 1000 c

        [<Test>]
        let ``Array: Float`` () =
            let bigFlt = Array.init 100000 (fun i -> float i)
            testPerf 10 bigFlt

        [<Test>]
        let ``Array: Int`` () =
            let bigFlt = Array.init 100000 id
            testPerf 10 bigFlt

        
        [<Test>]
        let ``Array: String`` () =
            let bigArr = Array.init 10000 (fun i -> stringValue + string i)
            testPerf 100 bigArr

        [<Test>]
        let ``Array: Key-Value Pairs`` () =
            let kvarr = [|1..10000|] |> Array.map (fun i -> i, string i)
            testPerf 100 kvarr

        [<Test>]
        let ``Array: Discriminated Unions`` () = 
            testPerf 100 [| for i in 1 .. 10000 -> (Something ("asdasdasdas", i)) |]

        [<Test>]
        let ``Array: Objects`` () =
            let array = 
                [| 
                    box 2; box 3; box "hello" ; box <| Some 3; box(2,3) ; 
                    box <| new Class(2, stringValue) ; box <| new SerializableClass<int option>(2, stringValue, Some 12); 
                    box stringValue 
                |]

            testPerf 1000 array

        [<Test>]
        let ``Array: Rank-3 Float`` () =
            let arr = Array3D.init 100 100 100 (fun i j k -> float (i * j + k))
            testPerf 10 arr

        [<Test>]
        let ``.NET Dictionary`` () =
            let dict = new System.Collections.Generic.Dictionary<string, int>()
            for i = 0 to 1000 do dict.Add(string i, i)
            testPerf 100 dict

        [<Test>]
        let ``.NET Stack`` () =
            let stack = new System.Collections.Generic.Stack<string> ()
            for i = 0 to 1000 do stack.Push <| string i
            testPerf 100 stack

        [<Test>]
        let ``.NET List`` () =
            let list = new System.Collections.Generic.List<string * int>()
            for i = 0 to 1000 do list.Add (string i, i)
            testPerf 100 list

        [<Test>]
        let ``.NET Set`` () =
            let set = new System.Collections.Generic.SortedSet<string> ()
            for i = 0 to 1000 do set.Add (string i) |> ignore
            testPerf 100 set

        [<Test>]
        let ``FSharp: Tuple Small`` () = testPerf 10000 (1, DateTime.Now,"hello")

        [<Test>]
        let ``FSharp: Tuple Large`` () =
            let tuple = (stringValue, 1, 2, 3, true, "", Some(3.14, [2]), 3, 2, 1, stringValue)
            testPerf 10000 tuple

        [<Test>]
        let ``FSharp: List Int`` () = testPerf 1000 [1..1000]

        [<Test>]
        let ``FSharp: List String`` () =
            let smallLst = [ for i in 1 .. 1000 -> stringValue + string i ]
            testPerf 1000 smallLst

        [<Test>]
        let ``FSharp: List Key-Value`` () =
            testPerf 1000 <| [ for i in 1 .. 1000 -> (string i, i) ]

        [<Test>]
        let ``FSharp: List Nested`` () =
            let nestedLst = let n = [1..1000] in [for _ in 1 .. 100 -> n]
            testPerf 1000 nestedLst

        [<Test>]
        let ``FSharp: Union`` () =
            let u = SomethingElse(stringValue, 42, box (Some 42))
            testPerf 10000 u

        [<Test>]
        let ``FSharp: Record`` () =
            let r = { Int = 42 ; String = stringValue ; Tuple = (13, "") }
            testPerf 10000 r

        [<Test>]
        let ``FSharp: Peano Rectype`` () =
            let int2Peano n =
                let rec aux pred = 
                    function
                    | 0 -> pred
                    | n -> aux (Succ pred) (n-1)

                aux Zero n

            testPerf 100 <| int2Peano 500

        [<Test>]
        let ``FSharp: Curried Function`` () =
            let clo = (@) [ Some([1..100], Set.ofList [1..100]) ]
            testPerf 1000 clo

        [<Test>]
        let ``FSharp: Binary Tree`` () =
            let rec mkTree = 
                function
                | 0 -> Leaf
                | n ->
                    Node(string n, mkTree (n-1), mkTree (n-1))

            testPerf 100 <| mkTree 10

        [<Test>]
        let ``FSharp: Set`` () = testPerf 1000 <| Set.ofList [1..1000]

        [<Test>]
        let ``FSharp: Map`` () = testPerf 1000 <| ([1..1000] |> Seq.map (fun i -> (string i,i)) |> Map.ofSeq)

        [<Test>]
        let ``FSharp: Quotation Small`` () = testPerf 10000 <@ fun x -> pown 2 x @>

        [<Test>]
        let ``FSharp: Quotation Large`` () =
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

            testPerf 1000 quotation