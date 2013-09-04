#r "bin/Debug/FsCoreSerializer.dll"

open FsCoreSerializer

let bfs = new BinaryFormatterSerializer() :> ISerializer
let ndc = new NDCSerializer() :> ISerializer
let fsc = new FsCoreSerializer() :> ISerializer

#time
let test n (s : ISerializer) (inp : 'T) =
    for _ in 1 .. n do
        s.Serialize inp |> s.Deserialize :?> 'T |> ignore

let quot =
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

test 1000 fsc quot
test 1000 ndc quot
test 1000 bfs quot


let simple = (("asdasdasdas", 12), Some(1.1132, [2;3]))

test 10000 fsc simple
test 10000 ndc simple
test 10000 bfs simple

type Test = Nothing | Something of string * int

let list = [ for i in 1 .. 100 -> (Something ("asdasdasdas", i)) ]

test 1000 fsc list
test 1000 ndc list
test 1000 bfs list


let bigObj = let seed = Seq.map (fun i -> i, string i) [1..100000] in (List.ofSeq seed, Some (Map.ofSeq seed))

test 10 fsc bigObj
test 10 ndc bigObj // stack overflow
test 10 bfs bigObj

let longstr = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."

let smallArr = Array.init 500 (fun i -> longstr + string i)

test 1000 fsc smallArr
test 1000 ndc smallArr
test 1000 bfs smallArr


let bigArr = Array.init 1000000 (fun i -> longstr + string i)

test 10 fsc bigArr
test 10 ndc bigArr
test 10 bfs bigArr

let bigFlt = Array.init 10000000 (fun i -> float i)

test 10 fsc bigFlt
test 10 ndc bigFlt
test 10 bfs bigFlt

let smallLst = [ for i in 1 .. 1000 -> longstr + string i ]

test 1000 fsc smallLst
test 1000 ndc smallLst
test 1000 bfs smallLst

let bigLst = [for i in 1 .. 1000000 -> longstr + string i ]

test 10 fsc bigLst
test 10 ndc bigLst // stack overflow
test 10 bfs bigLst

let nestedLst = let n = [1..1000000] in [ for _ in 1 .. 5 -> n]

test 10 fsc nestedLst
test 10 ndc nestedLst // stack overflow
test 10 bfs nestedLst

// rectypes

type Peano = Zero | Succ of Peano

let int2Peano n =
    let rec helper pred = 
        function
        | 0 -> pred
        | n -> helper (Succ pred) (n-1)

    helper Zero n

let p = int2Peano 500

test 100 fsc p
test 100 ndc p
test 100 bfs p

let pHuge = int2Peano 10000

test 1 fsc pHuge // exception
test 1 ndc pHuge // stack overflow
test 1 bfs pHuge // stack overflow

// binary trees carrying untyped data

type BinTree =
    | Leaf
    | Node of obj * BinTree * BinTree

let rec mkTree = 
    function
    | 0 -> Leaf
    | n ->
        Node(string n, mkTree (n-1), mkTree (n-1))

let tr = mkTree 20

test 1 fsc tr
test 1 ndc tr
test 1 bfs tr


// multi rank arrays

open System

let arr = Array3D.init 100 100 100 (fun i j k -> float (i * j + k))

test 10 fsc arr
test 10 ndc arr // fail
test 10 bfs arr

// lambdas

let add x y z = x + y + z

test 10000 fsc (add 2 3)
test 10000 ndc (add 2 3)
test 10000 bfs (add 2 3)

type NestedLambda = Func of (int -> int)

test 1000 fsc (Func (add 2 3))
test 1000 ndc (Func (add 2 3))
test 1000 bfs (Func (add 2 3))


// key value arrays

let kvarr = [|1..10000000|] |> Array.map (fun i -> i, string i)

test 1 fsc kvarr
test 1 ndc kvarr
test 1 bfs kvarr