namespace MBrace.FsPickler.Benchmarks

open System
open FsCheck

module Poco =
    [<CLIMutable>]
    type T =
        { A : int ; B : string ; C : bool ; D : byte[] ; 
          F : DateTimeOffset ; G : TimeSpan ; H : Guid }

    let value = { A = 42 ; B = "lorem ipsum" ; C = true ; D = [|1uy .. 20uy|]
                  F = DateTimeOffset(2018,1,1,23,11,12,TimeSpan.Zero) ; 
                  G = TimeSpan.FromDays 30. 
                  H = Guid.Empty }

    type PocoRoundtrip() =
        inherit RoundtripBenchmark<T>(value)

module FloatArray =
    type T = float[]

    let value =
        [| for i in 1 .. 100 -> float i / float 100.
           yield Double.Epsilon
           yield Double.PositiveInfinity
           yield Double.NaN
           yield Double.NegativeInfinity |]

    type FloatArrayRoundtrip() =
        inherit RoundtripBenchmark<T>(value)

module BoxedArray =
    type T = obj[]

    let value : T = [|box 1; box "foo" ; box true ; box(1,2) ; box (ref 42) |]

    type BoxedArrayRoundtrip() =
        inherit RoundtripBenchmark<T>(value)

module Array3D =
    type T = float [,,]

    let value : T = Array3D.init 20 20 20 (fun i j k -> 0.1 * float i + float j + 10. * float k)
    
    type Array3DRoundtrip() =
        inherit RoundtripBenchmark<T>(value)

module CyclicObject =
    type T = obj[]

    let value : T = let x = [| null |] in x.[0] <- box x ; x
    
    type CyclicObjectRoundtrip() =
        inherit RoundtripBenchmark<T>(value)

module LargeTuple =
    type T = string * int * int * int * bool * string * (float * int list) option * int * int * int * string
    let value : T = ("lorem ipsum dolor", 1, 2, 3, true, "", Some(3.14, [2]), 3, 2, 1, "lorem")

    type LargeTupleRoundtrip() =
        inherit RoundtripBenchmark<T>(value)

module FSharpList =
    type T = int list
    let value : T = [1..1000]

    type FSharpListRoundtrip() =
        inherit RoundtripBenchmark<T>(value)

module Dictionary =
    open System.Collections.Generic
    open Poco

    type T = Dictionary<string, Poco.T>
    
    let mkDict size =
        let d = new T()
        for i = 1 to size do
            let key = sprintf "key-%d" i
            let value = { 
                A = i ; 
                B = sprintf "value-%d" i ; 
                C = i % 2 = 0 ; 
                D = [|byte i|]
                F = DateTimeOffset(2018,1,1,23,11,12,TimeSpan.Zero) ; 
                G = TimeSpan.FromDays 30.
                H = Guid.Empty
            } 

            d.Add(key, value)
        d

    let value = mkDict 1000

    type DictionaryRoundtrip() =
        inherit RoundtripBenchmark<T>(value)

module ExceptionBench =

    let rec mkExn d =
        if d = 0 then raise (FormatException "kaboom!")
        else
            1 + mkExn(d - 1)

    let exn = try mkExn 20 |> ignore ; failwith "" with :? FormatException as e -> e

    type ExceptionRoundtrip() =
        inherit RoundtripBenchmark<FormatException>(exn)

module LargeObject =
    type Foo = { A : int ; B : string ; C : bool }
    type Bar = A of int * string | B of Foo | C
    type T = Bar list list option * string list * byte []

    let value : T [] =
        Arb.from<T>.Generator |> Gen.sampleWithSeed (Rnd 2018UL) 20 20
    
    type LargeFSharpValueRoundtrip() =
        inherit RoundtripBenchmark<T []>(value)

module ISerializable =
    open System.Runtime.Serialization

    type T(x : int, y : string, z : bool) =
        interface ISerializable with
            member __.GetObjectData(si, _) =
                si.AddValue("x", x)
                si.AddValue("y", y)
                si.AddValue("z", z)

        new (si : SerializationInfo, _ : StreamingContext) =
            let inline get x = si.GetValue(x, typeof<'T>) :?> 'T
            new T(get "x", get "y", get "z")

    let instance = new T(42, "lorem ipsum", true)

    type ISerializableRoundtrip() =
        inherit RoundtripBenchmark<T>(instance)


module FSharpBinTree =
    type Tree = Node | Leaf of int * Tree * Tree

    let rec mkTree d =
        if d = 0 then Node
        else
            let t = mkTree (d-1)
            Leaf(d, mkTree(d-1), mkTree(d-1))

    let value = mkTree 10

    type FSharpBinaryRoundtrip() =
        inherit RoundtripBenchmark<Tree>(value)

module FSharpSet =
    let value = set [1 .. 40]

    type FSharpSetRoundtrip() =
        inherit RoundtripBenchmark<Set<int>>(value)


module FSharpMap =
    let value = [1 .. 40] |> Seq.map (fun i -> string i, i) |> Map.ofSeq

    type FSharpMapRountrip() =
        inherit RoundtripBenchmark<Map<string, int>>(value)


module MemberInfo =
    open System.Reflection
    open FSharp.Quotations.Patterns

    type private Foo =
        static member Method(x : int) = ()
        static member Method<'T>(x : 'T, y : Foo) = ()

    let value : MemberInfo = 
        match <@ Foo.Method("string", Unchecked.defaultof<_>) @> with
        | Call(_,m,_) -> m :> _
        | _ -> failwith "impossible"

    type MemberInfoRoundtrip() =
        inherit RoundtripBenchmark<MemberInfo>(value)

module CurriedFunction =

    type T = int list -> int list

    let closure = (@) [ 1 .. 10 ]

    type ClosureBenchmark() =
        inherit RoundtripBenchmark<T>(closure)

module Quotation =
    type T = Quotations.Expr<Async<int>>
    
    let value =
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

    type QuotationRoundtrip() =
        inherit RoundtripBenchmark<T>(value)