namespace MBrace.FsPickler.Benchmarks

open FsCheck

module Poco =
    [<CLIMutable>]
    type Foo =
        { A : int ; B : string ; C : bool ; D : byte[] }

    let value = { A = 42 ; B = "lorem ipsum" ; C = true ; D = [|1uy .. 20uy|] }

    type Roundtrip() =
        inherit RoundtripBenchmark<Foo>(value)

module LargeObject =
    type Foo = { A : int ; B : string ; C : bool }
    type Bar = A of int * string | B of Foo | C
    type T = Bar list list option * string list * byte []

    let value : T [] =
        Arb.from<T>.Generator |> Gen.sampleWithSeed (Rnd 2018UL) 20 20
    
    type Roundtrip() =
        inherit RoundtripBenchmark<T []>(value)