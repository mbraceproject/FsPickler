namespace MBrace.FsPickler.Benchmarks

open FsCheck

module Poco =
    [<CLIMutable>]
    type T =
        { A : int ; B : string ; C : bool ; D : byte[] }

    let value = { A = 42 ; B = "lorem ipsum" ; C = true ; D = [|1uy .. 20uy|] }

    type Roundtrip() =
        inherit RoundtripBenchmark<T>(value)

module LargeObject =
    type Foo = { A : int ; B : string ; C : bool }
    type Bar = A of int * string | B of Foo | C
    type T = Bar list list option * string list * byte []

    let value : T [] =
        Arb.from<T>.Generator |> Gen.sampleWithSeed (Rnd 2018UL) 20 20
    
    type Roundtrip() =
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

    type Roundtrip() =
        inherit RoundtripBenchmark<T>(instance)