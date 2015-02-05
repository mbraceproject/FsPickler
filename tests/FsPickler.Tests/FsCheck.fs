namespace Nessos.FsPickler.Tests

open FsCheck

type FsPicklerGenerators =
    static member BigInt = Arb.generate<byte []> |> Gen.map (fun bs -> System.Numerics.BigInteger(bs)) |> Arb.fromGen
    static member Array2D<'T> () = Arb.generate<'T> |> Gen.array2DOf |> Arb.fromGen
    static member Array3D<'T> () =
        let mkSized (n : int) =
            gen {
                // sqrt is good enough here
                let chooseSqrtOfSize = Gen.choose(0, n |> float |> sqrt |> int)
                let! N = chooseSqrtOfSize
                let! M = chooseSqrtOfSize
                let! K = chooseSqrtOfSize
                let g = Arb.generate<'T>
                let! seed = Gen.arrayOfLength (N*M*K) g
                return Array3D.init N M K (fun n m k -> seed.[n + m * N + k * N * M])
            }

        mkSized |> Gen.sized |> Arb.fromGen

    static member Array4D<'T> () =
        let mkSized (n : int) =
            gen {
                let chooseSqrtOfSize = Gen.choose(0, n |> float |> sqrt |> sqrt |> int)
                let! N = chooseSqrtOfSize
                let! M = chooseSqrtOfSize
                let! K = chooseSqrtOfSize
                let! L = chooseSqrtOfSize
                let g = Arb.generate<'T>
                let! seed = Gen.arrayOfLength (N*M*K*L) g
                return Array4D.init N M K L (fun n m k l -> seed.[n + m * N + k * N * M + l * N * M * K])
            }

        mkSized |> Gen.sized |> Arb.fromGen

    static member Seq<'T> () = Arb.generate<'T []> |> Gen.map Array.toSeq |> Arb.fromGen

            
type Check =
    // quick check methods with explicit type annotation
    static member QuickThrowOnFail<'T> (f : 'T -> unit) = Check.QuickThrowOnFailure f
    static member QuickThrowOnFail<'T> (f : 'T -> bool) = Check.QuickThrowOnFailure f