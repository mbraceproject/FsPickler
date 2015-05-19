namespace Nessos.FsPickler.Tests

open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Reflection
open System.Runtime.Serialization
open System.Threading.Tasks

open Nessos.FsPickler
open Nessos.FsPickler.Json
open Nessos.FsPickler.Combinators

open Nessos.FsPickler.Tests.TestTypes

open NUnit.Framework
open FsUnit
open FsCheck

[<TestFixture>]
module ``Generic Tests`` =

    let _ = Arb.register<FsPicklerGenerators> ()

    let clone x = FsPickler.Clone x

    let isNotSameTo (expected:'T) (actual:'T) = 
        if obj.ReferenceEquals(expected, null) then Assert.IsNull actual
        else
            Assert.IsNotNull actual
            Assert.AreNotSame(expected, actual) 
    
    let testCloneEq (x : 'T) =
        let y = FsPickler.Clone x
        y |> should equal x

    let testCloneRefEq (x : 'T) =
        let y = FsPickler.Clone x
        y |> should equal x
        y |> isNotSameTo x
        

    //
    //  Pickler generation tests
    //

    [<Test; Category("Pickler tests")>]
    let ``1. pickler generation order should not affect result`` () =
        // see type definitions on an explanation of what this test checks
        FsPickler.IsSerializableType<Foo> () |> should equal false
        FsPickler.IsSerializableType<Bar> () |> should equal false

    [<Test; Category("Pickler tests")>]
    let ``1. Correctly resolve recursive types`` () =
        isRecursive<int> |> should equal false
        isRecursive<DateTime> |> should equal false
        isRecursive<DateTimeOffset> |> should equal false
        isRecursive<bigint> |> should equal false
        isRecursive<string> |> should equal false
        isRecursive<Type> |> should equal false
        isRecursive<int * string []> |> should equal false
        isRecursive<Type option * string []> |> should equal false
        isRecursive<Record> |> should equal false
        isRecursive<SimpleDU> |> should equal false
        isRecursive<GenericClass<GenericClass<int>>> |> should equal false

        isRecursive<obj> |> should equal true
        isRecursive<Peano> |> should equal true
        isRecursive<int list> |> should equal true
        isRecursive<int -> int> |> should equal true
        isRecursive<RecursiveClass> |> should equal true
        isRecursive<CyclicClass> |> should equal true
        isRecursive<SimpleISerializableClass> |> should equal true
        isRecursive<GenericISerializableClass<int>> |> should equal true

    [<Test; Category("Pickler tests")>]
    let ``1. Correctly resolve finite types`` () =
        isFixedSize<int> |> should equal true
        isFixedSize<DateTime> |> should equal true
        isFixedSize<DateTimeOffset> |> should equal true
        isFixedSize<int * byte * (int * int64 * DateTime)> |> should equal true
        isFixedSize<string> |> should equal false
        isFixedSize<Type> |> should equal true
        isFixedSize<int * string []> |> should equal false
        isFixedSize<Type option * string []> |> should equal false
        isFixedSize<Record> |> should equal false
        isFixedSize<SimpleDU> |> should equal false
        isFixedSize<GenericClass<GenericClass<int>>> |> should equal true

        isFixedSize<obj> |> should equal false
        isFixedSize<Peano> |> should equal false
        isFixedSize<bigint> |> should equal false
        isFixedSize<int list> |> should equal false
        isFixedSize<int -> int> |> should equal false
        isFixedSize<RecursiveClass> |> should equal false
        isFixedSize<SimpleISerializableClass> |> should equal false

    [<Test; Category("Pickler tests")>]
    let ``1. detect polymorphic recursive types`` () =
        FsPickler.IsSerializableType<PolyRec<int>> () |> should equal false
        FsPickler.IsSerializableType<PolyRec<int> ref> () |> should equal false
        FsPickler.IsSerializableType<APoly<int, string>> () |> should equal false
        FsPickler.IsSerializableType<BPoly<int>> () |> should equal false
        FsPickler.IsSerializableType<PseudoPolyRec<int>> () |> should equal true

    [<Test; Category("Pickler tests")>]
    let ``1. Should mark subtypes of nonserializable types serializable`` () =
        FsPickler.IsSerializableType<NonSerializable> () |> should equal false
        FsPickler.IsSerializableType<SerializableInheritingNonSerializable> () |> should equal false

    [<Test; Category("Pickler tests")>]
    let ``1. Should mark serializable instances of nonserializable interfaces serializable`` () =
        FsPickler.IsSerializableType<NonSerializableInterface> () |> should equal true
        FsPickler.IsSerializableType<SerializableImplementingNonSerializable> () |> should equal true

    
    //
    //  Clone tests
    //

    [<Test; Category("Clone")>]
    let ``2. Clone: bool`` () = testCloneEq false ; testCloneEq true

    [<Test; Category("Clone")>]
    let ``2. Clone: byte`` () = Check.QuickThrowOnFail<byte> (testCloneEq, maxRuns = 10)

    [<Test; Category("Clone")>]
    let ``2. Clone: int32`` () = Check.QuickThrowOnFail<int32> (testCloneEq, maxRuns = 10)

    [<Test; Category("Clone")>]
    let ``2. Clone: string`` () = Check.QuickThrowOnFail<string> testCloneEq

    [<Test; Category("Clone")>]
    let ``2. Clone: byte []`` () = Check.QuickThrowOnFail<byte []> testCloneRefEq

    [<Test; Category("Clone")>]
    let ``2. Clone: array`` () = 
        testCloneRefEq (null : int [])
        Check.QuickThrowOnFail<string []> (testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<string [,]> (testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<string [,,]> (testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<string [,,,]> (testCloneRefEq, maxRuns = 10)

    [<Test; Category("Clone")>]
    let ``2. Clone: optional`` () = 
        Check.QuickThrowOnFail<int option> (testCloneRefEq, maxRuns = 10)

    [<Test; Category("Clone")>]
    let ``2. Clone: ref`` () = 
        Check.QuickThrowOnFail<int ref> (testCloneRefEq, maxRuns = 10)

    [<Test; Category("Clone")>]
    let ``2. Clone: Nullable`` () = 
        Check.QuickThrowOnFail<Nullable<int>> (testCloneEq, maxRuns = 10)

    [<Test; Category("Clone")>]
    let ``2. Clone: list`` () = 
        // unions encode certain branches as singletons
        Check.QuickThrowOnFail<int list> (function [] as l -> testCloneEq l | _ as l -> testCloneRefEq l)

    [<Test; Category("Clone")>]
    let ``2. Clone: struct`` () = 
        let c = new StructType(42,"42")
        let c' = clone c
        c'.X |> should equal c.X
        c'.Y |> should equal c.Y

    [<Test; Category("Clone")>]
    let ``2. Clone: simple class`` () = 
        let c = new SimpleClass(42, "42")
        let c' = clone c
        c'.Value |> should equal c.Value

    [<Test; Category("Clone")>]
    let ``2. Clone: simple datacontract`` () = 
        let d = new DataContractClass<_>((1,2), "42")
        let d' = clone d
        d'.A |> should equal d.A
        d'.B |> should equal d'.B
        d'.A |> isNotSameTo d.A

    [<Test; Category("Clone")>]
    let ``2. Clone: field datacontract`` () = 
        let d = new FieldDataContractClass<_>((1,2), "42")
        let d' = clone d
        d'.A |> should equal d.A
        d'.A |> isNotSameTo d.A
        d'.B |> should equal d'.B

    [<Test; Category("Clone")>]
    let ``2. Clone: field datacontract with parameterless ctor`` () = 
        let d = new DataContractWithParameterlessConstructor()
        d.A <- "test"
        let d' = clone d
        d'.A |> should equal d.A
        d' |> isNotSameTo d

    [<Test; Category("Clone")>]
    let ``2. Clone: delegate simple`` () = 
        let d = Func<int>(fun () -> 1 + 1)
        let d' = clone d
        d'.Invoke() |> should equal 2

    [<Test; Category("Clone")>]
    let ``2. Clone: delegate multicast`` () =
        DeleCounter.Value <- 0
        let f n = new TestDelegate(fun () -> DeleCounter.Value <- DeleCounter.Value + n) :> Delegate
        let g = Delegate.Combine [| f 1 ; f 2 |]
        let h = Delegate.Combine [| g ; f 3 |]
        let h' = clone h
        h' |> isNotSameTo h
        h'.DynamicInvoke [| |] |> ignore
        DeleCounter.Value |> should equal 6

    [<Test; Category("Clone")>]
    let ``2. Clone: ISerializable`` () = 
        let e = FSharpException(42, "fortytwo") :?> FSharpException |> addStackTrace
        let e' = clone e
        e'.Data0 |> should equal e.Data0
        e'.Data1 |> should equal e.Data1
        e'.StackTrace |> should equal e.StackTrace

    [<Test; Category("Clone")>]
    let ``2. Clone: record`` () = 
        Check.QuickThrowOnFail<Record> testCloneRefEq

    [<Test; Category("Clone")>]
    let ``2. Clone: union`` () = 
        // unions encode certain branches as singletons
        Check.QuickThrowOnFail<SimpleDU> testCloneEq
        Check.QuickThrowOnFail<Peano> (function Zero -> testCloneEq Zero | Succ _ as p -> testCloneRefEq p)
        Check.QuickThrowOnFail<Forest<int>> testCloneEq
        Check.QuickThrowOnFail<Either<int,int>> (testCloneRefEq, maxRuns = 10)

    [<Test; Category("Clone")>]
    let ``2. Clone: tuple`` () = 
        Check.QuickThrowOnFail<Tuple<int>>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<int * string>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<int * string * string>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<int * string * string * string>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<int * string * string * string * string>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<int * string * string * string * string * string>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<int * string * string * string * string * string * string>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<int * string * string * string * string * string * string * string>(testCloneRefEq, maxRuns = 10)

    [<Test; Category("Clone")>]
    let ``2. Clone: choice`` () = 
        Check.QuickThrowOnFail<Choice<int,int>>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<Choice<int,int,int>>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<Choice<int,int,int,int>>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<Choice<int,int,int,int,int>>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<Choice<int,int,int,int,int,int>>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<Choice<int,int,int,int,int,int,int>>(testCloneRefEq, maxRuns = 10)

    [<Test; Category("Clone")>]
    let ``2. Clone: quotation`` () = 
        let quot =
            <@
                do int2Peano 42 |> ignore

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

        let quot' = clone quot
        quot'.ToString() |> should equal (quot.ToString())
        
    [<Test; Category("Clone")>]
    let ``2. Clone: caching`` () = 
        let x = obj ()
        let y,z = clone ((x,x))
        Assert.AreSame(y,z)

    [<Test; Category("Clone")>]
    let ``2. Clone: recursive objects`` () = 
        let x = Array.zeroCreate<obj> 10
        for i = 0 to 9 do x.[i] <- box x
        let y = clone x
        for z in y do Assert.AreSame(y,z :?> _)

    [<Test; Category("Clone"); Repeat(5)>]
    let ``2. Clone: random graph`` () = 
        let g = createRandomGraph 0.7 20
        let g' = clone g
        g' |> isNotSameTo g
        areEqualGraphs g g' |> should equal true

    //
    //  In-memory sifting tests
    //

    [<Test; Category("Sift")>]
    let ``3. Object: simple sift`` () =
        let graph : (int * int []) option * int [] option option list = (Some (1, [|1 .. 100|]), [None; None ; Some None; Some (Some [|12|])])
        let sifter = { new IObjectSifter with member __.Sift(p,_) = p.TypeKind = TypeKind.Array }
        let sifted, values = FsPickler.Sift(graph, sifter)
        values.Length |> should equal 2
        FsPickler.UnSift(sifted, values) |> should equal graph

    [<Test; Category("Sift")>]
    let ``3. Object: tuple sifting`` () =
        // test that values are sifted even if they are not cached by reference
        let tuple = (1,2,3,4,5,6,7,8)
        let p = FsPickler.GeneratePickler (tuple.GetType())
        p.IsCacheByRef |> should equal false
        let xs = Array.init 10 (fun _ -> tuple)
        let calls = ref 0
        let sifted, values = FsPickler.Sift(xs, fun o -> if obj.ReferenceEquals(o,tuple) then incr calls ; true else false)
        calls.Value |> should equal 1
        values.Length |> should equal 1
        FsPickler.UnSift(sifted, values) |> should equal xs

    [<Test; Category("Sift")>]
    let ``3. Object: random sift`` () =
        let r = new System.Random()
        let randomSifter = { new IObjectSifter with member __.Sift(_,_) = r.Next(0,5) = 0 }
        Check.QuickThrowOnFail(fun (tree : ListTree<int>) ->
            let sifted, values = FsPickler.Sift(tree, randomSifter)
            FsPickler.UnSift(sifted, values) |> should equal tree)

    [<Test; Category("Sift"); Repeat(5)>]
    let ``3. Object: random graph sifting`` () =
        let g = createRandomGraph 0.4 30
        let r = new System.Random()
        let randomSifter = { new IObjectSifter with member __.Sift(_,_) = r.Next(0,5) = 0 }
        let sifted, values = FsPickler.Sift(g, randomSifter)
        let g' = FsPickler.UnSift(sifted, values)
        areEqualGraphs g g' |> should equal true