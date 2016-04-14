namespace Nessos.FsPickler.Tests

open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Reflection
open System.Runtime.Serialization
open System.Threading.Tasks

open Nessos.FsPickler
open Nessos.FsPickler.Hashing
open Nessos.FsPickler.Combinators
open Nessos.FsPickler.Json

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

    let testCloneRef (x : 'T) =
        let y = clone x
        y |> isNotSameTo x
    
    let testCloneEq (x : 'T) =
        let y = clone x
        y |> should equal x

    let testCloneRefEq (x : 'T) =
        let y = clone x
        y |> should equal x
        y |> isNotSameTo x

    let testClonePayload (proj : 'T -> 'S) (t : 'T) =
        let t' = clone t
        proj t' |> isNotSameTo (proj t)

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
#if NET35
#else
        isRecursive<bigint> |> should equal false
#endif
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
#if NET35
#else
        isFixedSize<bigint> |> should equal false
#endif
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
        FsPickler.IsSerializableType<SerializableInheritingNonSerializable> () |> should equal true

    [<Test; Category("Pickler tests")>]
    let ``1. Should mark serializable instances of nonserializable interfaces serializable`` () =
        FsPickler.IsSerializableType<NonSerializableInterface> () |> should equal true
        FsPickler.IsSerializableType<SerializableImplementingNonSerializable> () |> should equal true

    [<Test; Category("Pickler tests")>]
    let ``1. Should mark types carrying the SerializableAttribute serializable`` () =
        FsPickler.IsSerializableType<SerializableOnAccountOfAttribute> () |> should equal true

    let mutable private isRunSerializableDeclarationTest = false
    [<Test; Category("Pickler tests")>]
    let ``1. Serializable type declaration simple test`` () =
        // ensure test is only run once per AppDomain
        if isRunSerializableDeclarationTest then () else
        isRunSerializableDeclarationTest <- true

        FsPickler.DeclareSerializable<DeclaredSerializableType> ()
        let p = FsPickler.GeneratePickler<DeclaredSerializableType> ()
        ()

    let mutable private isRunPicklerFactoryTest = false
    [<Test; Category("Pickler tests")>]
    let ``1. Pickler factory simple test`` () =
        // ensure test is only run once per AppDomain
        if isRunPicklerFactoryTest then () else
        isRunPicklerFactoryTest <- true

        let factory _ = Pickler.FromPrimitives((fun _ -> failwith ""), (fun _ _ -> failwith ""))

        FsPickler.RegisterPicklerFactory<PicklerFactoryType> factory

        let p = FsPickler.GeneratePickler<PicklerFactoryType> ()
        p.PicklerInfo |> should equal PicklerInfo.UserDefined

    let mutable private isRunPicklerConcurrencyTest = false
    let private gen<'T> () = FsPickler.GeneratePickler<'T>() |> ignore
    let private reg<'T> () = FsPickler.DeclareSerializable<'T> ()
    [<Test; Category("Pickler tests")>]
    let ``1. Pickler registry concurrency test`` () =
        // ensure test is only run once per AppDomain
        if isRunPicklerConcurrencyTest then () else
        isRunPicklerConcurrencyTest <- true

        // test that registration behaves correctly in conjunction
        // with concurrent pickler generation operations.

        [| 
            gen<Foo0> ; reg<Bar0> ; 
            gen<Foo1> ; reg<Bar1> ; 
            gen<Foo2> ; reg<Bar2> ; 
            gen<Foo3> ; reg<Bar3> ; 
            gen<Foo4> ; reg<Bar4> ;
            gen<Foo5> ; reg<Bar5> ;
            gen<Foo6> ; reg<Bar6> ;
            gen<Foo7> ; reg<Bar7> ;
            gen<Foo8> ; reg<Bar8> ;
            gen<Foo9> ; reg<Bar9> ;
        |] |> Array.Parallel.iter (fun f -> f ())

        [| 
            gen<Bar0> ; 
            gen<Bar1> ; 
            gen<Bar2> ; 
            gen<Bar3> ; 
            gen<Bar4> ;
            gen<Bar5> ;
            gen<Bar6> ;
            gen<Bar7> ;
            gen<Bar8> ;
            gen<Bar9> ;
        |] |> Array.Parallel.iter (fun f -> f ())
    
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
    let ``2. Clone: string`` () = Check.QuickThrowOnFail<string> testCloneRefEq

    let checkArray<'T> () =
        testCloneRefEq (null : 'T [])
        Check.QuickThrowOnFail<'T []> (testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<'T [,]> (testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<'T [,,]> (testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<'T [,,,]> (testCloneRefEq, maxRuns = 10)

    [<Test; Category("Clone")>]
    let ``2. Clone: array int`` () = checkArray<int> ()

    [<Test; Category("Clone")>]
    let ``2. Clone: array string`` () = checkArray<string> ()

    [<Test; Category("Clone")>]
    let ``2. Clone: array byte`` () = checkArray<byte> ()

    [<Test; Category("Clone")>]
    let ``2. Clone: array enum`` () = checkArray<Enum> () ; checkArray<CharEnum> ()

    [<Test; Category("Clone")>]
    let ``2. Clone: cached array`` () = 
        let xs = [|1uy .. 10uy|]
        testCloneRef [|for i in 1 .. 100 -> xs|]
        let xs = [|1 .. 10|]
        testCloneRef [|for i in 1 .. 100 -> xs|]
        let xs = [|"test"|]
        testCloneRef [|for i in 1 .. 100 -> xs|]
        [|obj()|] |> testClonePayload (fun xs -> xs.[0])

    [<Test; Category("Clone")>]
    let ``2. Clone: optional`` () = 
        Check.QuickThrowOnFail<int option> (testCloneRefEq, maxRuns = 10)
        Some (obj()) |> testClonePayload (fun o -> o.Value)

    [<Test; Category("Clone")>]
    let ``2. Clone: ref`` () = 
        Check.QuickThrowOnFail<int ref> (testCloneRefEq, maxRuns = 10)
        ref (obj()) |> testClonePayload (fun r -> r.Value)

    [<Test; Category("Clone")>]
    let ``2. Clone: Nullable`` () = 
        Check.QuickThrowOnFail<Nullable<int>> (testCloneEq, maxRuns = 10)
        let s = new GenericStruct<_>(obj())
        new Nullable<_>(s) |> testClonePayload (fun n -> n.Value.Value)

    [<Test; Category("Clone")>]
    let ``2. Clone: list`` () = 
        // unions encode certain branches as singletons
        Check.QuickThrowOnFail<int list> (function [] as l -> testCloneEq l | _ as l -> testCloneRefEq l)
        [obj()] |> testClonePayload List.head

    [<Test; Category("Clone")>]
    let ``2. Clone: struct`` () = 
        let c = new StructType(42,"42")
        let c' = clone c
        c'.X |> should equal c.X
        c'.Y |> should equal c.Y
        new GenericStruct<_>(obj()) |> testClonePayload (fun s -> s.Value)

    [<Test; Category("Clone")>]
    let ``2. Clone: simple class`` () = 
        let c = new SimpleClass(42, "42")
        let c' = clone c
        c'.Value |> should equal c.Value

    [<Test; Category("Clone")>]
    let ``2. Clone: generic class`` () = 
        let gc = new GenericClass<_>(obj())
        testCloneRef gc
        gc |> testClonePayload (fun c -> c.Value)

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
    let ``2. Clone: class with lone CollectionDataContractAttribute`` () = 
        let d = new ClassWithLoneCollectionDataContractAttribute<int>([1 .. 10])
        let d' = clone d
        d'.Values |> should equal d.Values

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
    let ``2. Clone: Exception without ISerializable`` () = 
        let e = new ExceptionWithoutISerializable<int>(42, "Message", new Exception()) |> addStackTrace
        let e' = clone e
        e'.Value |> should equal e.Value
        e'.InnerException.Message |> should equal e.InnerException.Message
        e'.StackTrace |> should equal e.StackTrace

    [<Test; Category("Clone")>]
    let ``2. Clone: record`` () = 
        Check.QuickThrowOnFail<Record> (testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<Record> (testCloneRefEq, maxRuns = 10)
        { GValue = obj() } |> testClonePayload (fun r -> r.GValue)

    [<Test; Category("Clone")>]
    let ``2. Clone: union`` () = 
        // unions encode certain branches as singletons
        Check.QuickThrowOnFail<SimpleDU> testCloneEq
        Check.QuickThrowOnFail<GenericDU<int>> (testCloneEq, maxRuns = 10)
        Check.QuickThrowOnFail<Peano> (function Zero -> testCloneEq Zero | Succ _ as p -> testCloneRefEq p)
        Check.QuickThrowOnFail<Forest<int>> testCloneEq
        Check.QuickThrowOnFail<Either<int,int>> (testCloneRefEq, maxRuns = 10)
        GValue(obj()) |> testClonePayload (function GValue v -> v)

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
        (obj(),obj()) |> testClonePayload fst

    [<Test; Category("Clone")>]
    let ``2. Clone: choice`` () = 
        Check.QuickThrowOnFail<Choice<int,int>>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<Choice<int,int,int>>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<Choice<int,int,int,int>>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<Choice<int,int,int,int,int>>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<Choice<int,int,int,int,int,int>>(testCloneRefEq, maxRuns = 10)
        Check.QuickThrowOnFail<Choice<int,int,int,int,int,int,int>>(testCloneRefEq, maxRuns = 10)
        Choice1Of2 (obj()) |> testClonePayload(function Choice1Of2 v -> v | _ -> failwith "error")

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

    [<Test; Category("Pickler tests")>]
    let ``2. Clone: cloneable only types`` () =
        FsPickler.IsSerializableType<CloneableOnlyType> () |> should equal false
        let cov = new CloneableOnlyType()
        FsPickler.Clone cov |> should equal cov
        FsPickler.ComputeHash [|cov|] |> ignore
        FsPickler.ComputeSize [cov] |> ignore
        fun () -> Binary.pickle Pickler.auto<_> [box 1 ; box cov] |> ignore
        |> shouldFailwith<FsPicklerException>
        FsPickler.EnsureSerializable(box [cov], failOnCloneableOnlyTypes = false)
        fun () -> FsPickler.EnsureSerializable(box [cov], failOnCloneableOnlyTypes = true)
        |> shouldFailwith<NonSerializableTypeException>

    [<Test; Category("Pickler tests")>]
    let ``2. Clone: Hash id parsing`` () =
        let seed = FsPickler.ComputeHash [|1 .. 10000|]
        Check.QuickThrowOnFail<int64 * Type * byte []>(
            fun (l : int64, t : Type, bytes : byte []) ->
                let hash = { seed with Type = t.FullName ; Hash = bytes ; Length = abs l }
                let id = hash.Id
                HashResult.Parse id |> should equal hash)

    //
    //  In-memory sifting tests
    //

    [<Test; Category("Sift")>]
    let ``3. Object: simple sift`` () =
        let graph : (int * int []) option * int [] option option list = (Some (1, [|1 .. 100|]), [None; None ; Some None; Some (Some [|12|])])
        let sifter = { new IObjectSifter with member __.Sift(p,_,_) = p.Kind = Kind.Array }
        let sifted, values = FsPickler.Sift(graph, sifter)
        values.Length |> should equal 2
        FsPickler.UnSift(sifted, values) |> should equal graph

    [<Test; Category("Sift")>]
    let ``3. Object: sifting with reference equality`` () =
        let array = [|1 .. 100|]
        let graph = [array; array]
        let sifted, sifts = FsPickler.Sift(graph, (function :? Array -> true | _ -> false))
        sifts.Length |> should equal 1
        let graph' = FsPickler.UnSift(sifted, sifts)
        graph' |> should equal graph

    [<Test; Category("Sift")>]
    let ``3. Object: sifting boxed primitives`` () =
        let value = [|1|]
        let graph = [box value; box 2; box 3 ; box 4; box value]
        let sifted, sifts = FsPickler.Sift(graph, (function :? Array -> true | :? int -> true | _ -> false))
        sifts.Length |> should equal 1
        let graph' = FsPickler.UnSift(sifted, sifts)
        graph' |> should equal graph

    [<Test; Category("Sift")>]
    let ``3. Object: sifting boxed values`` () =
        let value = [|1 .. 100|]
        let sifted, sifts = FsPickler.Sift(box value, (function :? Array -> true | _ -> false))
        sifts.Length |> should equal 1
        let value' = FsPickler.UnSift(sifted, sifts) :?> int []
        value' |> should equal value

    [<Test; Category("Sift")>]
    let ``3. Object: random sift`` () =
        let r = new System.Random()
        let randomSifter = { new IObjectSifter with member __.Sift(_,_,_) = r.Next(0,5) = 0 }
        Check.QuickThrowOnFail(fun (tree : ListTree<int>) ->
            let sifted, values = FsPickler.Sift(tree, randomSifter)
            FsPickler.UnSift(sifted, values) |> should equal tree)

    [<Test; Category("Sift"); Repeat(5)>]
    let ``3. Object: random graph sifting`` () =
        let g = createRandomGraph 0.4 30
        let r = new System.Random()
        let randomSifter = { new IObjectSifter with member __.Sift(_,_,_) = r.Next(0,5) = 0 }
        let sifted, values = FsPickler.Sift(g, randomSifter)
        let g' = FsPickler.UnSift(sifted, values)
        areEqualGraphs g g' |> should equal true

    //
    //  Object visitor
    //

    [<Test; Category("Visitor")>]
    let ``4. Visitor: simple node count`` () =
        let count = ref 0
        let visitor = { new IObjectVisitor with member __.Visit(_,_) = incr count ; true }
        FsPickler.VisitObject(visitor, ((1,2), (3,4)))
        count.Value |> should equal 7
        
    [<Test; Category("Visitor")>]
    let ``4. Visitor: cyclic object`` () =
        let count = ref 0
        let visitor = { new IObjectVisitor with member __.Visit(_,_) = incr count ; true }
        let rec r = { Rec = r }
        FsPickler.VisitObject(visitor, r)
        count.Value |> should equal 1

    [<Test; Category("Visitor")>]
    let ``4. Visitor: specialized int counter`` () =
        let count = ref 0
        let visitor = 
            { 
                new ISpecializedObjectVisitor<int> with 
                    member __.Visit(_,_) = true 
                    member __.VisitSpecialized(_,i) = count := !count + i ; true
            }

        FsPickler.VisitObject(visitor, ([1 .. 49], Some 50, [51 .. 100]))
        count.Value |> should equal 5050

    [<Test; Category("Visitor")>]
    let ``4. Visitor: cancellation`` () =
        let count = ref 0
        let visitor = 
            { 
                new IObjectVisitor with 
                    member __.Visit(_,_) = incr count ; !count < 1000
            }

        FsPickler.VisitObject(visitor, [for i in 1 .. 100 -> (obj(), [|1 .. 10|])])
        count.Value |> should equal 1000

    [<Test; Category("Visitor")>]
    let ``4. Visitor: specialized cancellation`` () =
        let count = ref 0
        let visitor = 
            { 
                new ISpecializedObjectVisitor<string> with 
                    member __.Visit(_,_) = incr count ; true
                    member __.VisitSpecialized(_,x:string) = false
            }

        FsPickler.VisitObject(visitor, [for i in 1 .. 1000 -> if i = 50 then Choice1Of2 "string" else Choice2Of2 i])
        count.Value |> should equal 100

    [<Test; Category("Visitor")>]
    let ``4. Visitor: traverse order`` () =
        let value = Node("1", Node("2", Leaf, Leaf), Node("3", Leaf, Leaf))
        let order = new ResizeArray<string> ()
        let visitor = 
            { 
                new ISpecializedObjectVisitor<BinTree> with 
                    member __.Visit(_,_) = true
                    member __.VisitSpecialized(_,t:BinTree) =
                        match t with
                        | Leaf -> ()
                        | Node(id,_,_) -> order.Add id
                        true
            }

        FsPickler.VisitObject(visitor, value, visitOrder = VisitOrder.PreOrder)
        order.ToArray() |> should equal [|"1";"2";"3"|]
        order.Clear()

        FsPickler.VisitObject(visitor, value, visitOrder = VisitOrder.PostOrder)
        order.ToArray() |> should equal [|"2";"3";"1"|]

    [<Test; Category("Visitor")>]
    let ``4. Visitor: should properly visit nulls 1`` () =
        let visited = ref 0
        let visitor = { new IObjectVisitor with member __.Visit(_,_) = incr visited ; true }
        FsPickler.VisitObject(visitor, Unchecked.defaultof<SimpleClass>)
        visited.Value |> should equal 1
        
    [<Test; Category("Visitor")>]
    let ``4. Visitor: should properly visit nulls 2`` () =
        let hasFoundNull = ref false
        let visitor =
            {
                new IObjectVisitor with
                    member __.Visit(_,v) = 
                        if obj.ReferenceEquals(v,null) then hasFoundNull := true
                        true
            }

        let graphContainingAbstractNodeWithNull = Some ([|1;2;3|], ("test", Unchecked.defaultof<IAbstract>))

        FsPickler.VisitObject(visitor, graphContainingAbstractNodeWithNull)

        hasFoundNull.Value |> should equal true

    [<Test; Category("Visitor")>]
    let ``4. Visitor: ensure serializable`` () =
        let mkGraph (o:obj) = [box 1 ; box "" ; box <| Some (42, [box 1 ; o])]
        FsPickler.EnsureSerializable(mkGraph [1..100])
        FsPickler.EnsureSerializable(mkGraph (new System.ObjectDisposedException("")))
        shouldFailwith<NonSerializableTypeException>(fun () -> FsPickler.EnsureSerializable(mkGraph (new System.Net.WebClient())))