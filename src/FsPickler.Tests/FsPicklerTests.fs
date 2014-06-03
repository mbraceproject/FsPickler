namespace Nessos.FsPickler.Tests

    open System
    open System.Collections
    open System.Collections.Generic
    open System.Reflection
    open System.Runtime.Serialization

    open Nessos.FsPickler
    open Nessos.FsPickler.Combinators

    open Nessos.FsPickler.Tests.TestTypes

    open NUnit.Framework
    open FsUnit
    open FsCheck

    [<TestFixture("FsPickler.Binary")>]
    [<TestFixture("FsPickler.BclBinary")>]
    [<TestFixture("FsPickler.Xml")>]
    [<TestFixture("FsPickler.Json")>]
    type ``FsPickler Tests`` (picklerName : string) as self =

        let pickler = FsPicklerSerializer.Activate(picklerName).Pickler

        let _ = Arb.register<FsPicklerQCGenerators> ()

        let testLoop x = self.TestLoop x
        let testEquals x = self.TestLoop x |> should equal x
        let testReflected x =
            let y = self.TestLoop x
            y.GetType() |> should equal (x.GetType())
            y.ToString() |> should equal (x.ToString())

        member __.Pickler = pickler

        abstract IsRemoted : bool
        default __.IsRemoted = false

        abstract TestPickle : 'T -> obj
        default __.TestPickle (t : 'T) =
            match pickler with
            | :? StringPickler as p -> p.Pickle t :> obj
            | :? BinaryPickler as p -> p.Pickle t :> obj
            | _ -> invalidOp "unexpected pickler type."

        abstract TestLoop : 'T -> 'T
        default __.TestLoop (t : 'T) =
            match pickler with
            | :? StringPickler as p -> let pickle = p.Pickle t in p.UnPickle pickle
            | :? BinaryPickler as p -> let pickle = p.Pickle t in p.UnPickle pickle
            | _ -> invalidOp "unexpected pickler type."

        //
        //  Primitive Serialization tests
        //

        [<Test; Category("Primitives")>]
        member __.``Primitive: Bool`` () = testEquals false ; testEquals true

        [<Test; Category("Primitives")>]
        member __.``Primitive: Byte`` () = Check.QuickThrowOnFail<byte> testEquals

        [<Test; Category("Primitives")>]
        member __.``Primitive: SByte`` () = Check.QuickThrowOnFail<sbyte> testEquals

        [<Test; Category("Primitives")>]
        member __.``Primitive: Int16`` () = Check.QuickThrowOnFail<int16> testEquals

        [<Test; Category("Primitives")>]
        member __.``Primitive: Int32`` () = Check.QuickThrowOnFail<int32> testEquals

        [<Test; Category("Primitives")>]
        member __.``Primitive: Int64`` () = Check.QuickThrowOnFail<int64> testEquals

        [<Test; Category("Primitives")>]
        member __.``Primitive: UInt16`` () = Check.QuickThrowOnFail<uint16> testEquals

        [<Test; Category("Primitives")>]
        member __.``Primitive: UInt32`` () = Check.QuickThrowOnFail<uint32> testEquals

        [<Test; Category("Primitives")>]
        member __.``Primitive: UInt64`` () = Check.QuickThrowOnFail<uint64> testEquals

        [<Test; Category("Primitives")>]
        member __.``Primitive: Single`` () = Check.QuickThrowOnFail<single> testEquals

        [<Test; Category("Primitives")>]
        member __.``Primitive: Double`` () = Check.QuickThrowOnFail<double> testEquals

        [<Test; Category("Primitives")>]
        member __.``Primitive: Decimal`` () = Check.QuickThrowOnFail<decimal> testEquals

        [<Test; Category("Primitives")>]
        member __.``Primitive: Char`` () = Check.QuickThrowOnFail<char> testEquals

        [<Test; Category("Primitives")>]
        member __.``Primitive: String`` () = testEquals (null : string) ; Check.QuickThrowOnFail<string> testEquals

        [<Test; Category("Primitives")>]
        member __.``Primitive: Date`` () = Check.QuickThrowOnFail<DateTime> testEquals

        [<Test; Category("Primitives")>]
        member __.``Primitive: TimeSpan`` () = Check.QuickThrowOnFail<TimeSpan> testEquals

        [<Test; Category("Primitives")>]
        member __.``Primitive: Guid`` () = Check.QuickThrowOnFail<Guid> testEquals

        [<Test; Category("Primitives")>]
        member __.``Primitive: BigInteger`` () = Check.QuickThrowOnFail<bigint> testEquals

        [<Test; Category("Bytes")>]
        member __.``Primitive: Bytes`` () = testEquals (null : byte []) ; Check.QuickThrowOnFail<byte []> testEquals

        //
        //  Reflection types
        //

        [<Test; Category("Reflection types")>]
        member __.``Reflection: Type`` () = 
            // base types
            testEquals (null : Type) ; testEquals typeof<int> ; testEquals typeof<IEnumerable> ; testEquals <| Type.GetType("System.__Canon")
            // generic types
            testEquals typeof<int * string list option> ; testEquals typedefof<int * string option> ; testEquals typeof<Map<int, string>>
            // array types
            testEquals typeof<int []> ; testEquals typeof<int [,]> ; testEquals typeof<System.Array> ; testEquals typeof<(int * string) [,,,]>
            // generic type paramaters
            let tparams = typedefof<Map<_,_>>.GetGenericArguments() in testEquals tparams.[0] ; testEquals tparams.[1]
            // generic method parameters
            let mparams = typeof<ClassWithGenericMethod>.GetMethod("Method").GetGenericArguments() 
            testEquals mparams.[0] ; testEquals mparams.[1]

        [<Test; Category("Reflection types")>]
        member __.``Reflection: MemberInfo`` () =
            [| 
                typeof<obj> ; typeof<exn> ; typeof<int> ; typeof<string> ; typeof<bool> ; typeof<int option> ; 
                typeof<Quotations.Expr> ; typeof<System.Collections.Generic.Dictionary<int,string>> ; 
                typeof<int list> ; typedefof<_ list> ; typedefof<_ ref> ; typeof<OverLoaded> ;
                Pickler.auto<int * string>.GetType()
            |]
            |> Array.collect(fun t -> t.GetMembers(allFlags ||| BindingFlags.FlattenHierarchy))
            |> Array.iter testEquals

        [<Test; Category("Reflection types")>]
        member __.``Reflection: Assembly`` () =
            System.AppDomain.CurrentDomain.GetAssemblies()
            |> Array.iter testEquals


        //
        //  Composite types
        //

        // Tuples

        [<Test; Category("Generic BCL Types")>]
        member __.``Tuple: Simple`` () =
            Check.QuickThrowOnFail<Tuple<string>> testEquals
            Check.QuickThrowOnFail<string * byte> testEquals
            Check.QuickThrowOnFail<string * byte * double> testEquals
            Check.QuickThrowOnFail<string * byte * double * bigint> testEquals
            Check.QuickThrowOnFail<string * byte * double * bigint * int> testEquals
            Check.QuickThrowOnFail<string * byte * double * bigint * int * uint64> testEquals
            Check.QuickThrowOnFail<string * byte * double * bigint * int * uint64 * decimal> testEquals
            Check.QuickThrowOnFail<string * byte * double * bigint * int * uint64 * decimal * int> testEquals
            Check.QuickThrowOnFail<string * byte * double * bigint * int * uint64 * decimal * int * int> testEquals

        [<Test; Category("Generic BCL Types")>]
        member __.``Tuple: Nested`` () =
            Check.QuickThrowOnFail<int * (string * decimal)> testEquals
            Check.QuickThrowOnFail<(int * (bool * string)) * (string * int16)> testEquals
            Check.QuickThrowOnFail<(int * (bool * (sbyte * string * uint32) * (string * string)))> testEquals
            

        // Arrays

        member __.CheckArray<'T> () =
            Check.QuickThrowOnFail<'T []> testEquals
            Check.QuickThrowOnFail<'T [,]> testEquals
            Check.QuickThrowOnFail<'T [,,]> testEquals
            Check.QuickThrowOnFail<'T [,,,]> testEquals


        [<Test; Category("Generic BCL Types")>]
        member __.``Array: Bool`` () = __.CheckArray<bool> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``Array: Byte`` () = __.CheckArray<byte> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``Array: SByte`` () = __.CheckArray<sbyte> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``Array: Int16`` () = __.CheckArray<int16> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``Array: Int32`` () = __.CheckArray<int32> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``Array: Int64`` () = __.CheckArray<int64> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``Array: UInt16`` () = __.CheckArray<uint16> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``Array: UInt32`` () = __.CheckArray<uint32> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``Array: UInt64`` () = __.CheckArray<uint64> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``Array: String`` () = __.CheckArray<string> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``Array: Decimal`` () = __.CheckArray<decimal> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``Array: Byte []`` () = __.CheckArray<byte []> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``Array: int * string`` () = __.CheckArray<int * string> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``Array: string * (int * decimal)`` () = __.CheckArray<int * string> ()


        // exceptions

        // should properly serialize stacktrace
        member __.TestException(e : 'exn) = e |> addStackTrace |> testReflected

        [<Test; Category("Generic BCL Types")>]
        member __.``Exception: System.Exception`` () = __.TestException <| new Exception("exception message")

        [<Test; Category("Generic BCL Types")>]
        member __.``Exception: System.Exception with inner exception`` () =
            let inner = new Exception("inner") |> addStackTrace
            __.TestException <| new Exception("outer", inner)

        [<Test; Category("Generic BCL Types")>]
        member __.``Exception: Misc Exceptions`` () =
            __.TestException <| new InvalidOperationException()
            __.TestException <| new AccessViolationException()
            __.TestException <| new InvalidTimeZoneException()
            __.TestException <| new System.IO.EndOfStreamException()
            __.TestException <| new System.IO.InvalidDataException()
            __.TestException <| new System.IO.FileNotFoundException()


        // collections

        [<Test; Category("Generic BCL Types")>]
        member __.``Collections: Dictionary`` () =
            let testDictionary (data : seq<'K * 'V>) =
                let data = data |> Seq.distinctBy fst |> Seq.toList
                let d = dict data
                let d' = testLoop d
                let data' = d' |> Seq.map (function KeyValue(k,v) -> k,v) |> Seq.toList
                data' |> should equal data

            Check.QuickThrowOnFail<seq<int64 * (string * float)>> testDictionary
            Check.QuickThrowOnFail<seq<(int64 * string) * string>> testDictionary


        [<Test; Category("Generic BCL Types")>]
        member __.``Collections: HashSet`` () =
            let testSet (data : seq<'T>) =
                let data = data |> Seq.distinct |> Seq.toList
                let d = new HashSet<'T>(data)
                let data' = testLoop d |> Seq.toList
                data' |> should equal data

            Check.QuickThrowOnFail<seq<int64>> testSet
            Check.QuickThrowOnFail<seq<string>> testSet
            Check.QuickThrowOnFail<seq<int * string>> testSet
            






//
//        [<Test>] member __.``System.Type`` () = testEquals typeof<int>
//        [<Test>] member __.``Option types`` () = testEquals (Some 42) ; testEquals (None : obj option) ; testEquals (Some (Some "test"))
//        [<Test>] member __.``Simple DU`` () = testEquals A ; testEquals E ; testEquals (D(42, "42"))
//        [<Test>] member __.``Recursive DU`` () = testEquals Zero ; testEquals (int2Peano 42)
//        [<Test>] member __.``Mutual Recursive Unions`` () = testEquals <| nTree 6
//        [<Test>] member __.``Simple Class`` () = testEquals <| SimpleClass(42, "fortyTwo")
//        [<Test>] member __.``Generic Class`` () = testEquals <| new GenericClass<string * int>("fortyTwo", 42)
//        [<Test>] member __.``Recursive Class`` () = testEquals <| RecursiveClass(Some (RecursiveClass(None)))
//        [<Test>] member __.``Cyclic Object`` () = test <| CyclicClass()
//        [<Test>] member __.``ISerializable Class`` () = testEquals <| SerializableClass(42, "fortyTwo")
//
//        [<Test>] 
//        member __.``System.Reflection.MethodInfo`` () = 
//            testMembers typeof<int> ; testMembers typedefof<GenericClass<_>> ; 
//            testMembers typeof<Pickler> ; testMembers typeof<WriteState>
//
//        [<Test>]
//        member __.``Reflection: Avoid Recursion in MemberInfo values`` () =
////            try
//            let ms = typeof<OverLoaded>.GetMethods() 
//            let m0 = ms |> Seq.find(fun x -> x.Name = "A" && x.GetParameters().Length = 1) |> fun m -> m.MakeGenericMethod(typeof<int>)
//            testEquals m0
//            for m in ms do
//                testEquals m
//
//            // deserialization fails with binaryformatter,
//            // catch protocol error in remoted tests
////            with :? ProtocolError -> ()            
//        
//        [<Test>] 
//        member __.``Pickler Factory Class`` () = 
//            let x = ClassWithPicklerFactory(0) |> testLoop
//            x.Value |> should equal 42
//
//
//        [<Test>]
//        member __.``Should correctly deserialize boxed arrays`` () =
//            let boxed = { A = [| 1 .. 10|] ; B = [| 1 .. 4 |] }
//            testEquals boxed
//
//
//        [<Test>] 
//        member __.``Combinator-based Peano`` () =
//            let pp = 
//                Pickler.fix(fun peanoP -> 
//                    peanoP 
//                    |> Pickler.option 
//                    |> Pickler.wrap 
//                        (function None -> Zero | Some p -> Succ p) 
//                        (function Zero -> None | Succ p -> Some p))
//
//            let p = int2Peano 100
//
//            p |> Binary.pickle pp |> Binary.unpickle pp |> should equal p
//
//        [<Test>] 
//        member __.``Combinator-based Mutual Recursion`` () =
//            let tp,_ = getTreeForestPicklers Pickler.int
//            let t = nTree 6
//
//            t |> Binary.pickle tp |> Binary.unpickle tp |> should equal t
//
//        [<Test>]
//        member __.``NonSerializable Type`` () =
//            let fs = new System.IO.FileStream(System.IO.Path.GetTempFileName(), System.IO.FileMode.Open)
//            shouldFailWith<FsPicklerException>(fun () -> self.TestSerializer fs |> ignore)
//        
//        [<Test>] 
//        member __.``Cyclic Array`` () = 
//            let cyclicArray : obj [] = 
//                let array = Array.zeroCreate<obj> 10
//                for i = 0 to array.Length - 1 do
//                    array.[i] <- Some (array, i) :> obj
//                array
//
//            test cyclicArray
//
//        [<Test>]
//        member __.``Random Object Graph`` () =
//            let g = createRandomGraph 0.7 20
//
//            let g' = testLoop g
//
//            areEqualGraphs g g' |> should equal true
//
//        [<Test>] 
//        member __.``Lists`` () = 
//            testEquals []
//            testEquals [1..100] 
//            testEquals ([1..100] |> List.map (fun i -> i, string i))
//
//        [<Test>]
//        member __.``Arrays`` () =
//            testEquals [||]
//            testEquals ([|1.0 .. 100.0|] |> Array.map (fun i -> (i, i)))
//            testEquals ([|1L .. 100L|] |> Array.map (fun i -> TimeSpan i))
//
//        [<Test>]
//        member __.``Call-By-Value Sequence Pickler`` () =
//            let seqPickler = Pickler.seq Pickler.int
//
//            let state = ref 0
//            let sequence =
//                seq {
//                    while !state < 100 do
//                        yield !state
//                        incr state
//                }
//            
//            let sequence' = sequence |> Binary.pickle seqPickler |> Binary.unpickle seqPickler
//
//            // check that sequence has been evaluated
//            !state |> should equal 100
//
//            sequence' |> Seq.length |> should equal 100
//
//        [<Test>]
//        member __.``3D float array`` () =
//            let array = Array3D.init 10 10 10 (fun i j k -> float (i * j - k))
//            testEquals array
//
//        [<Test>]
//        member __.``4D tuple array`` () =
//            let array = Array4D.init 10 10 10 10 (fun i j k l -> i,j,k,l)
//            testEquals array
//
//        [<Test>]
//        member __.``Exceptions`` () =
//            test <| Exception("outer", Exception("inner"))
//            test <| ArgumentException()
//
//    
//        [<Test>]
//        member __.``FSharpException`` () =
//            let e = FsharpException(42, "fortyTwo")
//            match testLoop e with
//            | FsharpException(42, "fortyTwo") -> ()
//            | FsharpException _ when __.IsRemoted -> () // Protocol will not serialize fields correctly, do not compare
//            | e' -> failwithf "Expected '%A' but got '%A'." e e'
//
//
//        [<Test>]
//        member __.``Generic Dictionary`` () =
//            let d = [1..100] |> Seq.map (fun i -> i, string i) |> dict
//            let d' = testLoop d
//            Seq.toArray d |> should equal (Seq.toArray d')
//
//        [<Test>]
//        member __.``FSharpMap`` () =
//            let m = [1..100] |> Seq.map (fun i -> i, string i) |> Map.ofSeq
//            testEquals m
//
//        [<Test>]
//        member __.``FSharpSet`` () =
//            let s = [1..100] |> Seq.map (fun i -> i, string i) |> set
//            testEquals s
//
//        [<Test>]
//        member __.``FSharpRef`` () = testEquals (ref 42)
//
//        [<Test>]
//        member __.``Complex FSharp Type`` () =
//            let x = ((1, [Some(A, int2Peano 10); None], [|1.0 .. 100.0|]), [(1,2,3) ; (1,1,1)], set [1..100])
//            let y = testLoop x
//            x = y |> should equal true
//
//        [<Test>]
//        member __.``Simple Delegate`` () =
//            let d = System.Func<int, int>(fun x -> x + 1)
//            
//            (testLoop d).Invoke 41 |> should equal 42
//
//        [<Test>]
//        member __.``Multicast Delegate`` () =
//            DeleCounter.Value <- 0
//            let f n = new TestDelegate(fun () -> DeleCounter.Value <- DeleCounter.Value + n) :> Delegate
//            let g = Delegate.Combine [| f 1 ; f 2 |]
//            let h = Delegate.Combine [| g ; f 3 |]
//            (testLoop h).DynamicInvoke [| |] |> ignore
//            DeleCounter.Value |> should equal 6
//
//        [<Test>]
//        member __.``Struct Test`` () =
//            let s = new StructType(42, "foobar")
//            (testLoop s).X |> should equal 42
//            (testLoop s).Y |> should equal "foobar"
//
//        [<Test>]
//        member __.``Lazy Values`` () =
//            let v = lazy(if true then 42 else 0)
//            (testLoop v).Value |> should equal 42
//
//        [<Test>]
//        member __.``FSharp Function`` () =
//            let f x = x + 1
//
//            (testLoop f) 41 |> should equal 42
//
//        [<Test>]
//        member __.``FSharp Curried Function`` () =
//            let f x y = x + y
//
//            (testLoop (f 41)) 1 |> should equal 42
//
//
//        [<Test>]
//        member __.``Combinators with recursive bindings`` () =
//            let x = new ClassWithCombinators(12, None)
//            let y = new ClassWithCombinators(0, Some x)
//
//            let z = testLoop y
//
//            z.Value |> snd |> Option.map (fun x -> x.Value) |> Option.get |> fst |> should equal 42
//
//
//        [<Test>]
//        member __.``FSharp Closure`` () =
//            let f () =
//                let x = System.Random().Next()
//                fun () -> x + 1
//
//            let g = f ()
//
//            (testLoop g) () |> should equal (g ())
//
//
//        [<Test>]
//        member __.``FSharp Tree`` () = testEquals (mkTree 5)
//
//
//        [<Test>]
//        member __.``FSharp Cyclic Value`` () = 
//            let rec f = { Rec = f }
//            test f
//
//        [<Test>]
//        member __.``FSharp Builders`` () =
//            let infty =
//                seq {
//                    let i = ref 0
//                    while true do
//                        incr i
//                        yield !i
//                }
//
//            testLoop infty |> Seq.take 5 |> Seq.toArray |> should equal [|1..5|]
//
//        [<Test>]
//        member __.``FSharp Quotations`` () =
//            let quot =
//                <@
//                    do int2Peano 42 |> ignore
//
//                    async {
//                        let rec fibAsync n =
//                            async {
//                                match n with
//                                | _ when n < 0 -> return invalidArg "negative" "n"
//                                | _ when n < 2 -> return n
//                                | n ->
//                                    let! fn = fibAsync (n-1)
//                                    let! fnn = fibAsync (n-2)
//                                    return fn + fnn
//                            }
//
//                        let! values = [1..100] |> Seq.map fibAsync |> Async.Parallel
//                        return Seq.sum values
//                    }
//                @>
//
//            test quot
//
////        [<Test>]
////        member __.``Pluggable Pickler Factory`` () =
////            let (NGValue x) = testLoop (NGValue 0)
////            x |> should equal 42
////
////        [<Test>]
////        member __.``Pluggable Generic Pickler Factory`` () =
////            let x = testLoop (GenericType<int>(42))
////            x.Value |> should equal 0  
//
//        [<Test>]
//        member __.``Nullable type support`` () =
//            let x = Nullable<int>(42)
//            let y = Nullable<int> ()
//            testEquals x ; testEquals y
//
//            
//        [<Test>]
//        member __.``Test Massively Auto-Generated Objects`` () =
//            // generate serializable objects that reside in mscorlib and FSharp.Core
//            let inputData = 
//                Seq.concat
//                    [
//                        generateSerializableObjects typeof<int>.Assembly
//                        generateSerializableObjects typeof<_ option>.Assembly
//                        generateSerializableObjects <| Assembly.GetExecutingAssembly()
//                    ]
//
//            let test (t : Type, x : obj) =
//                try testLoop x |> ignore ; None
//                with 
////                | :? ProtocolError -> None
//                | e ->
//                    printfn "Serializing '%O' failed with error: %O" t e
//                    Some e
//
//            let results = inputData |> Seq.map test |> Seq.toArray
//            let failedResults = results |> Seq.choose id |> Seq.length
//
//            if failedResults > 10 then
//                let msg = sprintf "Too many random object serialization failures (%d out of %d)." failedResults results.Length
//                raise <| new AssertionException(msg)
//            else
//                printfn "Failed Serializations: %d out of %d." failedResults results.Length
//
//
//        [<Test>]
//        member __.``Int Sequence Serialization`` () =
//            testSequence __.Pickler [1 .. 10000] |> should equal true
//            testSequence __.Pickler [|1 .. 10000|] |> should equal true
//            
//            let customSeq =
//                seq {
//                    let cnt = ref 0
//                    while !cnt < 100 do
//                        yield 2 * !cnt
//                        incr cnt
//                }
//
//            testSequence __.Pickler customSeq |> should equal true
//
//        [<Test>]
//        member __.``String Sequence Serialization`` () =
//            testSequence __.Pickler (List.map string [1 .. 10000]) |> should equal true
//            testSequence __.Pickler (Array.map string [|1 .. 10000|]) |> should equal true
//
//            let customSeq =
//                seq {
//                    yield "string0"
//                    for i in 1 .. 10 do
//                        yield sprintf "string%d" i
//                }
//
//            testSequence __.Pickler customSeq |> should equal true
//
//        [<Test>]
//        member __.``Pair Sequence Serialization`` () =
//            testSequence __.Pickler ([1..10000] |> List.map (fun i -> string i, i)) |> should equal true
//            
//            let customSeq =
//                seq {
//                    for i in 1 .. 10000 do
//                        yield (string i, i)
//                }
//
//            testSequence __.Pickler customSeq |> should equal true
//
//
//
//    [<AbstractClass>]
//    type ``In-memory Correctness Tests`` (pickler : FsPicklerSerializer) =
//        inherit ``Serializer Correctness Tests`` (pickler)
//
//        override __.IsRemoted = false
//        override __.TestSerializer (x : 'T) = Serializer.write __.Pickler x
//        override __.TestDeserializer (bytes : byte []) = Serializer.read __.Pickler bytes
//        override __.TestLoop(x : 'T) = Serializer.roundtrip x __.Pickler
//
//        override __.Init () = ()
//        override __.Fini () = ()
//
//    type ``Binary Pickler In-Memory Tests`` () =
//        inherit ``In-memory Correctness Tests``(new FsPicklerBinary())
//
//    type ``Xml Pickler In-Memory Tests`` () =
//        inherit ``In-memory Correctness Tests``(new FsPicklerXml())
//
//    type ``Json Pickler In-Memory Tests`` () =
//        inherit ``In-memory Correctness Tests``(new FsPicklerJson())


//    [<AbstractClass>]
//    type ``Remoted Corectness Tests`` (pickler : FsPicklerSerializer) =
//        inherit ``Serializer Correctness Tests`` (pickler)
//
//        let mutable state = None : (ServerManager * SerializationClient) option
//
//        override __.IsRemoted = true
//
//        override __.TestSerializer(x : 'T) = 
//            match state with
//            | Some (_,client) -> Serializer.write client.Serializer x
//            | None -> failwith "remote server has not been set up."
//            
//        override __.TestDeserializer(bytes : byte []) =
//            match state with
//            | Some (_,client) -> Serializer.read client.Serializer bytes
//            | None -> failwith "remote server has not been set up."
//
//        override __.TestLoop(x : 'T) =
//            match state with
//            | Some (_,client) -> client.Test x
//            | None -> failwith "remote server has not been set up."
//
//        override __.Init () =
//            match state with
//            | Some _ -> failwith "remote server appears to be running."
//            | None ->
//                let mgr = new ServerManager(__.Pickler)
//                do mgr.Start()
//                do System.Threading.Thread.Sleep 2000
//                let client = mgr.GetClient()
//                state <- Some(mgr, client)
//
//        override __.Fini () =
//            match state with
//            | None -> failwith "no remote server appears to be running."
//            | Some (mgr,_) -> mgr.Stop() ; state <- None
//
//
//    type ``Binary Pickler Remoted Tests`` () =
//        inherit ``Remoted Corectness Tests``(new FsPicklerBinary())
//
//    type ``Xml Pickler Remoted Tests`` () =
//        inherit ``Remoted Corectness Tests``(new FsPicklerXml())
//
//    type ``Json Pickler Remoted Tests`` () =
//        inherit ``Remoted Corectness Tests``(new FsPicklerJson())