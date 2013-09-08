namespace FsCoreSerializer.Tests

    #nowarn "346"

    open System
    open System.Reflection
    open System.Runtime.Serialization

    open FsUnit
    open FsCoreSerializer

    open NUnit.Framework

    module TestTypes =

        type SimpleDU =
            | A
            | B
            | C
            | D of int * string
            | E
            | F of bool

        type Peano =
            | Zero
            | Succ of Peano

        let rec int2Peano n = match n with 0 -> Zero | n -> Succ(int2Peano(n-1))

        type SimpleClass(x : int, y : string) =
            member __.Value = string x + y
            override __.Equals y = match y with :? SimpleClass as y -> y.Value = __.Value | _ -> false

        type GenericClass<'T when 'T : equality>(x : 'T) =
            member __.Value = x
            override __.Equals y = match y with :? GenericClass<'T> as y -> y.Value = __.Value | _ -> false

        type RecursiveClass(x : RecursiveClass option) =
            member __.Value = x
            override __.Equals y = match y with :? RecursiveClass as y -> y.Value = __.Value | _ -> false

        type CyclicClass () as self =
            let s = Some (self, 42)
            member x.Value = s

        type SerializableClass(x : int, y : string) =
            new(s : SerializationInfo, _ : StreamingContext) =
                new SerializableClass(s.GetInt32 "integer", s.GetString "string")

            member __.Value = y + string x
            override __.Equals y = match y with :? SerializableClass as y -> y.Value = __.Value | _ -> false

            interface ISerializable with
                member __.GetObjectData(s : SerializationInfo, _ : StreamingContext) =
                    s.AddValue("string", y)
                    s.AddValue("integer", x)


        type FsCoreSerializableClass(x : int, y : string) =
            new(r : Reader) = new FsCoreSerializableClass(r.Read<int>(), r.Read<string>())

            member __.Value = string x + y
            override __.Equals y = match y with :? FsCoreSerializableClass as y -> y.Value = __.Value | _ -> false

            interface IFsCoreSerializable with
                member __.GetObjectData(w : Writer) =
                    w.Write x ; w.Write y


        exception FsharpException of int * string

        type Tree =
            | Leaf
            | Node of obj * Tree * Tree

        let rec mkTree (n : int) =
            match n with
            | 0 -> Leaf
            | n -> Node(n, mkTree(n-1), mkTree(n-1))

        type Rec = Rec of (Rec -> Rec)

        type FormatterFactoryTest() =
            interface IFormatterFactory with
                member __.Type = typeof<int * string * unit>
                member __.Create (resolver : Type -> Lazy<Formatter>) =
                    let writer (w : Writer) ((x,y,_) : int * string * unit) = w.BW.Write x ; w.BW.Write y
                    let reader (r : Reader) = (42, "42", ())
                    Formatter.Create(reader, writer, cache = false)


        type GenericType<'T when 'T : comparison>(x : 'T) =
            member __.Value = x

        type GenericTypeFormatter () =
            interface IGenericFormatterFactory

            member __.Create<'T when 'T : comparison> (resolver : Type -> Lazy<Formatter>) =
                let valueFmt = resolver typeof<'T>

                let writer (w : Writer) (g : GenericType<'T>) = 
                    w.WriteObj(valueFmt.Value, g.Value)

                let reader (r : Reader) = 
                    let value = r.ReadObj(valueFmt.Value) :?> 'T
                    new GenericType<'T>(Unchecked.defaultof<'T>)

                Formatter.Create(reader, writer)

        let registerCustomSerializers =
            Utils.runOnce (fun () ->
                FsCoreSerializerRegistry.RegisterFormatterFactory(new FormatterFactoryTest())
                FsCoreSerializerRegistry.RegisterGenericFormatter (new GenericTypeFormatter()))


        type TestDelegate = delegate of unit -> unit

        and DeleCounter () =
            static let mutable cnt = 0
            static member Value 
                with get () = cnt
                and set i = cnt <- i

    open TestTypes

    [<TestFixture>]
    [<AbstractClass>]
    type ``Serializer Correctness Tests`` () as self =

        do registerCustomSerializers ()

        let test x = self.TestLoop x |> ignore
        let testLoop x = self.TestLoop x
        let testEquals x = self.TestLoop x = x |> should equal true
        let testReflected x =
            let y = self.TestLoop x
            (y.GetType()) |> should equal (x.GetType())

        let testMembers (t : Type) =
            let members = t.GetMembers(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static)
            for m in members do test m 

        abstract TestSerializer : 'T -> byte []
        abstract TestDeserializer : byte [] -> obj
        abstract TestLoop : 'T -> 'T

        [<TestFixtureSetUp>] abstract Init : unit -> unit
        [<TestFixtureTearDown>] abstract Fini : unit -> unit

        [<Test>] member __.``Unit`` () = testEquals ()
        [<Test>] member __.``Boolean`` () = testEquals false
        [<Test>] member __.``Integer`` () = testEquals 1
        [<Test>] member __.``String`` () = testEquals "lorem ipsum dolor"
        [<Test>] member __.``Float`` () = testEquals 3.1415926
        [<Test>] member __.``Guid`` () = testEquals <| Guid.NewGuid()
        [<Test>] member __.``Decimal`` () = testEquals Decimal.MaxValue
        [<Test>] member __.``Byte []`` () = testEquals [|0uy .. 100uy|]
        [<Test>] member __.``DateTime`` () = testEquals DateTime.Now

        [<Test>] member __.``System.Type`` () = testEquals typeof<int>
        [<Test>] member __.``System.Reflection.MethodInfo`` () = testMembers typeof<int> ; testMembers typedefof<GenericClass<_>>
        [<Test>] member __.``Option types`` () = testEquals (Some 42) ; testEquals (None : obj option) ; testEquals (Some (Some "test"))
        [<Test>] member __.``Tuples`` () = testEquals (2,3) ; testEquals (2, "test", Some (3, Some 2)) ; testEquals (1,2,3,4,5,(1,"test"),6,7,8,9,10)
        [<Test>] member __.``Simple DU`` () = testEquals A ; testEquals E ; testEquals (D(42, "42"))
        [<Test>] member __.``Recursive DU`` () = testEquals Zero ; testEquals (int2Peano 42)
        [<Test>] member __.``Simple Class`` () = testEquals <| SimpleClass(42, "fortyTwo")
        [<Test>] member __.``Generic Class`` () = testEquals <| new GenericClass<string * int>("fortyTwo", 42)
        [<Test>] member __.``Recursive Class`` () = testEquals <| RecursiveClass(Some (RecursiveClass(None)))
        [<Test>] member __.``Cyclic Object`` () = test <| CyclicClass()
        [<Test>] member __.``ISerializable Class`` () = testEquals <| SerializableClass(42, "fortyTwo")
        [<Test>] member __.``IFsCoreSerializable Class`` () = testEquals <| FsCoreSerializableClass(42, "fortyTwo")

        [<Test>]
        member __.``NonSerializable Type`` () =
            let fs = new System.IO.FileStream(System.IO.Path.GetTempFileName(), System.IO.FileMode.Open)
            shouldFailWith<SerializationException>(fun () -> self.TestSerializer fs |> ignore)
        
        [<Test>] 
        member __.``Cyclic Array`` () = 
            let cyclicArray : obj [] = 
                let array = Array.zeroCreate<obj> 10
                for i = 0 to array.Length - 1 do
                    array.[i] <- Some (array, i) :> obj
                array

            test cyclicArray

        [<Test>] 
        member __.``Lists`` () = 
                    testEquals []
                    testEquals [1..100] 
                    testEquals ([1..100] |> List.map (fun i -> i, string i))

        [<Test>]
        member __.``Arrays`` () =
                    testEquals [||]
                    testEquals ([|1.0 .. 100.0|] |> Array.map (fun i -> (i, i)))
                    testEquals ([|1L .. 100L|] |> Array.map (fun i -> TimeSpan i))

        [<Test>]
        member __.``Exceptions`` () =
                    test <| Exception("outer", Exception("inner"))
                    test <| ArgumentException()

    
        [<Test>]
        member __.``FSharpException`` () = test <| FsharpException(42, "fortyTwo")

        [<Test>]
        member __.``Generic Dictionary`` () =
            let d = [1..100] |> Seq.map (fun i -> i, string i) |> dict
            let d' = testLoop d
            Seq.toArray d |> should equal (Seq.toArray d')

        [<Test>]
        member __.``FSharpMap`` () =
            let m = [1..100] |> Seq.map (fun i -> i, string i) |> Map.ofSeq
            testEquals m

        [<Test>]
        member __.``FSharpSet`` () =
            let s = [1..100] |> Seq.map (fun i -> i, string i) |> set
            testEquals s

        [<Test>]
        member __.``FSharpRef`` () = testEquals (ref 42)

        [<Test>]
        member __.``Complex FSharp Type`` () =
            let x = ((1, [Some(A, int2Peano 10); None], [|1.0 .. 100.0|]), [(1,2,3) ; (1,1,1)], set [1..100])
            let y = testLoop x
            x = y |> should equal true

        [<Test>]
        member __.``Simple Delegate`` () =
            let d = System.Func<int, int>(fun x -> x + 1)
            
            (testLoop d).Invoke 41 |> should equal 42

        [<Test>]
        member __.``Multicast Delegate`` () =
            DeleCounter.Value <- 0
            let f n = new TestDelegate(fun () -> DeleCounter.Value <- DeleCounter.Value + n) :> Delegate
            let g = Delegate.Combine [| f 1 ; f 2 |]
            let h = Delegate.Combine [| g ; f 3 |]
            (testLoop h).DynamicInvoke [| |] |> ignore
            DeleCounter.Value |> should equal 6

        [<Test>]
        member __.``Lazy Values`` () =
            let v = lazy(if true then 42 else 0)
            (testLoop v).Value |> should equal 42

        [<Test>]
        member __.``FSharp Function`` () =
            let f x = x + 1

            (testLoop f) 41 |> should equal 42

        [<Test>]
        member __.``FSharp Curried Function`` () =
            let f x y = x + y

            (testLoop (f 41)) 1 |> should equal 42


        [<Test>]
        member __.``FSharp Closure`` () =
            let f () =
                let x = System.Random().Next()
                fun () -> x + 1

            let g = f ()

            (testLoop g) () |> should equal (g ())


        [<Test>]
        member __.``FSharp Tree`` () =
            testEquals (mkTree 5)


        [<Test>]
        member __.``FSharp Rec`` () =
            let rec f = Rec (fun g -> let (Rec f) = f in f g)
            test f

        [<Test>]
        member __.``FSharp Builders`` () =
            let infty =
                seq {
                    let i = ref 0
                    while true do
                        incr i
                        yield !i
                }

            testLoop infty |> Seq.take 5 |> Seq.toArray |> should equal [|1..5|]

        [<Test>]
        member __.``FSharp Quotations`` () =
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

            test quot

        [<Test>]
        member __.``IFormatterFactory test`` () =
            (0,"0",()) |> testLoop |> should equal (42,"42",()) 


        [<Test>]
        member __.``GenericFormatterFactory test`` () =
            let x = testLoop (GenericType<int>(42))
            x.Value |> should equal 0

        [<Test>]
        member __.``Test Massively Auto-Generated Objects`` () =
            // generate serializable objects that reside in mscorlib and FSharp.Core
            let inputData = 
                Array.concat
                    [
                        Utils.generateSerializableObjects typeof<int>.Assembly
                        Utils.generateSerializableObjects typeof<_ option>.Assembly
                        Utils.generateSerializableObjects <| Assembly.GetExecutingAssembly()
                    ]

            let test (t : Type, x : obj) =
                try test x ; None
                with e -> Some(e,t)

            let failedResults = inputData |> Array.choose test

            if failedResults.Length > 0 then
                let errors = 
                    failedResults
                    |> Array.map (fun (e,t) -> sprintf "%s failed with error: %s" t.FullName e.Message) 
                    |> String.concat "\r\n"

                failwithf "The following types failed to serialize: %s" errors
            


    [<TestFixture>]
    type ``In-memory Correctness Tests`` () =
        inherit ``Serializer Correctness Tests`` ()

        let fsc = new FsCoreSerializer() :> ISerializer

        override __.TestSerializer (x : 'T) = fsc.Serialize x
        override __.TestDeserializer (bytes : byte []) = fsc.Deserialize bytes
        override __.TestLoop(x : 'T) = fsc.Serialize x |> fsc.Deserialize :?> 'T

        override __.Init () = ()
        override __.Fini () = ()