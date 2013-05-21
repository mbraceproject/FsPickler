namespace FsCoreSerializer.Tests

    #nowarn "346"

    open System
    open System.Reflection
    open System.Runtime.Serialization

    open FsUnit
    open FsCoreSerializer

    open NUnit.Framework

    [<AutoOpen>]
    module Utils =
        
        let fsc = new FsCoreSerializer() :> ISerializer

        let test (x : 'a) = fsc.Serialize x |> fsc.Deserialize :?> 'a

        let testEquals (x : 'a) =
            test x |> should equal x

        let testReflectedType (o : obj) =
            (test o).GetType() |> should equal (o.GetType())

        let testMembers (t : Type) =
            let members = t.GetMembers(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static)
            for m in members do test m |> should equal m


        // Test typedefs

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


    [<TestFixture>]
    type FsCoreSerializerTests() =

        [<Test>] member __.``Unit`` () = testEquals ()
        [<Test>] member __.``Boolean`` () = testEquals false
        [<Test>] member __.``Integer`` () = testEquals 1
        [<Test>] member __.``String`` () = testEquals "lorem ipsum dolor"
        [<Test>] member __.``Float`` () = testEquals 3.1415926
        [<Test>] member __.``Guid`` () = testEquals <| Guid.NewGuid()
        [<Test>] member __.``Decimal`` () = testEquals Decimal.MaxValue
        [<Test>] member __.``NativePtr`` () = testEquals 42n
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
        [<Test>] member __.``Cyclic Object`` () = testReflectedType <| CyclicClass()
        [<Test>] member __.``ISerializable Class`` () = testEquals <| SerializableClass(42, "fortyTwo")
        [<Test>] member __.``IFsCoreSerializable Class`` () = testEquals <| FsCoreSerializableClass(42, "fortyTwo")

        [<Test>]
        member __.``NonSerializable Type`` () =
            let o = new System.IO.FileStream(System.IO.Path.GetTempFileName(), System.IO.FileMode.Open)
            shouldFailWith<SerializationException>(fun () -> test o |> ignore)
        
        [<Test>] 
        member __.``Cyclic Array`` () = 
            let cyclicArray : obj [] = 
                let array = Array.zeroCreate<obj> 10
                for i = 0 to array.Length - 1 do
                    array.[i] <- Some (array, i) :> obj
                array

            testReflectedType cyclicArray

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
                    testReflectedType <| Exception("outer", Exception("inner"))
                    testReflectedType <| ArgumentException()

    
        [<Test>]
        member __.``FSharpException`` () = testReflectedType <| FsharpException(42, "fortyTwo")

        [<Test>]
        member __.``Generic Dictionary`` () =
            let d = [1..100] |> Seq.map (fun i -> i, string i) |> dict
            let d' = test d
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
            let y = test x
            x = y |> should equal true

        [<Test>]
        member __.``Delegates`` () =
            let d = System.Func<int, int>(fun x -> x + 1)
            
            (test d).Invoke 41 |> should equal 42

        [<Test>]
        member __.``FSharp Function`` () =
            let f x = x + 1

            (test f) 41 |> should equal 42

        [<Test>]
        member __.``FSharp Curried Function`` () =
            let f x y = x + y

            (test (f 41)) 1 |> should equal 42


        [<Test>]
        member __.``FSharp Closure`` () =
            let f () =
                let x = System.Random().Next()
                fun () -> x + 1

            let g = f ()

            (test g) () |> should equal (g ())


        [<Test>]
        member __.``FSharp Tree`` () =
            testEquals (mkTree 5)


        [<Test>]
        member __.``FSharp Rec`` () =
            let rec f = Rec (fun g -> let (Rec f) = f in f g)
            testReflectedType f

        [<Test>]
        member __.``FSharp Builders`` () =
            let infty =
                seq {
                    let i = ref 0
                    while true do
                        incr i
                        yield !i
                }

            test infty |> Seq.take 5 |> Seq.toArray |> should equal [|1..5|]

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

            testReflectedType quot