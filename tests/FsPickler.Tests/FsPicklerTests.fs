namespace Nessos.FsPickler.Tests

    open System
    open System.IO
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
    [<TestFixture("FsPickler.Xml")>]
    [<TestFixture("FsPickler.Json")>]
    [<TestFixture("FsPickler.Json-headerless")>]
    type ``FsPickler Tests`` (picklerName : string) as self =

        let pickler =
            match picklerName with
            | "FsPickler.Binary" -> FsPickler.CreateBinary() :> BasePickler
            | "FsPickler.Xml" -> FsPickler.CreateXml(indent = true) :> BasePickler
            | "FsPickler.Json" -> FsPickler.CreateJson(indent = true) :> BasePickler
            | "FsPickler.Json-headerless" -> 
                let jsp = FsPickler.CreateJson(omitHeader = true)
                jsp.UseCustomTopLevelSequenceSeparator <- true
                jsp.SequenceSeparator <- Environment.NewLine
                jsp :> BasePickler

            | _ -> invalidArg "name" <| sprintf "unexpected pickler format '%s'." picklerName

        let _ = Arb.register<FsPicklerGenerators> ()

        let testRoundtrip x = self.TestRoundtrip x
        let testEquals x = self.TestRoundtrip x |> should equal x
        let testReflected x =
            let y = self.TestRoundtrip x
            if obj.ReferenceEquals(x, null) then
                y |> should equal x
            else
                y.GetType() |> should equal (x.GetType())
                y.ToString() |> should equal (x.ToString())

        member __.Pickler = pickler

        abstract TestPickle : Pickler<'T> -> 'T -> byte []
        default __.TestPickle p (t : 'T) = pickler.Pickle(p,t)

        abstract TestUnPickle<'T> : Pickler<'T> -> byte [] -> 'T
        default __.TestUnPickle<'T> p (pickle : byte []) : 'T = pickler.UnPickle<'T>(p, pickle)

        abstract TestSequenceRoundtrip<'T when 'T : equality> : seq<'T> -> unit
        default __.TestSequenceRoundtrip (xs : seq<'T>) =
            use m = new MemoryStream()
            let length = pickler.SerializeSequence(m, xs, leaveOpen = true)
            m.Position <- 0L
            let xs' = pickler.DeserializeSequence<'T>(m)
            use enum = xs'.GetEnumerator()

            for i,x in xs |> Seq.mapi (fun i x -> (i,x)) do
                if enum.MoveNext() then 
                    if enum.Current = x then ()
                    else
                        failwithf "element %d: expected '%A' but was '%A'." i x enum.Current
                else
                    failwithf "sequence terminated early at %d." i
                    

        abstract TestRoundtrip : 'T -> 'T
        default __.TestRoundtrip (t : 'T) = 
            let pickle = pickler.Pickle t
            pickler.UnPickle pickle

        //
        //  Primitive Serialization tests
        //

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: bool`` () = testEquals false ; testEquals true

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: byte`` () = Check.QuickThrowOnFail<byte> testEquals

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: sbyte`` () = Check.QuickThrowOnFail<sbyte> testEquals

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: int16`` () = Check.QuickThrowOnFail<int16> testEquals

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: int32`` () = Check.QuickThrowOnFail<int32> testEquals

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: int64`` () = Check.QuickThrowOnFail<int64> testEquals

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: uint16`` () = Check.QuickThrowOnFail<uint16> testEquals

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: uint32`` () = Check.QuickThrowOnFail<uint32> testEquals

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: uint64`` () = Check.QuickThrowOnFail<uint64> testEquals

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: single`` () = Check.QuickThrowOnFail<single> testEquals

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: double`` () = Check.QuickThrowOnFail<double> testEquals

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: decimal`` () = Check.QuickThrowOnFail<decimal> testEquals

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: char`` () = Check.QuickThrowOnFail<char> testEquals

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: string`` () = testEquals (null : string) ; Check.QuickThrowOnFail<string> testEquals

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: date`` () = 
            if runsOnMono then
                // Mono bug: https://bugzilla.xamarin.com/show_bug.cgi?id=20457
                Check.QuickThrowOnFail<DateTime> (fun d -> testRoundtrip d |> ignore)
            else
                Check.QuickThrowOnFail<DateTime> testEquals

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: System.TimeSpan`` () = Check.QuickThrowOnFail<TimeSpan> testEquals

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: System.Guid`` () = Check.QuickThrowOnFail<Guid> testEquals

        [<Test; Category("Primitives")>]
        member __.``1. Primitive: bigint`` () = 
            if runsOnMono then
                // Mono bug: https://bugzilla.xamarin.com/show_bug.cgi?id=20456
                Check.QuickThrowOnFail<bigint> (fun i -> let j = testRoundtrip i in i.ToString() = j.ToString())
            else
                Check.QuickThrowOnFail<bigint> testEquals

        [<Test; Category("Bytes")>]
        member __.``1. Primitive: byte []`` () = testEquals (null : byte []) ; Check.QuickThrowOnFail<byte []> testEquals

        //
        //  Reflection types
        //

        [<Test; Category("Reflection types")>]
        member __.``2. Reflection: Type`` () = 
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
        member __.``2. Reflection: MemberInfo`` () =
            [| 
                typeof<obj> ; typeof<exn> ; typeof<int> ; typeof<string> ; typeof<bool> ; typeof<int option> ; 
                typeof<Quotations.Expr> ; typeof<System.Collections.Generic.Dictionary<int,string>> ; 
                typeof<int list> ; typedefof<_ list> ; typedefof<_ ref> ; typeof<OverLoaded> ;
                Pickler.auto<int * string>.GetType()
            |]
            |> Array.collect(fun t -> t.GetMembers(allFlags ||| BindingFlags.FlattenHierarchy))
            |> Array.iter testEquals

        [<Test; Category("Reflection types")>]
        member __.``2. Reflection: Assembly`` () =
            System.AppDomain.CurrentDomain.GetAssemblies()
            |> Array.iter testEquals

        [<Test; Category("Reflection types")>]
        member __.``2. Reflection: AssemblyName`` () =
            AssemblyName() |> testReflected

            System.AppDomain.CurrentDomain.GetAssemblies()
            |> Array.map (fun a -> a.GetName())
            |> Array.iter testReflected
            
        //
        // Arrays
        //

        member __.CheckArray<'T> () =
            Check.QuickThrowOnFail<'T []> testEquals
            Check.QuickThrowOnFail<'T [,]> testEquals
            Check.QuickThrowOnFail<'T [,,]> testEquals
            Check.QuickThrowOnFail<'T [,,,]> testEquals


        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: bool`` () = __.CheckArray<bool> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: byte`` () = __.CheckArray<byte> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: sbyte`` () = __.CheckArray<sbyte> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: int16`` () = __.CheckArray<int16> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: int32`` () = __.CheckArray<int32> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: int64`` () = __.CheckArray<int64> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: uint16`` () = __.CheckArray<uint16> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: uint32`` () = __.CheckArray<uint32> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: uint64`` () = __.CheckArray<uint64> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: string`` () = __.CheckArray<string> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: single`` () = __.CheckArray<single> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: double`` () = __.CheckArray<double> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: decimal`` () = __.CheckArray<decimal> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: System.Guid`` () = __.CheckArray<Guid> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: System.DateTime`` () = 
            if runsOnMono then
                // Mono Bug: https://bugzilla.xamarin.com/show_bug.cgi?id=20457
                Check.QuickThrowOnFail<DateTime []> testReflected
                Check.QuickThrowOnFail<DateTime [,]> testReflected
                Check.QuickThrowOnFail<DateTime [,,]> testReflected
                Check.QuickThrowOnFail<DateTime [,,,]> testReflected
            else
                __.CheckArray<DateTime> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: System.TimeSpan`` () = __.CheckArray<TimeSpan> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: byte []`` () = __.CheckArray<byte []> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: int * string`` () = __.CheckArray<int * string> ()

        [<Test; Category("Generic BCL Types")>]
        member __.``3. Array: string * (int * decimal)`` () = __.CheckArray<int * string> ()


        //
        //  BCL types
        //

        // Tuples

        [<Test; Category("Generic BCL Types")>]
        member __.``4. BCL: tuple simple`` () =
            Check.QuickThrowOnFail<Tuple<string>> testEquals
            Check.QuickThrowOnFail<string * byte> testEquals
            Check.QuickThrowOnFail<string * byte * TimeSpan> testEquals
            Check.QuickThrowOnFail<string * byte * TimeSpan * Guid> testEquals
            Check.QuickThrowOnFail<string * byte * TimeSpan * Guid * int> testEquals
            Check.QuickThrowOnFail<string * byte * TimeSpan * Guid * int * uint64> testEquals
            Check.QuickThrowOnFail<string * byte * TimeSpan * Guid * int * uint64 * decimal> testEquals
            Check.QuickThrowOnFail<string * byte * TimeSpan * Guid * int * uint64 * decimal * int> testEquals
            Check.QuickThrowOnFail<string * byte * TimeSpan * Guid * int * uint64 * decimal * int * int> testEquals

        [<Test; Category("Generic BCL Types")>]
        member __.``4. BCL: tuple nested`` () =
            Check.QuickThrowOnFail<int * (string * decimal)> testEquals
            Check.QuickThrowOnFail<(int * (bool * string)) * (string * int16)> testEquals
            Check.QuickThrowOnFail<(int * (bool * (sbyte * string * uint32) * (string * string)))> testEquals


        // exceptions

        // should properly serialize stacktrace
        member __.TestException(e : 'exn) = e |> addStackTrace |> testReflected

        [<Test; Category("Generic BCL Types")>]
        member __.``4. BCL: System.Exception`` () = __.TestException <| new Exception("exception message")

        [<Test; Category("Generic BCL Types")>]
        member __.``4. BCL: System.Exception with inner exception`` () =
            let inner = new Exception("inner") |> addStackTrace
            __.TestException <| new Exception("outer", inner)

        [<Test; Category("Generic BCL Types")>]
        member __.``4. BCL: misc exceptions`` () =
            __.TestException <| new InvalidOperationException()
            __.TestException <| new AccessViolationException()
            __.TestException <| new InvalidTimeZoneException()
            __.TestException <| new System.IO.EndOfStreamException()
            __.TestException <| new System.IO.InvalidDataException()
            __.TestException <| new System.IO.FileNotFoundException()


        // collections

        [<Test; Category("Generic BCL Types")>]
        member __.``4. BCL: System.Collections.Generic.Dictionary`` () =
            let testDictionary (data : seq<'K * 'V>) =
                let data = data |> Seq.distinctBy fst |> Seq.toList
                let d = dict data
                let d' = testRoundtrip d
                let data' = d' |> Seq.map (function KeyValue(k,v) -> k,v) |> Seq.toList
                data' |> should equal data

            Check.QuickThrowOnFail<seq<int64 * (string * float)>> testDictionary
            Check.QuickThrowOnFail<seq<(int64 * string) * string>> testDictionary


        [<Test; Category("Generic BCL Types")>]
        member __.``4. BCL: System.Collections.Generic.HashSet`` () =
            let testSet (data : seq<'T>) =
                let data = data |> Seq.distinct |> Seq.toList
                let d = new HashSet<'T>(data)
                let data' = testRoundtrip d |> Seq.toList
                data' |> should equal data

            Check.QuickThrowOnFail<seq<int64>> testSet
            Check.QuickThrowOnFail<seq<string>> testSet
            Check.QuickThrowOnFail<seq<int * string>> testSet


        [<Test; Category("Generic BCL Types")>]
        member __.``4. BCL: System.Collections.Generic.Stack`` () =
            let testStack (data : 'T list) =
                let d = new Stack<'T>(data)
                let data' = testRoundtrip d |> Seq.toList |> List.rev
                data' |> should equal data

            Check.QuickThrowOnFail<int64 list> testStack
            Check.QuickThrowOnFail<string list> testStack
            Check.QuickThrowOnFail<(int * string) list> testStack

        // Delegates

        [<Test; Category("Generic BCL Types")>]
        member __.``4. BCL: delegate simple`` () =
            let d = System.Func<int, int>(fun x -> x + 1)
            
            (testRoundtrip d).Invoke 41 |> should equal 42

        [<Test; Category("Generic BCL Types")>]
        member __.``4. BCL: delegate multicast`` () =
            DeleCounter.Value <- 0
            let f n = new TestDelegate(fun () -> DeleCounter.Value <- DeleCounter.Value + n) :> Delegate
            let g = Delegate.Combine [| f 1 ; f 2 |]
            let h = Delegate.Combine [| g ; f 3 |]
            (testRoundtrip h).DynamicInvoke [| |] |> ignore
            DeleCounter.Value |> should equal 6

        [<Test; Category("Generic BCL Types")>]
        member __.``4. BCL: lazy int`` () =
            let v = lazy(if true then 42 else 0)
            (testRoundtrip v).Value |> should equal 42

        [<Test; Category("Generic BCL Types")>]
        member __.``4. BCL: lazy tuple`` () =
            let f () = (12, "value")
            let v = lazy(f ())
            (testRoundtrip v).Value |> should equal (f ())

        [<Test; Category("Generic BCL Types")>]
        member __.``4. BCL: nullable int`` () =
            let x = Nullable<int>(42)
            let y = Nullable<int> ()
            testEquals x ; testEquals y

        [<Test; Category("Generic BCL Types")>]
        member __.``4. BCL: nullable decimal`` () =
            let x = Nullable<decimal>(42.01M)
            let y = Nullable<decimal> ()
            testEquals x ; testEquals y

        //
        // Object serialization
        //

        [<Test; Category("FsPickler Generic tests")>]
        member __.``5. Object: boxed values`` () =
            testEquals <| box 42
            testEquals <| box 1.1
            testEquals <| box "test"
            testEquals <| box DateTime.Now
            testEquals <| box (1,1,1)

        [<Test; Category("FsPickler Generic tests")>]
        member __.``5. Object: boxed array`` () =
            testEquals <| [| box 1 ; box "test" ; box true ; box 16I ; box (1,"1") ; box DateTime.Now |]

        [<Test; Category("FsPickler Generic tests")>]
        member __.``5. Object: should correctly deserialize boxed arrays`` () =
            let boxed = { A = [| 1 .. 10|] ; B = [| 1 .. 4 |] }
            testEquals boxed

        [<Test; Category("FsPickler Generic tests")>]
        member __.``5. Object: avoid recursion in memberInfo values`` () =
            let m = typeof<OverLoaded>.GetMethods() |> Seq.find(fun x -> x.Name = "A" && x.GetParameters().Length = 1)
            let m0 = m.MakeGenericMethod(typeof<int>)
            testEquals m0

        [<Test; Category("FsPickler Generic tests")>]
        member __.``5. Object: should fail at non-serializable type`` () =
            let m = box <| new System.IO.MemoryStream()
            shouldFailwith<NonSerializableTypeException>(fun () -> self.TestPickle Pickler.auto m |> ignore)

        [<Test; Category("FsPickler Generic tests")>]
        member __.``5. Object: cyclic object`` () = testReflected <| CyclicClass()

        [<Test; Category("FsPickler Generic tests")>]
        member __.``5. Object: cyclic array`` () = 
            let cyclicArray : obj [] = 
                let array = Array.zeroCreate<obj> 10
                for i = 0 to array.Length - 1 do
                    array.[i] <- Some (array, i) :> obj
                array

            testReflected cyclicArray

        [<Test; Category("FsPickler Generic tests") ; Repeat(5)>]
        member __.``5. Object: random object graph`` () =
            let g = createRandomGraph 0.7 20

            let g' = testRoundtrip g

            areEqualGraphs g g' |> should equal true

        [<Test; Category("FsPickler Generic tests")>]
        member __.``5. Object: int sequence`` () =
            Check.QuickThrowOnFail<seq<int>> __.TestSequenceRoundtrip

        [<Test; Category("FsPickler Generic tests")>]
        member __.``5. Object: string sequence`` () =
            Check.QuickThrowOnFail<seq<string>> __.TestSequenceRoundtrip

        [<Test; Category("FsPickler Generic tests")>]
        member __.``5. Object: pair sequence`` () =
            Check.QuickThrowOnFail<seq<int * string>> __.TestSequenceRoundtrip

        [<Test; Category("FsPickler Generic tests")>]
        member __.``5. Object: large int sequence`` () =
            __.TestSequenceRoundtrip [| 1 .. 1000000 |]

        [<Test; Category("FsPickler Generic tests")>]
        member __.``5. Object: large pair sequence`` () =
            let pairs = seq { for i in 1 .. 1000000 -> string i,i }
            __.TestSequenceRoundtrip pairs

        [<Test; Category("FsPickler Generic tests")>]
        member __.``5. Object: record sequence`` () =
            let records = seq { for i in 1 .. 10000 -> { Int = i ; String = string i ; Tuple = (i, "const") } }
            __.TestSequenceRoundtrip records

        [<Test; Category("FsPickler Generic tests")>]
        member __.``5. Object: sequence pickler`` () =
            let seqPickler = Pickler.seq Pickler.int

            let state = ref 0
            let sequence =
                seq {
                    while !state < 100 do
                        yield !state
                        incr state
                }
            
            let sequence' = sequence |> self.TestPickle seqPickler |> self.TestUnPickle seqPickler

            // check that sequence has been evaluated
            !state |> should equal 100

            sequence' |> Seq.length |> should equal 100


        //
        //  Custom types
        //


        [<Test ; Category("Custom types")>] 
        member __.``6. Custom: simple class`` () = testEquals <| SimpleClass(42, "fortyTwo")

        [<Test ; Category("Custom types")>] 
        member __.``6. Custom: generic class`` () = testEquals <| new GenericClass<string * int>("fortyTwo", 42)

        [<Test ; Category("Custom types")>] 
        member __.``6. Custom: recursive class`` () = testEquals <| RecursiveClass(Some (RecursiveClass(None)))

        [<Test ; Category("Custom types")>] 
        member __.``6. Custom: Simple ISerializable class`` () = testEquals <| SimpleISerializableClass(42, "fortyTwo")

        [<Test ; Category("Custom types")>] 
        member __.``6. Custom: Generic ISerializable class`` () = testEquals <| GenericISerializableClass<int * string>(42, "fortyTwo", (42, "fortyTwo"))

        [<Test ; Category("Custom types")>] 
        member __.``6. Custom: struct`` () =
            let s = new StructType(42, "foobar") |> testRoundtrip
            s.X |> should equal 42
            s.Y |> should equal "foobar"

        [<Test ; Category("Custom types")>] 
        member __.``6. Custom: pickler factory class`` () = 
            let x = ClassWithPicklerFactory(0) |> testRoundtrip
            x.Value |> should equal 42

        [<Test ; Category("Custom types")>] 
        member __.``6. Custom: pickler factory with recursive bindings`` () =
            let x = new ClassWithCombinators(12, None)
            let y = new ClassWithCombinators(0, Some x)

            let z = testRoundtrip y

            z.Value |> snd |> Option.map (fun x -> x.Value) |> Option.get |> fst |> should equal 42





        //
        //  FSharp Tests
        //

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: option`` () =
            Check.QuickThrowOnFail<int option> testEquals
            Check.QuickThrowOnFail<string option> testEquals
            Check.QuickThrowOnFail<(int * string) option> testEquals
            Check.QuickThrowOnFail<decimal option option> testEquals

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: ref`` () =
            Check.QuickThrowOnFail<int ref> testEquals
            Check.QuickThrowOnFail<string ref> testEquals

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: choice`` () =
            Check.QuickThrowOnFail<Choice<int, string>> testEquals
            Check.QuickThrowOnFail<Choice<float, decimal, string * int>> testEquals

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: list`` () =
            Check.QuickThrowOnFail<int list> testEquals
            Check.QuickThrowOnFail<bool list> testEquals
            Check.QuickThrowOnFail<float list> testEquals
            Check.QuickThrowOnFail<decimal list> testEquals
            Check.QuickThrowOnFail<string list> testEquals
            Check.QuickThrowOnFail<(string * int) list> testEquals
            Check.QuickThrowOnFail<bool option list> testEquals
            Check.QuickThrowOnFail<byte [] list> testEquals
            
        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: simple union`` () =
            Check.QuickThrowOnFail<SimpleDU> testEquals

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: recursive union`` () =
            testEquals (int2Peano 42)
            Check.QuickThrowOnFail<Peano> testEquals

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: tree`` () = 
            testEquals (mkTree 5)
            Check.QuickThrowOnFail<BinTree<string * bool>> testEquals

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: mutual recursive union`` () =
            testEquals <| nTree 5
            Check.QuickThrowOnFail<Tree<bool option>> testEquals
            Check.QuickThrowOnFail<Forest<int * string>> testEquals

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: combinator-based recursive union`` () =
            let pp = 
                Pickler.fix(fun peanoP -> 
                    peanoP 
                    |> Pickler.option 
                    |> Pickler.wrap 
                        (function None -> Zero | Some p -> Succ p) 
                        (function Zero -> None | Succ p -> Some p))

            let p = int2Peano 100

            p |> self.TestPickle pp |> self.TestUnPickle pp |> should equal p

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: combinator-based mutual recursive union`` () =
            let tp,_ = getTreeForestPicklers Pickler.int
            let t = nTree 6

            t |> self.TestPickle tp |> self.TestUnPickle tp |> should equal t

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: record`` () = 
            Check.QuickThrowOnFail<Record> testEquals

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: cyclic record`` () = 
            let rec f = { Rec = f }
            testReflected f

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: core types`` () =
            Check.QuickThrowOnFail<int * (SimpleDU * Peano) option * (int * string * float) list * Set<int>> testEquals

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: exception`` () =
            let e = FSharpException(42, "fortyTwo") :?> FSharpException |> addStackTrace
            testReflected e
            let e' = testRoundtrip e
            // test exception field serialization
            e'.Data0 |> should equal e.Data0
            e'.Data1 |> should equal e.Data1

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: map`` () =
            Check.QuickThrowOnFail<Map<int,string>> testEquals
            Check.QuickThrowOnFail<Map<int * int,string>> testEquals

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: set`` () =
            Check.QuickThrowOnFail<Set<float>> testEquals
            Check.QuickThrowOnFail<Set<int * string>> testEquals


        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: function`` () =
            let f x = x + 1

            (testRoundtrip f) 41 |> should equal 42

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: function curried`` () =
            let f x y = x + y

            (testRoundtrip (f 41)) 1 |> should equal 42

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: function closure`` () =
            let f () =
                let x = System.Random().Next()
                fun () -> x + 1

            let g = f ()

            (testRoundtrip g) () |> should equal (g ())

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: expression builders`` () =
            let i = ref 0
            let infty =
                seq {
                    while true do
                        incr i
                        yield !i
                }

            testRoundtrip infty |> Seq.take 5 |> Seq.toArray |> should equal [|1..5|]

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: quotation simple`` () =
            testReflected <@@ 1 + 1 @@>
            testReflected <@ if true then failwith "error" else 1 @>

        [<Test; Category("FSharp type tests")>]
        member __.``7. FSharp: quotation large`` () =
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

            testReflected quot


        //
        //  Stress tests
        //

        member t.TestTypeMismatch<'In, 'Out> (v : 'In) = 
            fun () ->
                t.TestPickle<'In> Pickler.auto<'In> v 
                |> t.TestUnPickle<'Out> Pickler.auto<'Out>
                |> ignore

            |> shouldFailwith<InvalidPickleTypeException>

        [<Test; Category("Stress tests")>]
        member t.``8. Stress test: deserialization type mismatch`` () =
            match pickler with
            | :? JsonPickler as jsp when jsp.OmitHeader -> ()
            | _ ->
                t.TestTypeMismatch<int, string> 42
                t.TestTypeMismatch<string, int> "forty-two"
                t.TestTypeMismatch<obj, int>(obj())
                t.TestTypeMismatch<int * int64, int * int> (1,1L)

        member t.TestDeserializeInvalidData<'T> (bytes : byte []) =
            try
                use m = new MemoryStream(bytes)
                let t' = pickler.Deserialize<'T>(m)
                ()
            with
            | :? InvalidPickleException
            | :? InvalidPickleTypeException -> ()

        [<Test; Category("Stress tests")>]
        member t.``8. Stress test: arbitrary data deserialization`` () =
            match pickler with
            | :? JsonPickler as jsp when jsp.OmitHeader -> ()
            | _ ->
                Check.QuickThrowOnFail (fun bs -> t.TestDeserializeInvalidData<int> bs)
                Check.QuickThrowOnFail (fun bs -> t.TestDeserializeInvalidData<string> bs)
                Check.QuickThrowOnFail (fun bs -> t.TestDeserializeInvalidData<byte []> bs)
                Check.QuickThrowOnFail (fun bs -> t.TestDeserializeInvalidData<int * string option> bs)

        [<Test; Category("Stress tests")>]
        member __.``8. Stress test: massively auto-generated objects`` () =
            // generate serializable objects that reside in mscorlib and FSharp.Core
            let inputData = 
                Seq.concat
                    [
                        generateSerializableObjects typeof<int>.Assembly
                        generateSerializableObjects typeof<_ option>.Assembly
                        generateSerializableObjects <| Assembly.GetExecutingAssembly()
                    ]

            let test (t : Type, x : obj) =
                try testRoundtrip x |> ignore ; None
                with 
                | e ->
                    printfn "Serializing '%O' failed with error: %O" t e
                    Some e

            let results = inputData |> Seq.map test |> Seq.toArray
            let failedResults = results |> Seq.choose id |> Seq.length

            if failedResults > 10 then
                let msg = sprintf "Too many random object serialization failures (%d out of %d)." failedResults results.Length
                raise <| new AssertionException(msg)
            else
                printfn "Failed Serializations: %d out of %d." failedResults results.Length


