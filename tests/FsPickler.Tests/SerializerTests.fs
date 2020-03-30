namespace MBrace.FsPickler.Tests

open System
open System.IO
open System.Linq
open System.Collections
open System.Collections.Generic
open System.Reflection
open System.Threading.Tasks

open MBrace.FsPickler
open MBrace.FsPickler.Json
open MBrace.FsPickler.Combinators
open MBrace.FsPickler.Hashing

open NUnit.Framework
open FsUnit
open FsCheck

#nowarn "8989" // PicklerCache size warnings

type ISerializerFixture =
    /// Specifies whether the fixture uses remoting for running the tests
    abstract IsRemotedFixture : bool
    /// Local serializer instance
    abstract Serializer : FsPicklerSerializer
    /// Creates a fresh serializer instance
    abstract NewSerializer : unit -> FsPicklerSerializer
    /// Pickle a value, potentially in a remote AppDomain
    abstract Pickle<'T> : value:'T -> byte[]
    /// Unpickle a value
    abstract UnPickle<'T> : pickle:byte[] -> 'T
    /// Runs an arbitrary serialization operation given a serializer input,
    /// potentially in a remote AppDomain
    abstract PickleF : pickler:(FsPicklerSerializer -> byte[]) -> byte[]

[<AbstractClass>]
type SerializationTests (fixture : ISerializerFixture) =
    static let _ = Arb.register<FsPicklerGenerators> ()

    let testRoundtrip (x : 'T) =
        let bytes = fixture.Pickle x
        fixture.UnPickle<'T>(bytes)

    let testEquals x =
        let y = testRoundtrip x
        y |> should equal x

    let testReflected x =
        let y = testRoundtrip x
        if obj.ReferenceEquals(x, null) then
            y |> should equal x
        else
            y.GetType() |> should equal (x.GetType())
            y.ToString() |> should equal (x.ToString())

    //
    //  Primitive Serialization tests
    //

    [<Test; Category("Primitives")>]
    member __.``Primitive: bool`` () = testEquals false ; testEquals true

    [<Test; Category("Primitives")>]
    member __.``Primitive: byte`` () = Check.QuickThrowOnFail<byte> testEquals

    [<Test; Category("Primitives")>]
    member __.``Primitive: sbyte`` () = Check.QuickThrowOnFail<sbyte> testEquals

    [<Test; Category("Primitives")>]
    member __.``Primitive: int16`` () = Check.QuickThrowOnFail<int16> testEquals

    [<Test; Category("Primitives")>]
    member __.``Primitive: int32`` () = Check.QuickThrowOnFail<int32> testEquals

    [<Test; Category("Primitives")>]
    member __.``Primitive: int64`` () = Check.QuickThrowOnFail<int64> testEquals

    [<Test; Category("Primitives")>]
    member __.``Primitive: uint16`` () = Check.QuickThrowOnFail<uint16> testEquals

    [<Test; Category("Primitives")>]
    member __.``Primitive: uint32`` () = Check.QuickThrowOnFail<uint32> testEquals

    [<Test; Category("Primitives")>]
    member __.``Primitive: uint64`` () = Check.QuickThrowOnFail<uint64> testEquals

    [<Test; Category("Primitives")>]
    member __.``Primitive: single`` () = Check.QuickThrowOnFail<single> testEquals

    [<Test; Category("Primitives")>]
    member __.``Primitive: double`` () = Check.QuickThrowOnFail<double> testEquals

    [<Test; Category("Primitives")>]
    member __.``Primitive: decimal`` () = Check.QuickThrowOnFail<decimal> testEquals

    [<Test; Category("Primitives")>]
    member __.``Primitive: char`` () = Check.QuickThrowOnFail<char> testEquals

    [<Test; Category("Primitives")>]
    member __.``Primitive: string`` () = Check.QuickThrowOnFail<string> testEquals

    [<Test; Category("Primitives")>]
    member __.``Primitive: DateTime`` () =
        Check.QuickThrowOnFail<DateTime * DateTimeKind> (fun (d : DateTime, k : DateTimeKind) ->
            let d =
                // normalize for DST adjustment logic
                if k = DateTimeKind.Local then DateTimeOffset(d).LocalDateTime
                else DateTime(d.Ticks, k)

            let d' = testRoundtrip d
            d' |> should equal d
            d'.Kind |> should equal d.Kind)

    [<Test; Category("Primitives")>]
    member __.``Primitive: DateTimeOffset`` () =
        Check.QuickThrowOnFail<DateTimeOffset> testEquals

        Check.QuickThrowOnFail<DateTime * TimeZoneInfo>(fun (d:DateTime,tz:TimeZoneInfo) ->
            let d = new DateTime(d.Ticks, DateTimeKind.Local)
            let dto1 = new DateTimeOffset(d)
            let dto2 =
                let utcTime = d.ToUniversalTime()
                new DateTimeOffset(TimeZoneInfo.ConvertTimeFromUtc(utcTime, tz), tz.GetUtcOffset utcTime)

            dto2 |> should equal dto1
            let dto1' = testRoundtrip dto1
            let dto2' = testRoundtrip dto2
            dto2' |> should equal dto1')

    [<Test; Category("Primitives")>]
    member __.``Primitive: ISO 8601 string`` () =
        Check.QuickThrowOnFail<DateTime * DateTimeKind> (fun (d : DateTime, k : DateTimeKind) ->
            let dt = new DateTime(d.Ticks, k)
            let s = Xml.XmlConvert.ToString(dt, Xml.XmlDateTimeSerializationMode.RoundtripKind)
            let s' = testRoundtrip s
            s' |> should equal s)

    [<Test; Category("Primitives")>]
    member __.``Primitive: System-TimeSpan`` () = Check.QuickThrowOnFail<TimeSpan> testEquals

    [<Test; Category("Primitives")>]
    member __.``Primitive: System-Guid`` () = Check.QuickThrowOnFail<Guid> testEquals

    [<Test; Category("Primitives")>]
    member __.``Primitive: bigint`` () = Check.QuickThrowOnFail<bigint> testEquals

    [<Test; Category("Bytes")>]
    member __.``Primitive: byte []`` () = testEquals (null : byte []) ; Check.QuickThrowOnFail<byte []> testEquals

    [<Test; Category("DirectoryInfo")>]
    member __.``DirectoryInfo`` () =
        let makeDirectoryInfoPickler (resolver : IPicklerResolver) =
            let stringPickler = resolver.Resolve<string> ()

            let writer (w : WriteState) (di : DirectoryInfo) =
                stringPickler.Write w "Directory" di.FullName

            let reader (r : ReadState) =
                stringPickler.Read r "Directory" |> DirectoryInfo

            Pickler.FromPrimitives (reader, writer)

        let di = DirectoryInfo "testDirectory"
        let registry = new CustomPicklerRegistry ()
        registry.RegisterFactory makeDirectoryInfoPickler
        let cache = PicklerCache.FromCustomPicklerRegistry registry
        let ser = FsPickler.CreateXmlSerializer (picklerResolver = cache)
        di
        |> ser.PickleToString
        |> ser.UnPickleOfString<DirectoryInfo>
        |> should equal di

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
        AppDomain.CurrentDomain.GetAssemblies()
        |> Array.filter (fun a -> if fixture.IsRemotedFixture then a.GlobalAssemblyCache else true)
        |> Array.iter testEquals

    [<Test; Category("Reflection types")>]
    member __.``Reflection: AssemblyName`` () =
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
    member __.``Array: bool`` () = __.CheckArray<bool> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: byte`` () = __.CheckArray<byte> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: sbyte`` () = __.CheckArray<sbyte> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: int16`` () = __.CheckArray<int16> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: int32`` () = __.CheckArray<int32> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: int64`` () = __.CheckArray<int64> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: uint16`` () = __.CheckArray<uint16> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: uint32`` () = __.CheckArray<uint32> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: uint64`` () = __.CheckArray<uint64> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: string`` () = __.CheckArray<string> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: single`` () = __.CheckArray<single> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: double`` () = __.CheckArray<double> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: decimal`` () = __.CheckArray<decimal> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: System-Guid`` () = __.CheckArray<Guid> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: enum`` () =
        __.CheckArray<IntEnum> () ;
        __.CheckArray<CharEnum> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: System-DateTime`` () =
            __.CheckArray<DateTime> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: System-DateTimeOffset`` () = 
            __.CheckArray<DateTimeOffset> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: System-TimeSpan`` () = __.CheckArray<TimeSpan> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: byte []`` () = __.CheckArray<byte []> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: byte [] caching`` () =
        let xs = [|1 .. 100|]
        let ys,zs = testRoundtrip(xs,xs)
        let refEq = obj.ReferenceEquals(ys,zs)
        let bs = [|1uy .. 100uy|]
        let ys,zs = testRoundtrip(bs,bs)
        obj.ReferenceEquals(ys,zs) |> should equal refEq

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: int * string`` () = __.CheckArray<int * string> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: string * (int * decimal)`` () = __.CheckArray<int * string> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: struct(int * string)`` () = __.CheckArray<struct(int * string)> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: struct(string * (int * decimal))`` () = __.CheckArray<struct(string * (int * decimal))> ()

    [<Test; Category("Generic BCL Types")>]
    member __.``Array: struct(string * struct(int * decimal))`` () = __.CheckArray<struct(string * struct (int * decimal))> ()


    //
    //  BCL types
    //

    // Tuples

    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: tuple simple`` () =
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
    member __.``BCL: struct tuple simple`` () =
        Check.QuickThrowOnFail<ValueTuple<string>> testEquals
        Check.QuickThrowOnFail<struct(string * byte)> testEquals
        Check.QuickThrowOnFail<struct(string * byte * TimeSpan)> testEquals
        Check.QuickThrowOnFail<struct(string * byte * TimeSpan * Guid)> testEquals
        Check.QuickThrowOnFail<struct(string * byte * TimeSpan * Guid * int)> testEquals
        Check.QuickThrowOnFail<struct(string * byte * TimeSpan * Guid * int * uint64)> testEquals
        Check.QuickThrowOnFail<struct(string * byte * TimeSpan * Guid * int * uint64 * decimal)> testEquals
        //Check.QuickThrowOnFail<struct(string * byte * TimeSpan * Guid * int * uint64 * decimal * int)> testEquals
        //Check.QuickThrowOnFail<struct(string * byte * TimeSpan * Guid * int * uint64 * decimal * int * int)> testEquals

    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: tuple nested`` () =
        Check.QuickThrowOnFail<int * (string * decimal)> testEquals
        Check.QuickThrowOnFail<(int * (bool * string)) * (string * int16)> testEquals
        Check.QuickThrowOnFail<(int * (bool * (sbyte * string * uint32) * (string * string)))> testEquals


    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: struct tuple nested`` () =
        Check.QuickThrowOnFail<struct(int * struct (string * decimal))> testEquals
        Check.QuickThrowOnFail<struct ((int * struct (bool * string)) * struct (string * int16))> testEquals
        Check.QuickThrowOnFail<struct (int * struct (bool * struct (sbyte * string * uint32) * struct (string * string)))> testEquals


    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: tuple/struct tuple mixed`` () =
        Check.QuickThrowOnFail<int * struct(string * decimal)> testEquals
        Check.QuickThrowOnFail<struct (int * (string * decimal))> testEquals
        Check.QuickThrowOnFail<struct (int * (bool * string)) * (string * int16)> testEquals
        Check.QuickThrowOnFail<struct (int * (bool * string)) * struct (string * int16)> testEquals
        Check.QuickThrowOnFail<(int * struct (bool * string)) * (string * int16)> testEquals
        Check.QuickThrowOnFail<(int * (bool * (sbyte * string * uint32) * struct (string * string)))> testEquals
        Check.QuickThrowOnFail<struct (int * (bool * (sbyte * string * uint32) * (string * string)))> testEquals
        Check.QuickThrowOnFail<(int * struct (bool * (sbyte * string * uint32) * (string * string)))> testEquals
        Check.QuickThrowOnFail<(int * (bool * struct (sbyte * string * uint32) * (string * string)))> testEquals
        Check.QuickThrowOnFail<(int * (bool * struct (sbyte * string * uint32) * struct (string * string)))> testEquals

    // exceptions

    // should properly serialize stacktrace
    member __.TestException(e : 'exn) = e |> addStackTrace |> testReflected

    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: System-Exception`` () = __.TestException <| new Exception("exception message")

    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: SystemException with inner exception`` () =
        let inner = new Exception("inner") |> addStackTrace
        __.TestException <| new Exception("outer", inner)

    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: System-Exception without ISerializable`` () =
        let e = new ExceptionWithoutISerializable<int>(42, "Message", new Exception()) |> addStackTrace
        let e' = testRoundtrip e
        e'.Value |> should equal e.Value
        e'.InnerException.Message |> should equal e.InnerException.Message
        e'.StackTrace |> should equal e.StackTrace

    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: System-Runtime-ExceptionServices-ExceptionDispatchInfo`` () =
        if runsOnMono then
            FsPickler.IsSerializableType<System.Runtime.ExceptionServices.ExceptionDispatchInfo> ()
            |> should equal false
        else
            let bytes =
                fixture.PickleF(fun fsp ->
                    let e = new Exception("message") |> addStackTrace
                    let edi = System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture e
                    fsp.Pickle edi)

            let edi = fixture.UnPickle<System.Runtime.ExceptionServices.ExceptionDispatchInfo> bytes
            let e = try edi.Throw() ; failwith "impossible" with e -> e
            e.StackTrace.Split('\n').Length |> should be (greaterThan 20)

    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: misc exceptions`` () =
        __.TestException <| new InvalidOperationException()
        __.TestException <| new AccessViolationException()
        __.TestException <| new InvalidTimeZoneException()
        __.TestException <| new System.IO.EndOfStreamException()
        __.TestException <| new System.IO.InvalidDataException()
        __.TestException <| new System.IO.FileNotFoundException()

    // collections

    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: System-Collections-Generic-Dictionary`` () =
        let testDictionary (data : seq<'K * 'V>) =
            let data = data |> Seq.distinctBy fst |> Seq.toList
            let d = dict data
            let d' = testRoundtrip d
            let data' = d' |> Seq.map (function KeyValue(k,v) -> k,v) |> Seq.toList
            data' |> should equal data

        Check.QuickThrowOnFail<seq<int64 * (string * float)>> testDictionary
        Check.QuickThrowOnFail<seq<(int64 * string) * string>> testDictionary


    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: System-Collections-Generic-HashSet`` () =
        if runsOnMono then () else // skip due to mono 5.2 bug

        let testSet (data : seq<'T>) =
            let data = data |> Seq.distinct |> Seq.toList
            let d = new HashSet<'T>(data)
            let data' = testRoundtrip d |> Seq.toList
            data' |> should equal data

        Check.QuickThrowOnFail<seq<int64>> testSet
        Check.QuickThrowOnFail<seq<string>> testSet
        Check.QuickThrowOnFail<seq<int * string>> testSet


    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: System-Collections-Generic-Stack`` () =
        let testStack (data : 'T list) =
            let d = new Stack<'T>(data)
            let data' = testRoundtrip d |> Seq.toList |> List.rev
            data' |> should equal data

        Check.QuickThrowOnFail<int64 list> testStack
        Check.QuickThrowOnFail<string list> testStack
        Check.QuickThrowOnFail<(int * string) list> testStack

    // Delegates

    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: delegate simple`` () =
        let d = System.Func<int, int>(fun x -> x + 1)

        (testRoundtrip d).Invoke 41 |> should equal 42

    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: delegate multicast`` () =
        DeleCounter.Value <- 0
        let f n = new TestDelegate(fun () -> DeleCounter.Value <- DeleCounter.Value + n) :> Delegate
        let g = Delegate.Combine [| f 1 ; f 2 |]
        let h = Delegate.Combine [| g ; f 3 |]
        (testRoundtrip h).DynamicInvoke [| |] |> ignore
        DeleCounter.Value |> should equal 6

    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: nullable int`` () =
        let x = Nullable<int>(42)
        let y = Nullable<int> ()
        testEquals x ; testEquals y

    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: nullable decimal`` () =
        let x = Nullable<decimal>(42.01M)
        let y = Nullable<decimal> ()
        testEquals x ; testEquals y

    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: System-Linq enumerables`` () =
        let a = [|1..100|].Select(fun i -> i + 1)
        let b = a.Where(fun i -> i % 2 = 0)
        let c = b.GroupBy(fun s -> s + 1)
        let test enum =
            enum |> testRoundtrip |> Seq.toArray |> should equal (Seq.toArray enum)

        test a
        test b
        test c

    [<Test; Category("Generic BCL Types")>]
    member __.``BCL: Dynamic methods should not be serializable`` () =
        let dynamicMethod = new System.Reflection.Emit.DynamicMethod("test", typeof<int>, [|typeof<int>|])
        let exn = Assert.Throws<FsPicklerException>(fun () -> dynamicMethod |> testRoundtrip |> ignore)
        let inner = Assert.Throws<NonSerializableTypeException>(fun () -> raise exn.InnerException)
        inner.Type |> should equal typeof<System.Reflection.Emit.DynamicMethod>

    //
    // Object serialization
    //

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: boxed values`` () =
        testEquals <| box 42
        testEquals <| box 1.1
        testEquals <| box "test"
        testEquals <| box DateTime.Now
        testEquals <| box (1,1,1)

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: boxed array`` () =
        testEquals <| [| box 1 ; box "test" ; box true ; box 3.14 ; box (1,"1") ; box DateTime.Now |]

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: should correctly deserialize boxed arrays`` () =
        let boxed = { A = [| 1 .. 10|] ; B = [| 1 .. 4 |] }
        testEquals boxed

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: avoid recursion in memberInfo values`` () =
        let m = typeof<OverLoaded>.GetMethods() |> Seq.find(fun x -> x.Name = "A" && x.GetParameters().Length = 1)
        let m0 = m.MakeGenericMethod(typeof<int>)
        testEquals m0

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: should fail at non-serializable type`` () =
        let v = [None ; Some(box <| new System.IO.MemoryStream())]
        try
            let _ = fixture.Pickle v
            failAssert "Should have failed serialization."
        with
        | :? FsPicklerException & InnerExn (:? NonSerializableTypeException) -> ()

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: cyclic array`` () =
        let cyclicArray : obj [] =
            let array = Array.zeroCreate<obj> 10
            for i = 0 to array.Length - 1 do
                array.[i] <- Some (array, i) :> obj
            array

        testReflected cyclicArray

    [<Test; Category("FsPickler Generic tests") ; Repeat(5)>]
    member __.``Object: random object graph`` () =
        let g = createRandomGraph 0.7 20

        let g' = testRoundtrip g

        areEqualGraphs g g' |> should equal true

    member self.TestSequenceRoundtrip (xs : seq<'T>) =
        let bytes =
            fixture.PickleF(fun p ->
                use m = new MemoryStream()
                let length = p.SerializeSequence(m, xs)
                m.ToArray())

        use m = new MemoryStream(bytes)
        let xs' = fixture.Serializer.DeserializeSequence<'T>(m)
        use enum = xs'.GetEnumerator()

        for i,x in xs |> Seq.mapi (fun i x -> (i,x)) do
            if enum.MoveNext() then
                if enum.Current = x then ()
                else
                    failwithf "element %d: expected '%A' but was '%A'." i x enum.Current
            else
                failwithf "sequence terminated early at %d." i

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: int sequence`` () =
        Check.QuickThrowOnFail<seq<int>> __.TestSequenceRoundtrip

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: string sequence`` () =
        Check.QuickThrowOnFail<seq<string>> __.TestSequenceRoundtrip

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: pair sequence`` () =
        Check.QuickThrowOnFail<seq<int * string>> __.TestSequenceRoundtrip

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: large int sequence`` () =
        __.TestSequenceRoundtrip [| 1 .. 1000000 |]

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: large pair sequence`` () =
        let pairs = seq { for i in 1 .. 1000000 -> string i,i }
        __.TestSequenceRoundtrip pairs

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: record sequence`` () =
        let records = seq { for i in 1 .. 10000 -> { Int = i ; String = string i ; Tuple = (i, "const") } }
        __.TestSequenceRoundtrip records

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: struct record sequence`` () =
        let records = seq { for i in 1 .. 10000 -> { SInt = i ; SString = string i ; STuple = (i, "const") } }
        __.TestSequenceRoundtrip records


    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: rec sequence`` () =
        let mkRec () = let rec r = { Rec = r } in r
        let recSeq = seq { for i in 1 .. 1000000 -> mkRec () }
        __.TestSequenceRoundtrip recSeq

    [<Test; Category("FsPickler Generic tests")>]
    member self.``Object: sequence pickler`` () =
        let data =
            fixture.PickleF(fun p ->
                let seqPickler = Pickler.seq Pickler.int

                let state = ref 0
                let sequence =
                    seq {
                        while !state < 100 do
                            yield !state
                            incr state
                    }

                let data = p.Pickle(sequence, pickler = seqPickler)

                !state |> should equal 100

                data)

        fixture.Serializer.UnPickle(data, pickler = Pickler.seq Pickler.int)
        |> Seq.length
        |> should equal 100

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: simple sift serialization`` () =
        let graph : (int * int []) option * int [] option option list = (Some (1, [|1 .. 100|]), [None; None ; Some None; Some (Some [|12|])])
        let sifter = { new IObjectSifter with member __.Sift(p,_,_) = p.Kind = Kind.Array }
        use m = new MemoryStream()
        let sifted = fixture.Serializer.SerializeSifted(m, graph, sifter)
        sifted.Length |> should equal 2
        fixture.Serializer.DeserializeSifted<(int * int []) option * int [] option option list>(m.Clone(), sifted) |> should equal graph

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: tuple sift serialization`` () =
        // test that values are sifted even if they are not cached by reference
        let tuple = (42,"42")
        let p = FsPickler.GeneratePickler<int * string> ()
        p.IsCacheByRef |> should equal false
        let xs = Array.init 10 (fun _ -> tuple)
        let calls = ref 0
        use m = new MemoryStream()
        let sifter = { new IObjectSifter with member __.Sift(_,_,o) = if obj.ReferenceEquals(o,tuple) then incr calls ; true else false }
        let values = fixture.Serializer.SerializeSifted(m, xs, sifter)
        calls.Value |> should equal 1
        values.Length |> should equal 1
        fixture.Serializer.DeserializeSifted<(int * string)[]>(m.Clone(), values) |> should equal xs

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: random sift serialization`` () =
        let r = new System.Random()
        let randomSifter = { new IObjectSifter with member __.Sift(_,_,_) = r.Next(0,5) = 0 }
        let serializer = fixture.Serializer
        Check.QuickThrowOnFail(fun (tree : ListTree<int>) ->
            use m = new MemoryStream()
            let sifted = serializer.SerializeSifted(m, tree, randomSifter)
            serializer.DeserializeSifted<ListTree<int>>(m.Clone(), sifted) |> should equal tree)

    [<Test; Category("FsPickler Generic tests"); Repeat(5)>]
    member __.``Object: random graph sift serialization`` () =
        let g = createRandomGraph 0.4 30
        let r = new System.Random()
        let randomSifter = { new IObjectSifter with member __.Sift(_,_,_) = r.Next(0,5) = 0 }
        use m = new MemoryStream()
        let sifted = fixture.Serializer.SerializeSifted(m, g, randomSifter)
        let g' = fixture.Serializer.DeserializeSifted<Graph<int>>(m.Clone(), sifted)
        areEqualGraphs g g' |> should equal true


    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: FNV1a hash collision tests`` () =
        let hashF = new FNV1aStreamFactory()
        let checkCollisions c N f =
            let collisions =
                getHashCollisions fixture.Serializer hashF N f
                |> Array.sumBy (fun (cs,fq) -> cs * fq)

            collisions |> should be (lessThanOrEqualTo c)


        checkCollisions 10000 0 (fun i -> i)
        checkCollisions 10000 0 (fun i -> [i])
        checkCollisions 10000 0 (fun i -> [|(i, 1L)|])
        checkCollisions 10000 0 (fun i -> sprintf "Lorem ipsum dolor sit amet #%d. Lorem ipsum dolor." i)

        let array = [|let r = new Random() in for i in 1 .. 1000 -> r.Next()|]
        checkCollisions 10000 0 (fun i -> (array, i, array))


    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: MurMur3 hash collision tests`` () =
        let hashF = new MurMur3()
        let checkCollisions c N f =
            let collisions =
                getHashCollisions fixture.Serializer hashF N f
                |> Array.sumBy (fun (cs,fq) -> cs * fq)

            collisions |> should be (lessThanOrEqualTo c)


        checkCollisions 10000 0 (fun i -> i)
        checkCollisions 10000 0 (fun i -> [i])
        checkCollisions 10000 0 (fun i -> [|(i, 1L)|])
        checkCollisions 10000 0 (fun i -> sprintf "Lorem ipsum dolor sit amet #%d. Lorem ipsum dolor." i)

        let array = [|let r = new Random() in for i in 1 .. 1000 -> r.Next()|]
        checkCollisions 10000 0 (fun i -> (array, i, array))

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: size computation`` () =
        let value = box [for i in 1 .. 1000 -> (string i, i)]
        let serializationSize =
            use m = new MemoryStream()
            fixture.Serializer.Serialize<obj>(m, value, leaveOpen = true)
            m.Length

        fixture.Serializer.ComputeSize<obj>(value) |> should equal serializationSize
        fixture.Serializer.ComputeHash(value).Length |> should equal serializationSize

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: leaveOpen=true`` () =
        let m = new MemoryStream()
        fixture.Serializer.Serialize(m, [1 .. 10], leaveOpen = true)
        m.WriteByte(1uy) // should not fail

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: leaveOpen=false`` () =
        let m = new MemoryStream()
        fixture.Serializer.Serialize(m, [1 .. 10], leaveOpen = false)
        (fun () -> m.WriteByte(1uy)) |> shouldFailwith<ObjectDisposedException>

    [<Test; Category("FsPickler Generic tests")>]
    member __.``Object: accumulated size counter`` () =
        let computeSize n =
            use counter = fixture.Serializer.CreateObjectSizeCounter()
            for i = 1 to n do
                counter.Append [1 .. 100]
            counter.Count

        let sz100 = computeSize 100
        let sz = computeSize 1

        sz100 |> should be (greaterThan (99L * sz))
        sz100 |> should be (lessThanOrEqualTo (100L * sz))

    //
    //  Custom types
    //

    [<Test ; Category("Custom types")>]
    member __.``Custom: Union with named data`` () = NamedDUData "" |> testEquals

    [<Test ; Category("Custom types")>]
    member __.``Custom: simple class`` () = testEquals <| SimpleClass(42, "fortyTwo")

    [<Test ; Category("Custom types")>]
    member __.``Custom: generic class`` () =
        let gc = new GenericClass<string * int>("fortyTwo", 42)
        let gc' = testRoundtrip gc
        gc'.Value |> should equal gc.Value

    [<Test ; Category("Custom types")>]
    member __.``Custom: recursive class`` () = testEquals <| RecursiveClass(Some (RecursiveClass(None)))

    [<Test ; Category("Custom types")>]
    member __.``Custom: Simple ISerializable class`` () = testEquals <| SimpleISerializableClass(42, "fortyTwo")

    [<Test ; Category("Custom types")>]
    member __.``Custom: Simple ISerializable struct`` () =
        let x = new SerializableStruct(10, SerializableStruct.DeserializedY + 20)
        let x' = testRoundtrip x
        x'.X |> should equal x.X
        x'.Y |> should equal SerializableStruct.DeserializedY

    [<Test ; Category("Custom types")>]
    member __.``Custom: ISerializable object missing constructor`` () =
        fun () ->
            let x = new ISerializableMissingCtor(42)
            let x' = testRoundtrip x
            ()

        |> shouldFailwith<FsPicklerException>

    [<Test ; Category("Custom types")>]
    member __.``Custom: Generic ISerializable class`` () = testEquals <| GenericISerializableClass<int * string>(42, "fortyTwo", (42, "fortyTwo"))

    [<Test ; Category("Custom types")>]
    member __.``Custom: ISerializable class with IObjectReference template`` () =
        let x = new ObjRef1(0)
        let x' = testRoundtrip x
        x'.Value |> should equal 42

    [<Test ; Category("Custom types")>]
    member __.``Custom: ISerializable class with IObjectReference object`` () =
        let x = new ObjRef2(-1)
        let x' = testRoundtrip x
        x'.Value |> should equal -1

    [<Test ; Category("Custom types")>]
    member __.``Custom: ISerializable class implementing IObjectReference`` () =
        let x = new ObjRef3(0)
        let x' = testRoundtrip x
        x'.Value |> should equal 42

    [<Test ; Category("Custom types")>]
    member __.``Custom: POCO implementing IObjectReference`` () =
        let x = new PocoObjRef(0)
        let x' = testRoundtrip x
        x'.Value |> should equal 42

    [<Test ; Category("Custom types")>]
    member __.``Custom: Data Contract class implementing IObjectReference`` () =
        let x = new DataContractObjRef(0)
        let x' = testRoundtrip x
        x'.Value |> should equal 42

    [<Test ; Category("Custom types")>]
    member __.``Custom: Data Contract class`` () =
        let value = new DataContractClass<int option>(Some 42, "value")
        let value' = testRoundtrip value
        value'.A |> should equal value.A
        value'.B |> should not' (equal value.B)

    [<Test ; Category("Custom types")>]
    member __.``Custom: Data Contract class with exclusion`` () =
        let value = new FieldDataContractClass<_>(42, "test")
        let value' = testRoundtrip value
        value'.A |> should equal value.A
        value'.B |> should equal value.B

    [<Test ; Category("Custom types")>]
    member __.``Custom: Data Contract class with parameterless constructor`` () =
        let value = new DataContractWithParameterlessConstructor()
        let value' = testRoundtrip value
        value'.A |> should equal value.A
        value'.Value |> should equal value.Value

    [<Test ; Category("Custom types")>]
    member __.``Custom: Data Contract struct`` () =
        let value = new StructWithDataContract(42, "Me", Guid.NewGuid(), 500.0)
        let value' = testRoundtrip value
        value'.Age |> should equal value.Age
        value'.Name |> should equal value.Name
        value'.Guid |> should equal value.Guid
        value'.Salary |> should equal Unchecked.defaultof<float>

    [<Test ; Category("Custom types")>]
    member __.``Custom: struct`` () =
        let s = new StructType(42, "foobar") |> testRoundtrip
        s.X |> should equal 42
        s.Y |> should equal "foobar"

    [<Test ; Category("Custom types")>]
    member __.``Custom: pickler factory class`` () =
        let x = ClassWithPicklerFactory(0) |> testRoundtrip
        x.Value |> should equal 42

    [<Test ; Category("Custom types")>]
    member __.``Custom: pickler factory with recursive bindings`` () =
        let x = new ClassWithCombinators(12, None)
        let y = new ClassWithCombinators(0, Some x)

        let z = testRoundtrip y

        z.Value |> snd |> Option.map (fun x -> x.Value) |> Option.get |> fst |> should equal 42

    [<Test ; Category("Custom types")>]
    member __.``Custom: record with SerializationInfo combinator`` () =
        Check.QuickThrowOnFail(
            fun (r : RecordWithISerializableCombinators) ->
                let r' = testRoundtrip r
                r'.Name |> should equal r.Name
                // custom pickler increments date of birth by one
                r'.DoB |> should equal (r.DoB + 1)
                r'.DoD |> should equal r.DoD)


    [<Test ; Category("Custom types")>]
    member __.``Custom: disable subtype resolution on serialization`` () =
        let serializer = fixture.NewSerializer()
        serializer.DisableSubtypeResolution <- true
        shouldFailwith<FsPicklerException> (fun () -> serializer.Pickle(fun i -> i + 1) |> ignore)

    [<Test ; Category("Custom types")>]
    member __.``Custom: disable subtype resolution on deserialization`` () =
        let serializer = fixture.NewSerializer()
        let pickle = serializer.Pickle(fun i -> i + 1)
        serializer.DisableSubtypeResolution <- true
        shouldFailwith<FsPicklerException> (fun () -> serializer.UnPickle<int -> int>(pickle) |> ignore)

    //
    //  FSharp Tests
    //

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: option`` () =
        Check.QuickThrowOnFail<int option> testEquals
        Check.QuickThrowOnFail<string option> testEquals
        Check.QuickThrowOnFail<(int * string) option> testEquals
        Check.QuickThrowOnFail<decimal option option> testEquals

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: ref`` () =
        Check.QuickThrowOnFail<int ref> testEquals
        Check.QuickThrowOnFail<string ref> testEquals

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: choice`` () =
        Check.QuickThrowOnFail<Choice<int, string>> testEquals
        Check.QuickThrowOnFail<Choice<float, decimal, string>> testEquals
        Check.QuickThrowOnFail<Choice<float, decimal, string, int>> testEquals
        Check.QuickThrowOnFail<Choice<float, decimal, string, int, string>> testEquals
        Check.QuickThrowOnFail<Choice<float, decimal, string, int, string, float>> testEquals
        Check.QuickThrowOnFail<Choice<float, decimal, string, int, string, float, byte>> testEquals

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: list`` () =
        Check.QuickThrowOnFail<int list> testEquals
        Check.QuickThrowOnFail<bool list> testEquals
        Check.QuickThrowOnFail<float list> testEquals
        Check.QuickThrowOnFail<decimal list> testEquals
        Check.QuickThrowOnFail<string list> testEquals
        Check.QuickThrowOnFail<(string * int) list> testEquals
        Check.QuickThrowOnFail<bool option list> testEquals
        Check.QuickThrowOnFail<byte [] list> testEquals

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: simple union`` () =
        Check.QuickThrowOnFail<SimpleDU> testEquals

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: struct union`` () =
        Check.QuickThrowOnFail<StructDU> testEquals

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: recursive union`` () =
        testEquals (int2Peano 42)
        Check.QuickThrowOnFail<Peano> testEquals

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: tree`` () =
        testEquals (mkTree 5)
        Check.QuickThrowOnFail<BinTree<string * bool>> testEquals

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: mutual recursive union`` () =
        testEquals <| nTree 5
        Check.QuickThrowOnFail<Tree<bool option>> testEquals
        Check.QuickThrowOnFail<Forest<int * string>> testEquals

    [<Test; Category("FSharp type tests")>]
    member self.``FSharp: combinator-based recursive union`` () =
        let data =
            fixture.PickleF(fun p ->
                let n = int2Peano 100
                let pp = mkPeanoPickler()
                p.Pickle(n, pickler = pp))

        fixture.Serializer.UnPickle(data, pickler = mkPeanoPickler()) |> should equal (int2Peano 100)

    [<Test; Category("FSharp type tests")>]
    member self.``FSharp: combinator-based mutual recursive union`` () =
        let data =
            fixture.PickleF(fun p ->
                let tp,_ = getTreeForestPicklers Pickler.int
                let t = nTree 6

                p.Pickle(t, pickler = tp))

        fixture.Serializer.UnPickle(data, pickler = (getTreeForestPicklers Pickler.int |> fst))
        |> should equal (nTree 6)

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: record`` () =
        Check.QuickThrowOnFail<Record> testEquals

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: struct record`` () =
        Check.QuickThrowOnFail<StructRecord> testEquals

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: cyclic record`` () =
        let rec f = { Rec = f }
        testReflected f

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: core types`` () =
        Check.QuickThrowOnFail<int * (SimpleDU * Peano) option * (int * string * float) list * Set<int>> testEquals

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: union types with reference equality`` () =
        let r = RefEq 42
        let (r',r'') = testRoundtrip (r,r)
        r' |> should equal r''

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: exception`` () =
        let mkExn () = FSharpException(42, "fortyTwo") :?> FSharpException |> addStackTrace

        // F# exception serialization is broken
        // so need to make sure that serialization is initialized at serialization domain
        // rather than copied
        let pickle = fixture.PickleF(fun p -> p.Pickle(mkExn()))

        let e0 = fixture.Serializer.UnPickle<FSharpException>(pickle)
        let e = mkExn()

        e0.ToString() |> should equal (e.ToString())
        e0.Data0 |> should equal e.Data0
        e0.Data1 |> should equal e.Data1

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: map`` () =
        Check.QuickThrowOnFail<Map<int,string>> testEquals
        Check.QuickThrowOnFail<Map<int * int,string>> testEquals

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: set`` () =
        Check.QuickThrowOnFail<Set<float>> testEquals
        Check.QuickThrowOnFail<Set<int * string>> testEquals


    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: function`` () =
        let f x = x + 1

        (testRoundtrip f) 41 |> should equal 42

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: function curried`` () =
        let f x y = x + y

        (testRoundtrip (f 41)) 1 |> should equal 42

    [<Test; Category("FSharp extension methods")>]
    member __.``FSharp: extension methods`` () =
        testEquals <| getMemberCall <@ fun (t : Task) -> Async.AwaitTask t @>
        testEquals <| getMemberCall <@ Stream.AsyncCopy @>

        testReflected
            <@
                async {
                    do! Unchecked.defaultof<Task>
                    let! x = Unchecked.defaultof<Task<int>>
                    return x + 1
                }
            @>

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: function closure`` () =
        let f () =
            let x = System.Random().Next()
            fun () -> x + 1

        let g = f ()

        (testRoundtrip g) () |> should equal (g ())

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: expression builders`` () =
        let i = ref 0
        let infty =
            seq {
                while true do
                    incr i
                    yield !i
            }

        testRoundtrip infty |> Seq.take 5 |> Seq.toArray |> should equal [|1..5|]

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: quotation simple`` () =
        testReflected <@@ 1 + 1 @@>
        testReflected <@ if true then failwith "error" else 1 @>

    [<Test; Category("FSharp type tests")>]
    member __.``FSharp: quotation large`` () =
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
        let pickle = fixture.Serializer.Pickle(v, Pickler.auto<'In>)
        try
            let result = fixture.Serializer.UnPickle<'Out>(pickle, Pickler.auto<'Out>)
            failAssert "should have failed deserialization"
        with :? FsPicklerException & InnerExn (:? InvalidPickleTypeException) -> ()

    [<Test; Category("Stress tests")>]
    member t.``Stress test: deserialization type mismatch`` () =
        match fixture.Serializer with
        | :? JsonSerializer as jsp when jsp.OmitHeader -> ()
        | _ ->
            t.TestTypeMismatch<int, string> 42
            t.TestTypeMismatch<string, int> "forty-two"
            t.TestTypeMismatch<obj, int>(obj())
            t.TestTypeMismatch<int * int64, int * int> (1,1L)

    member t.TestDeserializeInvalidData<'T> (bytes : byte []) =
        try
            use m = new MemoryStream(bytes)
            let t' = fixture.Serializer.Deserialize<'T>(m)
            failwith "should have failed."
        with :? FsPicklerException -> ()

    [<Test; Category("Stress tests")>]
    member t.``Stress test: arbitrary data deserialization`` () =
        match fixture.Serializer with
        | :? JsonSerializer as jsp when jsp.OmitHeader -> ()
        | _ ->
            Check.QuickThrowOnFail (fun bs -> t.TestDeserializeInvalidData<int> bs)
            Check.QuickThrowOnFail (fun bs -> t.TestDeserializeInvalidData<string> bs)
            Check.QuickThrowOnFail (fun bs -> t.TestDeserializeInvalidData<byte []> bs)
            Check.QuickThrowOnFail (fun bs -> t.TestDeserializeInvalidData<int * string option> bs)

    [<Test; Category("Stress tests")>]
    member __.``Stress test: massively auto-generated objects`` () =
        // generate serializable objects that reside in mscorlib and FSharp.Core
        let inputData = seq {
            if not runsOnMono then
                // as of Mono 3.8.0, performing serializations on
                // uninitialized BCL objects has become too unstable; eliminate.
                yield! generateSerializableObjects typeof<int>.Assembly

            yield! generateSerializableObjects typeof<_ option>.Assembly
            yield! generateSerializableObjects typeof<FsPickler>.Assembly
        }

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
