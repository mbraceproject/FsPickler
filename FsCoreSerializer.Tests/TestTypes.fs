namespace FsCoreSerializer.Tests

    #nowarn "346"

    open System
    open System.Reflection
    open System.Runtime.Serialization

    open FsCoreSerializer

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

        type TestDelegate = delegate of unit -> unit

        and DeleCounter () =
            static let mutable cnt = 0
            static member Value 
                with get () = cnt
                and set i = cnt <- i

        [<Struct>]
        type StructType(x : int, y : string) =
            member __.X = x
            member __.Y = y

        type DU = 
            | Nothing 
            | Something of string * int
            | SomethingElse of string * int * obj

        type BinTree =
            | Leaf
            | Node of string * BinTree * BinTree

        type Record =
            { Int : int ; String : string ; Tuple : int * string }


        type Class(x : int, y : string) =
            member __.X = x
            member __.Y = y

        type SerializableClass<'T>(x : int, y : string, z : 'T) =
            member __.X = x
            member __.Y = y
            member __.Z = z

            new (sI : SerializationInfo, _ : StreamingContext) =
                new SerializableClass<'T>(sI.GetInt32("x"), sI.GetString("y"), sI.GetValue("z", typeof<'T>) :?> 'T)


            interface ISerializable with
                member __.GetObjectData (sI : SerializationInfo, _ : StreamingContext) =
                    sI.AddValue("x", x)
                    sI.AddValue("y", y)
                    sI.AddValue("z", z)


        let stringValue = 
            "Lorem ipsum dolor sit amet, consectetur adipisicing elit, 
                sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."


        // create serializer
        let testSerializer =
            let registry = new FormatterRegistry()
            do
                registry.RegisterFormatterFactory(new FormatterFactoryTest())
                registry.RegisterGenericFormatter(new GenericTypeFormatter())

            new TestFsCoreSerializer(registry) :> ISerializer


        // automated large-scale object generation
        let generateSerializableObjects (assembly : Assembly) =
            let fscs = (testSerializer :?> TestFsCoreSerializer).FSCS

            let filterType (t : Type) =
                try fscs.IsSerializableType t
                with _ -> true

            let tryActivate (t : Type) =
                try
                    let ctorFlags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance
                    match t.GetConstructor(ctorFlags, null, [||], [||]) with
                    | null -> None
                    | ctorInfo -> Some (t, ctorInfo.Invoke [||])
                with _ -> None

            let bfs = new TestBinaryFormatter()
            let filterObject (t : Type, o : obj) =
                try Serializer.writeRead bfs o |> ignore ; true
                with _ -> false
            
            assembly.GetTypes()
            |> Seq.filter filterType
            |> Seq.choose tryActivate
            |> Seq.filter filterObject