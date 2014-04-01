namespace Nessos.FsPickler.Tests

    #nowarn "346"

    open System
    open System.IO
    open System.Reflection
    open System.Runtime.Serialization

    open Nessos.FsPickler
    open Nessos.FsPickler.Combinators

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

        type OverLoaded () =
            member __.A<'T> () = ()
            member __.A<'T> (x : 'T) = ()
            member __.B<'T> (x : 'T option) = ()
            member __.B<'T> (x : 'T) = ()

        type Tree<'T> = Empty | Node of 'T * Forest<'T>
        and Forest<'T> = Nil | Cons of Tree<'T> * Forest<'T>


        let rec nTree (n : int) =
            if n = 0 then Empty
            else
                Node(n, nForest (n-1) (n-1))

        and nForest (d : int) (l : int) =
            if l = 0 then Nil
            else
                Cons(nTree d, nForest d (l-1))


        let getTreeForestPicklers (elementPickler : Pickler<'T>) =
            Pickler.fix2(fun treeP forestP ->

                let treeP' =
                    Pickler.pair elementPickler forestP
                    |> Pickler.option 
                    |> Pickler.wrap 
                        (function None -> Empty | Some (v,f) -> Node(v,f))
                        (function Empty -> None | Node (v,f) -> Some(v,f))

                let forestP' =
                    Pickler.pair treeP forestP
                    |> Pickler.option
                    |> Pickler.wrap
                        (function None -> Nil | Some (t,f) -> Cons(t,f))
                        (function Nil -> None | Cons(t,f) -> Some (t,f))

                treeP', forestP')

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
            let t = Choice1Of2 self
            member x.Value = s
            member x.Value' = t

        type SerializableClass(x : int, y : string) =
            new(s : SerializationInfo, _ : StreamingContext) =
                new SerializableClass(s.GetInt32 "integer", s.GetString "string")

            member __.Value = y + string x
            override __.Equals y = match y with :? SerializableClass as y -> y.Value = __.Value | _ -> false

            interface ISerializable with
                member __.GetObjectData(s : SerializationInfo, _ : StreamingContext) =
                    s.AddValue("string", y)
                    s.AddValue("integer", x)

        [<CustomPickler>]
        type ClassWithPicklerFactory (x : int) =

            member __.Value = x

            static member CreatePickler (resolver : IPicklerResolver) =
                Pickler.FromPrimitives(
                    (fun _ -> ClassWithPicklerFactory(42)),
                    (fun _ _ -> ()),
                        true, false)

        [<CustomPickler>]
        type ClassWithCombinators (x : int, y : ClassWithCombinators option) =
            member __.Value = x,y

            static member CreatePickler (resolver : IPicklerResolver) =
                Pickler.fix(fun self -> 
                    self 
                    |> Pickler.option 
                    |> Pickler.pair Pickler.int
                    |> Pickler.wrap (fun (_,y) -> new ClassWithCombinators(42,y)) (fun c -> c.Value))

        exception FsharpException of int * string

        type BinTree<'T> =
            | Leaf
            | Node of 'T * BinTree<'T> * BinTree<'T>

        let rec mkTree (n : int) =
            match n with
            | 0 -> Leaf
            | n -> Node(string n, mkTree(n-1), mkTree(n-1))

        type Rec = { Rec : Rec }

        type BoxedArrays = { A : seq<int> ; B : seq<int> }

        type NonGenericType = NGValue of int

        type ExternalTypePickler () =
            interface IPicklerFactory

            member __.Create (resolver : IPicklerResolver) =
                Pickler.FromPrimitives((fun _ -> NGValue 42), fun _ _ -> ())

        type GenericType<'T when 'T : comparison>(x : 'T) =
            member __.Value = x

        type ExternalGenericTypePickler () =
            interface IPicklerFactory

            member __.Create<'T when 'T : comparison> (resolver : IPicklerResolver) =
                let valueFmt = resolver.Resolve<'T> ()

                let writer (w : Writer) (g : GenericType<'T>) =
                    w.Write(valueFmt, g.Value)

                let reader (r : Reader) =
                    let value = r.Read valueFmt
                    new GenericType<'T>(Unchecked.defaultof<'T>)

                Pickler.FromPrimitives(reader, writer) :> Pickler

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
            let registry = new CustomPicklerRegistry("unit test cache")
            do
                registry.RegisterPicklerFactory(new ExternalTypePickler())
                registry.RegisterPicklerFactory(new ExternalGenericTypePickler())

            new FsPicklerSerializer(registry)


        // test provision for top-level sequence serialization
        let testSequence<'T when 'T : equality> (xs : seq<'T>) =
            use m = new MemoryStream()
            let length = testSerializer.FSCS.SerializeSequence(m, xs)
            m.Position <- 0L
            use enum = xs.GetEnumerator()
            use enum' = testSerializer.FSCS.DeserializeSequence<'T>(m, length)
            let mutable success = true
            while success && enum.MoveNext() do 
                if enum'.MoveNext() && enum.Current = enum'.Current then ()
                else
                    success <- false
            success


        // automated large-scale object generation
        let generateSerializableObjects (assembly : Assembly) =
            let fscs = testSerializer.FSCS

            let filterType (t : Type) =
                try not <| t.Namespace.StartsWith "System.Reflection" && fscs.IsSerializableType t
                with _ -> false

            let tryActivate (t : Type) =
                try Some (t, Activator.CreateInstance t)
                with _ -> None

            let bfs = new BinaryFormatterSerializer()
            let filterObject (t : Type, o : obj) =
                try Serializer.roundtrip o bfs |> ignore ; true
                with _ -> false
            
            assembly.GetTypes()
            |> Seq.filter filterType
            |> Seq.choose tryActivate
            |> Seq.filter filterObject