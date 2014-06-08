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

        type ClassWithGenericMethod =
            static member Method<'T,'S> () = Unchecked.defaultof<'T>, Unchecked.defaultof<'S>

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

        type SimpleISerializableClass(x : int, y : string) =
            new(s : SerializationInfo, _ : StreamingContext) =
                new SimpleISerializableClass(s.GetInt32 "integer", s.GetString "string")

            member __.Value = y + string x
            override __.Equals y = 
                match y with 
                | :? SimpleISerializableClass as y -> y.Value = __.Value 
                | _ -> false

            interface ISerializable with
                member __.GetObjectData(s : SerializationInfo, _ : StreamingContext) =
                    s.AddValue("string", y)
                    s.AddValue("integer", x)

        type GenericISerializableClass<'T>(x : int, y : string, z : 'T) =
            member __.X = x
            member __.Y = y
            member __.Z = z

            new (sI : SerializationInfo, _ : StreamingContext) =
                new GenericISerializableClass<'T>(sI.GetInt32("x"), sI.GetString("y"), sI.GetValue("z", typeof<'T>) :?> 'T)

            interface ISerializable with
                member __.GetObjectData (sI : SerializationInfo, _ : StreamingContext) =
                    sI.AddValue("x", x)
                    sI.AddValue("y", y)
                    sI.AddValue("z", z)

            override x.Equals y =
                match y with
                | :? GenericISerializableClass<'T> as y -> x.X = y.X && x.Y = y.Y
                | _ -> false

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

        
        let addStackTrace (e : 'exn) =
            let rec dive n =
                if n = 0 then raise e
                else
                    1 + dive (n-1)

            try dive 20 |> ignore; invalidOp "should have failed"
            with :? 'exn as e -> e

        exception FSharpException of int * string


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

        type GenericType<'T when 'T : comparison>(x : 'T) =
            member __.Value = x

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


        type Node<'T> = { Value : 'T ; mutable Neigh : Node<'T> list }
        and Graph<'T> = { Nodes : Node<'T> list }

        let createRandomGraph (probability : float) (size : int) =
            let r = new System.Random()
            let nodes = [1..size] |> List.map (fun i -> { Value = i ; Neigh = []})
            for n in nodes do
                let neigh = nodes |> List.filter (fun _ -> r.NextDouble() < probability)
                n.Neigh <- neigh
            { Nodes = nodes }

        let areEqualGraphs<'T when 'T : comparison> (g1 : Graph<'T>) (g2 : Graph<'T>) =
            let toAdjacencyMap (g : Graph<'T>) = 
                g.Nodes 
                |> Seq.map (fun n -> n.Value, n.Neigh |> List.map (fun n -> n.Value) )
                |> Map.ofSeq

            toAdjacencyMap g1 = toAdjacencyMap g2


        // automated large-scale object generation
        let generateSerializableObjects (assembly : Assembly) =
            let filterType (t : Type) =
                if runsOnMono && t.Namespace = "System.Reflection" then false // mono bug that kills the runtime
                elif runsOnMono && t = typeof<System.Collections.CaseInsensitiveComparer> then false // mono bug in NoEmit build
                elif runsOnMono && t = typeof<System.Globalization.DateTimeFormatInfo> then false // mono bug in NoEmit build
                else
                    // types that cause .IsSerializable to fail
                    // should be included in the testing
                    try FsPickler.IsSerializableType t with _ -> true

            let tryActivate (t : Type) =
                try Some (t, Activator.CreateInstance t)
                with _ -> None

            // only test things that are successfully serialized by BinaryFormatter
            let filterObject (t : Type, o : obj) =
                try cloneObject o |> ignore ; true
                with _ -> false
            
            assembly.GetTypes()
            |> Seq.filter filterType
            |> Seq.choose tryActivate
            |> Seq.filter filterObject