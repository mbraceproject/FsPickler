namespace Nessos.FsPickler.Tests

    #nowarn "346"

    open System
    open System.IO
    open System.Reflection
    open System.Runtime.Serialization

    open Nessos.FsPickler
    open Nessos.FsPickler.Combinators

    module TestTypes =

        type Peano =
            | Zero
            | Succ of Peano

        let rec int2Peano n = match n with 0 -> Zero | n -> Succ(int2Peano(n-1))

        type BinTree<'T> =
            | Leaf
            | Node of 'T * BinTree<'T> * BinTree<'T>

        let rec mkTree (n : int) =
            match n with
            | 0 -> Leaf
            | n -> Node(string n, mkTree(n-1), mkTree(n-1))

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

        type DU = 
            | Nothing 
            | Something of string * int
            | SomethingElse of string * int * obj

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

        let stringValue = 
            "Lorem ipsum dolor sit amet, consectetur adipisicing elit, 
                sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."