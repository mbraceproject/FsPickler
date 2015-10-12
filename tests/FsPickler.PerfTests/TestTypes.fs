namespace Nessos.FsPickler.Tests

#nowarn "346"

open System
open System.IO
open System.Reflection
open System.Runtime.Serialization

open ProtoBuf

open Nessos.FsPickler
open Nessos.FsPickler.Combinators

[<AutoOpen>]
module TestTypes =

    let private random = new Random(System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(obj()))

    type Peano =
        | Zero
        | Succ of Peano

    let int2Peano n =
        let rec aux pred = 
            function
            | 0 -> pred
            | n -> aux (Succ pred) (n-1)

        aux Zero n

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

    type SimplePoco private (id : int, name : string, surname : string, age : int, data : byte [], date : DateTime) =
        static member Create() = new SimplePoco(random.Next(), "John", "Smith", 42, [| 1uy .. 100uy |], DateTime.Now)
        member __.Id = id
        member __.Name = name
        member __.Surname = surname
        member __.Age = age
        member __.Data = data
        member __.Date = date

    [<DataContract>]
    type DataContractClass (name : string, surname : string, age : int) =
        [<DataMember(Name = "Name")>]
        member val Name = name with get,set
        [<DataMember(Name = "Surname")>]
        member val Surname = surname with get,set
        [<DataMember(Name = "Age")>]
        member val Age = age with get,set
        new () = new DataContractClass(null,null,-1)

    [<Sealed>]
    [<ProtoContract(ImplicitFields = ImplicitFields.AllFields)>]
    type ClassTree<'T> (value : 'T, children : ClassTree<'T> []) =
        
        new () = ClassTree<'T>(Unchecked.defaultof<'T>, [||])
        
        member __.Value = value
        member __.Children = children

        member __.NodeCount =
            1 + (children |> Array.sumBy(fun c -> c.NodeCount))

    let rec mkClassTree (size : int) =
        if size = 0 then ClassTree(SimplePoco.Create(),[||])
        else
            let children = Array.init 3 (fun _ -> mkClassTree (size - 1))
            ClassTree(SimplePoco.Create(),children)

    let mkExceptionWithStackTrace() =
        let rec dive d =
            if d = 0 then failwith "boom!"
            else
                1 + dive (d - 1)

        let e = 
            try dive 42 |> ignore ; None
            with e -> Some e

        match e with
        | None -> failwith "error"
        | Some e -> e
