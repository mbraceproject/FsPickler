[<AutoOpen>]
module MBrace.FsPickler.Tests.TestTypes

#nowarn "346"

open System
open System.IO
open System.Reflection
open System.Runtime.Serialization

open MBrace.FsPickler
open MBrace.FsPickler.Combinators
open MBrace.FsPickler.Hashing

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

type NamedDUData = NamedDUData of AA: string

type GenericDU<'T> = GValue of 'T

[<Struct>]
type GenericStructDU<'T> = SGValue of 'T

type Either<'T,'S> = L of 'T | R of 'S

let rec int2Peano n = match n with 0 -> Zero | n -> Succ(int2Peano(n-1))

let mkPeanoPickler () =
    Pickler.fix(fun peanoP -> 
        peanoP 
        |> Pickler.option 
        |> Pickler.wrap 
            (function None -> Zero | Some p -> Succ p) 
            (function Zero -> None | Succ p -> Some p))

type ClassWithGenericMethod =
    static member Method<'T,'S> () = Unchecked.defaultof<'T>, Unchecked.defaultof<'S>

type IAbstract = interface end

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

type Enum =
    | EnumA = 0
    | EnumB = 1
    | EnumC = 2

type CharEnum =
    | CEnumA = 'a'
    | CEnumB = 'b'
    | CEnumC = 'c'

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

[<Sealed>]
type GenericClass<'T>(x : 'T) =
    member __.Value = x

[<Sealed>]
type RecursiveClass(x : RecursiveClass option) =
    member __.Value = x
    override __.Equals y = match y with :? RecursiveClass as y -> y.Value = __.Value | _ -> false

[<Sealed>]
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

[<DataContract>]
type DataContractClass<'T> =
            
    val mutable private x : 'T
    val mutable private y : string

    new(x : 'T, y : string) = { x = x ; y = y }

    [<DataMember>]
    member __.A
        with get () = __.x
        and set (t : 'T) = __.x <- t

    member __.B
        with get () = __.y
        and set (t : string) = __.y <- t

[<DataContract>]
type FieldDataContractClass<'T> =
    [<DataMember>]
    val mutable private x : 'T
    val mutable private y : string

    new(x : 'T, y : string) = { x = x ; y = y }

    member __.A
        with get () = __.x
        and set (t : 'T) = __.x <- t

    [<DataMember>]
    member __.B
        with get () = __.y
        and set (t : string) = __.y <- t

[<DataContract>]
type DataContractWithParameterlessConstructor() =
    let x = 42
    let mutable y = "test"

    member __.Value = x
    [<DataMember>]
    member __.A
        with get () = y
        and  set y' = y <- y'

[<CustomPickler>]
type ClassWithPicklerFactory (x : int) =

    member __.Value = x

    static member CreatePickler (resolver : IPicklerResolver) =
        Pickler.FromPrimitives(
            (fun _ -> ClassWithPicklerFactory(42)),
            (fun _ _ -> ()), cacheByRef = true, useWithSubtypes = false)

[<CustomPickler>]
type ClassWithCombinators (x : int, y : ClassWithCombinators option) =
    member __.Value = x,y

    static member CreatePickler (resolver : IPicklerResolver) =
        Pickler.fix(fun self -> 
            self 
            |> Pickler.option 
            |> Pickler.pair Pickler.int
            |> Pickler.wrap (fun (_,y) -> new ClassWithCombinators(42,y)) (fun c -> c.Value))


[<CustomPickler>]
type RecordWithISerializableCombinators =
    { Name : string ; DoB : int ; DoD : int option }
with
    static member CreatePickler (_ : IPicklerResolver) =
        Pickler.fromSerializationInfo
                    (fun sI -> { Name = sI.GetString "Name" ; DoB = sI.GetInt32 "DateOfBirth" ; DoD = sI.TryGet "DateOfDeath" })
                    (fun sI r -> sI.AddValue("Name", r.Name) ; r.DoD |> Option.iter (fun d -> sI.AddValue("DateOfDeath", d)) ; sI.AddValue("DateOfBirth", r.DoB + 1))

[<CloneableOnly>]
type CloneableOnlyType () =
    let cancellationToken = new System.Threading.CancellationToken()
    member __.Value = cancellationToken
        
let addStackTrace (e : 'exn) =
    let rec dive n =
        if n = 0 then raise e
        else
            1 + dive (n-1)

    try dive 20 |> ignore; invalidOp "should have failed"
    with :? 'exn as e -> e

exception FSharpException of int * string

type ExceptionWithoutISerializable<'T>(value : 'T, msg : string, inner) =
    inherit Exception(msg, inner)
    member __.Value = value

type BinTree<'T> =
    | Leaf
    | Node of 'T * BinTree<'T> * BinTree<'T>

let rec mkTree (n : int) =
    match n with
    | 0 -> Leaf
    | n -> Node(string n, mkTree(n-1), mkTree(n-1))

type ListTree<'T> =
    | Leaf
    | Branch of 'T * ListTree<'T> list

[<CustomEquality ; NoComparison>]
type Rec = { Rec : Rec }
with
    override r.Equals o =
        match o with
        | :? Rec as r' -> obj.ReferenceEquals(r, r.Rec) = obj.ReferenceEquals(r', r'.Rec)
        | _ -> false

type BoxedArrays = { A : seq<int> ; B : seq<int> }

type NonGenericType = NGValue of int

type GenericType<'T when 'T : comparison>(x : 'T) =
    member __.Value = x

type TestDelegate = delegate of unit -> unit

and DeleCounter =
    [<DefaultValue; ThreadStatic>]
    static val mutable private cnt : int
    static member Value 
        with get () = DeleCounter.cnt
        and set i = DeleCounter.cnt <- i

[<Struct>]
type StructType(x : int, y : string) =
    member __.X = x
    member __.Y = y

[<Struct>]
type GenericStruct<'T>(value : 'T) =
    member __.Value = value

type DU = 
    | Nothing 
    | Something of string * int
    | SomethingElse of string * int * obj

[<RequireQualifiedAccess>]
[<Struct>]
type StructDU =
    | Nothing
    | Something of something: string * numberA: int
    | SomethingElse of somethingElse: string * numberB:int * obj: obj

type BinTree =
    | Leaf
    | Node of string * BinTree * BinTree

type Record =
    { Int : int ; String : string ; Tuple : int * string }

type GenericRecord<'T> = { GValue : 'T }

[<Struct>]
type StructRecord =
    { SInt : int ; SString : string ; STuple : int * string }

[<Struct>]
type StructGenericRecord<'T> =
    { SGValue : 'T }

type Class(x : int, y : string) =
    member __.X = x
    member __.Y = y

type PolyRec<'T> = V of PolyRec<'T option>

type APoly<'T, 'S> = { B : BPoly<'T> }
and BPoly<'X> = { A : APoly<'X option, int> }

type PseudoPolyRec<'T> = V of PseudoPolyRec<bool * int>

let isRecursive<'T> = FsPickler.GeneratePickler<'T>().IsRecursiveType
let isOpenHierarchy<'T> = FsPickler.GeneratePickler<'T>().IsOpenHierarchy
let isFixedSize<'T> = FsPickler.GeneratePickler<'T>().IsOfFixedSize


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

type ISerializableMissingCtor(x : int) =
    member __.Value = x
    interface ISerializable with
        member __.GetObjectData(s:SerializationInfo,_:StreamingContext) =
            s.AddValue("value", x)

type ObjRef1(x : int) =
    member __.Value = x

    interface ISerializable with    
        member __.GetObjectData(s:SerializationInfo,_:StreamingContext) =
            s.SetType typeof<ObjRefHelper1>

and ObjRefHelper1 =
    interface IObjectReference with
        member __.GetRealObject _ = new ObjRef1(42) :> obj

type ObjRef2(x : int) =
    member __.Value = x

    interface ISerializable with    
        member __.GetObjectData(s:SerializationInfo,_:StreamingContext) =
            s.SetType typeof<ObjRefHelper2>
            s.AddValue("value",x)

and ObjRefHelper2 private (s:SerializationInfo, c:StreamingContext) =
    let value = s.GetInt32 "value"
    interface IObjectReference with
        member __.GetRealObject _ = new ObjRef2(value) :> obj

type ObjRef3(x : int) =
    private new (s:SerializationInfo, c:StreamingContext) = new ObjRef3(s.GetInt32 "value")
    member __.Value = x

    interface ISerializable with
        member __.GetObjectData(s:SerializationInfo,_:StreamingContext) =
            s.AddValue("value",x)

    interface IObjectReference with
        member __.GetRealObject _ = new ObjRef3(42) :> obj

type PocoObjRef(x : int) =
    member __.Value = x

    interface IObjectReference with
        member __.GetRealObject _ = new PocoObjRef(42) :> obj

[<DataContract>]
type DataContractObjRef(x : int) =
    let mutable x = x

    [<DataMember>]
    member __.Value 
        with get () = x
        and set y = x <- y

    interface IObjectReference with
        member __.GetRealObject _ = new DataContractObjRef(42) :> obj

// the following types test a certain pickler generation ordering condition:
// in certain versions of FsPickler, if 'Foo' is to be generated
// *before* 'Bar', then 'Bar' would be saved in cache as serializable,
// only to cause exceptions at serialization time.
// this of course is a bug.

type Foo =
    {
        Bar : Bar
        Baz : Baz option // Baz field causes an exception, but only after 'Bar' 
                            // has placed in global cache with a dependency on 'Foo'.
    }

and Bar =
    {
        Foo : Foo option
    }

and [<AutoSerializable(false)>] Baz = class end

[<ReferenceEquality>]
type RefEqualityUnion = RefEq of int

[<AutoSerializable(false)>]
type NonSerializable () = class end

[<AutoSerializable(false); Serializable>]
type SerializableOnAccountOfAttribute () = class end

[<AutoSerializable(true)>]
type SerializableInheritingNonSerializable () =
    inherit NonSerializable()

[<AutoSerializable(false)>]
type NonSerializableInterface = interface end

[<AutoSerializable(true)>]
type SerializableImplementingNonSerializable () =
    interface NonSerializableInterface

[<Struct>]
type SerializableStruct(x : int, y : int) =
    member __.X = x
    member __.Y = y

    static member DeserializedY = 42

    private new (si : SerializationInfo, sc : StreamingContext) =
        let x = si.GetInt32("x")
        new SerializableStruct(x, SerializableStruct.DeserializedY)
            
    interface ISerializable with
        member __.GetObjectData(si : SerializationInfo, sc : StreamingContext) =
            si.AddValue("x", x)

[<Struct; DataContract>]
type StructWithDataContract =
    [<DataMember(Name = "Age", Order = 0)>]
    val private age : int
    [<DataMember(Name = "Age", Order = 1)>]
    val private name : string
    [<DataMember(Name = "Guid", Order = 1)>]
    val private guid : Guid
    val private salary : float

    new (age : int, name : string, guid : Guid, salary : float) =
        { age = age ; name = name ; guid = guid ; salary = salary }

    member __.Age = __.age
    member __.Name = __.name
    member __.Guid = __.guid
    member __.Salary = __.salary


[<AutoSerializable(false)>]
type PicklerFactoryType = class end

[<AutoSerializable(false)>]
type DeclaredSerializableType = class end

type Foo0 = class end
type Foo1 = class end
type Foo2 = class end
type Foo3 = class end
type Foo4 = class end
type Foo5 = class end
type Foo6 = class end
type Foo7 = class end
type Foo8 = class end
type Foo9 = class end

[<AutoSerializable(false)>]type Bar0 = class end
[<AutoSerializable(false)>]type Bar1 = class end
[<AutoSerializable(false)>]type Bar2 = class end
[<AutoSerializable(false)>]type Bar3 = class end
[<AutoSerializable(false)>]type Bar4 = class end
[<AutoSerializable(false)>]type Bar5 = class end
[<AutoSerializable(false)>]type Bar6 = class end
[<AutoSerializable(false)>]type Bar7 = class end
[<AutoSerializable(false)>]type Bar8 = class end
[<AutoSerializable(false)>]type Bar9 = class end

[<AutoSerializable(false)>]type BazBaz0 = class end
[<AutoSerializable(false)>]type BazBaz1 = class end
[<AutoSerializable(false)>]type BazBaz2 = class end
[<AutoSerializable(false)>]type BazBaz3 = class end
[<AutoSerializable(false)>]type BazBaz4 = class end
[<AutoSerializable(false)>]type BazBaz5 = class end
[<AutoSerializable(false)>]type BazBaz6 = class end
[<AutoSerializable(false)>]type BazBaz7 = class end
[<AutoSerializable(false)>]type BazBaz8 = class end
[<AutoSerializable(false)>]type BazBaz9 = class end

type ITest =
    abstract member Int : int

[<AutoSerializable(false)>]
type NonSerializableWithInterface(i) =
    interface ITest with
        member x.Int = i
    member x.Int = i

[<AutoSerializable(false); CollectionDataContract>]
type ClassWithLoneCollectionDataContractAttribute<'T>(xs : 'T list) =
    member __.Values = xs

// automated large-scale object generation
let generateSerializableObjects (assembly : Assembly) =
    let filterType (t : Type) =
        // types that cause .IsSerializable to fail
        // should be included in testing
        try FsPickler.IsSerializableType t with _ -> true

    let tryActivate (t : Type) =
        try Some (t, Activator.CreateInstance t)
        with _ -> None

    // only test things that are successfully serialized by other Picklers
    let filterObject (t : Type, o : obj) =
        FailoverPickler.IsPickleable o
            
    assembly.GetTypes()
    |> Seq.filter filterType
    |> Seq.choose tryActivate
    |> Seq.filter filterObject

/// Generates hash collision data using given serializer, hash and sample data.
/// Returns the distribution of hash collisions.
let getHashCollisions (s : FsPicklerSerializer) (hf : #IHashStreamFactory) (size : int) (factory : int -> 'T) : (int * int) [] =
    Seq.init size (fun i -> s.ComputeHash(factory i, hashFactory = hf))
    |> Seq.countBy id
    |> Seq.countBy snd
    |> Seq.sortBy fst
    |> Seq.map (fun (cs,fs) -> cs - 1, fs)
    |> Seq.toArray