# FsPickler

A fast, general-purpose binary serializer for .NET written in F# 
that doubles as a pickler combinator library.

* Based on the notion of pickler combinators.
* Provides an automated, strongly typed, pickler generation framework.
* Full support for .NET types, including classes and open hierarchies.
* Compatible with all serializable types, including the ``ISerializable`` interface.
* Highly optimized for F# core types.
* Performance about 5-20x faster than the default .NET serializers.
* Full support for the mono framework.

Get the NuGet package [here](https://www.nuget.org/packages/FsPickler/).

### Basic Usage

The following snippet presents the basic serialization/deserialization API for FsPickler:

```fsharp
open FsPickler

let fsp = new FsPickler()

// typed serialization
fsp.Serialize<int list option>(stream, Some [1; 2; 3])
fsp.Deserialize<int list option>(stream)

// typed serialization with explicit pickler use
let pickler : Pickler<int list option> = fsp.GeneratePickler<int list option> ()

fsp.Serialize(pickler, stream, Some [1; 2; 3])
fsp.Deserialize(pickler, stream) : int list option

// untyped serialization
fsp.Serialize(typeof<int>, stream, 42)
fsp.Deserialize(typeof<int>, stream) : obj

// untyped serialization with explicit pickler use
let pickler' : Pickler = fsp.GeneratePickler typeof<int>

fsp.Serialize(pickler', stream, 42)
fsp.Deserializer(pickler', stream) : obj
```

All generated picklers are strongly typed; pickling is performed efficiently
without intermediate boxings. Picklers are generated at runtime using reflection
and expression trees that are aggressively cached for future use.

### Pickler Combinators

FsPickler offers experimental support for generating user-defined picklers using combinators:

```fsharp
open FsPickler
open FsPickler.Combinators

let p : Pickler<int * string option> = 
    Pickler.string 
    |> Pickler.option 
    |> Pickler.pair Pickler.int
    
let data : byte [] = pickle p (42, Some "")
unpickle p data
```

The combinator library includes all primitives as described in Andrew Kennedy's 
[Pickler Combinators](http://research.microsoft.com/en-us/um/people/akenn/fun/picklercombinators.pdf)
such as ``wrap`` and ``alt``. Fixpoint combinators for declaring recursive picklers are also available:
```fsharp
// val Pickler.fix : (Pickler<'T> -> Pickler<'T>) -> Pickler<'T>

type Peano = Zero | Succ of Peano

let pp : Pickler<Peano> =
    Pickler.fix(fun peano ->
        peano
        |> Pickler.option
        |> Pickler.wrap (function None -> Zero | Some p -> Succ p)
                        (function Zero -> None | Succ p -> Some p))
                        
Succ (Succ Zero) |> pickle pp |> unpickle pp
```
The library comes with ``array``, ``array2D``, ``list``, ``set`` and ``map`` 
combinators that are used to build picklers for the corresponding generic types.
The ``Pickler.seq`` combinator generates picklers for sequences using eager evaluation of elements.
Finally, ``Pickler.func<'a,'b>`` : ``Pickler<('a -> 'b)>`` instantiates picklers for F# functions.

When it comes to generic types, picklers can be defined using user-defined combinators:

```fsharp
type BinTree<'T> = Leaf | Node of 'T * BinTree<'T> list

let binTree (ep : Pickler<'T>) =
    Pickler.fix(fun tree ->
        tree
        |> Pickler.list
        |> Pickler.pair ep
        |> Pickler.option
        |> Pickler.wrap (function None -> Leaf | Some (t,c) -> Node(t,c))
                        (function Leaf -> None | Node (t,c) -> Some(t,c)))
                        
Node(2,[]) |> pickle (binTree Pickler.int)
```
or it could be done using automatic resolution of type parameters:

```fsharp
let binTree<'T> =
    Pickler.fix(fun tree ->
        tree
        |> Pickler.list
        |> Pickler.pair Pickler.auto<'T>
        |> Pickler.option
        |> Pickler.wrap (function None -> Leaf | Some (t,c) -> Node(t,c))
                        (function Leaf -> None | Node (t,c) -> Some(t,c)))


Node([1],[Leaf ; Leaf]) |> pickle binTree
```

### Custom Pickler Declarations

Since F# lacks mechanisms such as type classes or implicits, 
pickler resolution can only be performed by means of reflection.
This is done using the following design pattern:

```fsharp
[<CustomPickler>]
type CustomClass<'T> (x : 'T) =

    member __.Value = x

    static member CreatePickler (resolver : IPicklerResolver) =
        let ep = resolver.Resolve<'T> ()
        ep |> Pickler.wrap (fun x -> CustomClass(x)) (fun c -> c.Value)
```
This tells the pickler generator to yield a pickler for the given type
using that particular factory method. The ``IPicklerResolver`` argument provides
a handle to the pickler generator and can be used for recursive types:
```fsharp
[<CustomPickler>]
type RecursiveClass(?nested : RecursiveClass) =

    member __.Value = nested

    static member CreatePickler (resolver : IPicklerResolver) =
        let self = resolver.Resolve<RecursiveClass> ()
        self 
        |> Pickler.option 
        |> Pickler.wrap (fun x -> RecursiveClass(?nested = x)) (fun rc -> rc.Value)


let p = Pickler.auto<RecursiveClass>

RecursiveClass(RecursiveClass()) |> pickle p |> unpickle p
```

### Pluggable Picklers

When in need to define custom serialization rules for elsewhere-defined types, 
a pluggable pickler mechanism is provided. 
Supposing the following custom ``Pickler<int option>``:
```fsharp
let customOptional = Pickler.FromPrimitives((fun _ -> Some 42), fun _ _ -> ())
```
how could one plug it into the pickler generator?
```fsharp
let registry = new CustomPicklerRegistry("pickler cache with custom optionals")

do registry.RegisterPickler customOptional

let fsp' = new FsPickler(registry)
```
The above initializes a *new* pickler cache; all picklers generated from that cache
will adhere to the custom rules:
```fsharp
let optionlist = fsp'.GeneratePickler<int option list> ()

// outputs a list of Some 42's
[ for i in 1 .. 100 -> Some i ] |> fsp.Pickle optionlist |> fsp.UnPickle optionlist
```
Note that auto-generated picklers do not support interop between caches:
```fsharp
let pickler = fsp.GeneratePickler<int list> ()

fsp'.Pickler pickler [1] // runtime error
```

Care should be taken that custom ``FsPickler`` instances are treated as singletons;
generating arbitrarily many instances might put strain on the runtime, 
since each will initialize a unique cache of picklers and emitted code.

### Pluggable Generic Picklers

Defining pluggable picklers for generic types is a bit more involved, 
and requires the use of special factory methods that need to be implemented
as instances of the ``IPicklerFactory`` interface. The interface itself
does not include any methods but instances need to have implemented a
factory method with type signature 
```fsharp
Create<'T1,...,'Tn when [constraints]> : IPicklerResolver -> Pickler
```
For instance, supposing I need to define a custom pickler for F# sets:
```fsharp
type CustomSet () =
    interface IPicklerFactory

    member __.Create<'T when 'T : comparison> (resolver : IPicklerResolver) =
        let ep = resolver.Resolve<'T> ()

        ep |> Pickler.wrap (fun x -> set [x]) (fun s -> Seq.head s)
        
do customPicklers.RegisterPicklerFactory (new CustomSet())

let fsp'' = new FsPickler(customPicklers)
```
I can now combine this rather bad pickler implementation as expected:
```fsharp
let p = fsp''.GeneratePickler<Set<int> option> ()

// will return singleton set
Some(set [ 1 .. 1000 ]) |> fsp.Pickle p |> fsp.UnPickle p
```

### Future work

* Support for fast structural hashcode generation.
* Support for multiple pickle formats.
* A C# friendly API.
