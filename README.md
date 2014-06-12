# FsPickler

A fast, general-purpose serialization framework for .NET written in F#
that doubles as a pickler combinator library.

* Based on the notion of pickler combinators.
* Provides an automated pickler generation framework.
* Offers binary, xml and json pickle formats.
* Support for F# types, quotations, closures and cyclic objects.
* Fully backwards compatible with .NET serialization and open hierarchies.
* One of the [fastest serializers](https://github.com/eiriktsarpalis/FsPickler/wiki/Performance) for the .NET framework.
* Full support for the mono framework.

### Build Status

Head (branch `master`), Build & Unit tests

* Windows/.NET [![Build status](https://ci.appveyor.com/api/projects/status/vwthnxgal50ua8ej/branch/master)](https://ci.appveyor.com/project/nessos/fspickler)
* Mac OS X/Mono 3.2 [![Build Status](https://travis-ci.org/nessos/FsPickler.png?branch=master)](https://travis-ci.org/nessos/FsPickler/branches)

### Basic Usage

The following snippet presents the basic serialization/deserialization API for FsPickler:

```fsharp
open Nessos.FsPickler

// Binary serialization
let bnp = FsPickler.CreateBinary()
bnp.Serialize<int option list>(stream, [Some 1; None; Some -1])
bnp.Deserialize<int option list>(stream)

// Xml serialization
let xmp = FsPickler.CreateXml()
xmp.Serialize<int option list>(stream, [Some 1; None; Some -1])

// Json serialization
let jsp = FsPickler.CreateJson()
jsp.Serialize<int option list>(stream, [Some 1; None; Some -1])
```

All generated picklers are strongly typed; pickling is performed efficiently
without intermediate boxings. Picklers are generated at runtime using reflection
and dynamic methods which are cached for future use.

### Pickler Combinators

FsPickler offers experimental support for generating user-defined picklers using combinators:

```fsharp
open Nessos.FsPickler
open Nessos.FsPickler.Combinators

let p : Pickler<int * string option> = 
    Pickler.string 
    |> Pickler.option 
    |> Pickler.pair Pickler.int
    
let pickle : string = Json.pickle p (42, Some "42") // {"item1":42,"item2":{"Some":"42"}}
Json.unpickle p pickle
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
                        
Succ (Succ Zero) |> Binary.pickle pp |> Binary.unpickle pp
```
The library comes with ``array``, ``array2D``, ``list``, ``set`` and ``map`` 
combinators that are used to build picklers for the corresponding generic types.
The ``Pickler.seq`` combinator generates picklers for sequences using eager evaluation of elements.
Finally, ``Pickler.func<'a,'b>`` : ``Pickler<('a -> 'b)>`` instantiates picklers for F# functions.

When it comes to generic types, picklers can be defined using user-defined combinators:

```fsharp
type Tree<'T> = Leaf | Node of 'T * Tree<'T> list

let tree (ep : Pickler<'T>) =
    Pickler.fix(fun tree ->
        tree
        |> Pickler.list
        |> Pickler.pair ep
        |> Pickler.option
        |> Pickler.wrap (function None -> Leaf | Some (t,c) -> Node(t,c))
                        (function Leaf -> None | Node (t,c) -> Some(t,c)))
                        
Node(2,[]) |> Xml.pickle (tree Pickler.int)
```
or it could be done using automatic resolution of type parameters:

```fsharp
let tree<'T> =
    Pickler.fix(fun tree ->
        tree
        |> Pickler.list
        |> Pickler.pair Pickler.auto<'T>
        |> Pickler.option
        |> Pickler.wrap (function None -> Leaf | Some (t,c) -> Node(t,c))
                        (function Leaf -> None | Node (t,c) -> Some(t,c)))


Node([1],[Leaf ; Leaf]) |> Json.pickle tree
```

### Experimental N-way Sum and Product Combinators

N-way sum and product combinators provide an alternative pretty syntax for
defining picklers over arbitrary discriminated unions and records.  Unfortunately
at the moment the performance of resulting picklers is sub-optimal, this might
improve in the future.

The types involved in the examples are not fit for human consumption, but thankfully
F# infers them automatically. The implementation is total and purely functional,
see this [gist][nway] for some Coq code used to model these combinators.

[nway]: https://gist.github.com/t0yv0/6834822

#### Records / Product Types
 
```fsharp    
type Person =
    {
        Address : string
        Age : int
        Name : string
    }

let makePerson name age address =
    {
        Address = address
        Age = age
        Name = name
    }

let personPickler =
    Pickler.product makePerson
    ^+ Pickler.field (fun p -> p.Name) Pickler.string
    ^+ Pickler.field (fun p -> p.Age) Pickler.int
    ^. Pickler.field (fun p -> p.Address) Pickler.string
```

#### Unions / Sum Types

```fsharp
type U =
| Case1
| Case2 of int
| Case3 of string * int

let uPickler =
    Pickler.sum (fun x k1 k2 k3 ->
        match x with
        | Case1 -> k1 ()
        | Case2 x -> k2 x
        | Case3 (x, y) -> k3 (x, y))
    ^+ Pickler.variant Case1
    ^+ Pickler.case Case2 Pickler.int
    ^. Pickler.case Case3 (Pickler.pair Pickler.string Pickler.int)
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

### Structural Hashcodes

FsPickler offers experimental support for structural, non-cryptographic, hashcode generation:
```fsharp
> FsPickler.ComputeHash [1 .. 100000] ;;
val it : HashResult =
  {Algorithm = "MurMur3";
   Length = 400008L;
   Hash =
    [|52uy; 70uy; 141uy; 214uy; 3uy; 231uy; 11uy; 100uy; 94uy; 250uy; 231uy;
      97uy; 188uy; 215uy; 70uy; 0uy|];}
```
This will generate a 128-bit structural hashcode based on the [MurMurHash](http://en.wikipedia.org/wiki/MurmurHash) algorithm.
Implementation is memory efficient, since the hashing algorithm is integrated with
the underlying stream implementation. It is possible for users to define their own hashing
algorithms by inheriting the special `HashStream` class. 

Hashing functionality offered by FsPickler is an ideal replacement to `.GetHashCode()` for
large objects or complex object graphs, but it is not recommended for small values or primitives.

If a hashcode is not required, the size of an object alone can be computed as follows:
```fsharp
> FsPickler.ComputeSize [1 .. 1000000] ;;
val it : int64 = 4000008L
```

### Object Graph Visitors

FsPickler offers experimental support for so-called object graph visitors.
An `IObjectVisitor` is a simple interface:
```fsharp
type IObjectVisitor =
  interface
    abstract member Visit : 'T -> unit
  end
```
FsPickler is capable of efficiently traversing arbitrary object graphs (as long as they are serializable)
by exploiting its pickler infrastructure. This can be done by calling the method:
```fsharp
static member FsPickler.VisitObject : visitor:IObjectVisitor * graph:obj -> unit
```
A couple of useful applications are provided by the core library:
```fsharp
> let types = FsPickler.GatherTypesInObjectGraph [box 42 ; box (Some (42, "42")) ] ;;

val types : System.Type [] =
  [|Microsoft.FSharp.Collections.FSharpList`1[System.Object]; System.Int32;
    Microsoft.FSharp.Core.FSharpOption`1[System.Tuple`2[System.Int32,System.String]];
    System.Tuple`2[System.Int32,System.String]|]
```

### Defining Custom Pickle Formats

It is possible to create user-defined pickle formats for FsPickler. One simply needs to implement the interface
```fsharp
type IPickleFormatProvider =
  interface
    abstract member Name : string
    abstract member CreateReader : Stream * Encoding * leaveOpen:bool -> IPickleFormatReader
    abstract member CreateWriter : Stream * Encoding * leaveOpen:bool -> IPickleFormatWriter
  end
```
which can then be bolted on a class that inherits either of the `BasePickler` or `TextPickler` types.
