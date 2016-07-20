(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

#r "Newtonsoft.Json.dll"
#r "FsPickler.dll"
#r "FsPickler.Json.dll"

let stream = Unchecked.defaultof<System.IO.Stream>
let serializer = Unchecked.defaultof<Nessos.FsPickler.FsPicklerSerializer>
let textWriter = Unchecked.defaultof<System.IO.TextWriter>
let textReader = Unchecked.defaultof<System.IO.TextReader>

(**

# An Introduction to FsPickler

The following provides an overview of the basic functionalities offered by the library.

## The core serialization API

The basic API is accessible through instances of type [`FsPicklerSerializer`](reference/nessos-fspickler-fspicklerserializer.html)
that can be initialized as follows:

*)

#r "FsPickler.dll"

open Nessos.FsPickler

let binarySerializer = FsPickler.CreateBinarySerializer()
let xmlSerializer = FsPickler.CreateXmlSerializer(indent = true)

(**

Json serialization formats can be accessed by referencing the `FsPickler.Json` project.
If evaluating from F# interactive, make sure to [add an explicit reference to Json.Net](https://github.com/nessos/FsPickler/issues/16).

*)

#r "Newtonsoft.Json.dll"
#r "FsPickler.Json.dll"

open Nessos.FsPickler.Json

let jsonSerializer = FsPickler.CreateJsonSerializer(indent = false)
let bsonSerializer = FsPickler.CreateBsonSerializer()

(**

A simple serialization/deserialization roundtrip can be performed as follows:

*)

xmlSerializer.Serialize(stream, [0. .. 0.1 .. 1.])
xmlSerializer.Deserialize<float list>(stream)

(**

FsPickler instances can be used to produce binary pickles, 
that is byte arrays containing the serialized values:

*)

let pickle = binarySerializer.Pickle <@ 1 + 1 @>
binarySerializer.UnPickle<Quotations.Expr<int>> pickle

(**

### Text-based serialization

The Xml and Json serializers are instances of type [`FsPicklerTextSerializer`](reference/nessos-fspickler-fspicklertextserializer.html)
that offers functionality for text-based serialization:

*)

xmlSerializer.Serialize(textWriter, [ Some 1 ; Some 2 ; None ])
xmlSerializer.Deserialize<int option list>(textReader)

let text = jsonSerializer.PickleToString (fun x -> x + 1)
jsonSerializer.UnPickleOfString<int -> int> text

(**

### Sequence serialization

FsPickler offers support for on-demand sequence serialization/deserialization:

*)

let seq = Seq.initInfinite string |> Seq.take 100

let length = binarySerializer.SerializeSequence(stream, seq) // returns the length of serialized elements
let seq' = binarySerializer.DeserializeSequence<int>(stream) // lazy deserialization IEnumerable
Seq.toArray seq' // evaluation forces full deserialization

(**

## Picklers and Pickler combinators

A pickler is essentially the type

*)

type Pickler<'T> =
    {
        Write : WriteState -> 'T -> unit
        Read  : ReadState  -> 'T
    }

(*** hide ***) 
open Nessos.FsPickler

(**

which defines the serialization/deserialization rules for a given type.
Picklers are strongly typed and perform serialization without reflection
or intermediate boxings.

There are two kinds of picklers:

 * Primitive or atomic picklers that are self-contained definitions 
   for simple values like primitives, strings or timespans.

 * Composite picklers which are derived from composition of simpler types.
   They are generated using *pickler combinators*, functions taking a collection
   of picklers as inputs yielding a composite result.

FsPickler is essentially an automated pickler generation framework: picklers are generated at runtime
and on demand using a combination of reflection and dynamic IL generation. Picklers are cached for 
future use, hence the cost of generation has a constant price.

Moreover, the library provides an experimental combinator module that allows direct manipulation
of picklers in a more functional style:

*)

open Nessos.FsPickler.Combinators

// atomic picklers
let int = Pickler.int
let string = Pickler.string
let dateTime = Pickler.dateTime

// composite picklers
let p1 = Pickler.pair int dateTime
let p2 = Pickler.option string
let p3 = Pickler.list p2
let p4 = Pickler.array2D p1
let p5 = Pickler.auto<int * string option list>

// pickler-based serialization
let t1 = Binary.pickle int 42
let t2 = Json.pickle p3 [ Some "" ; None ; Some "message" ]

Json.unpickle p3 t2

(**

The module includes all primitive combinators as described in Andrew Kennedy's 
[Pickler Combinators](http://research.microsoft.com/en-us/um/people/akenn/fun/picklercombinators.pdf)
like``wrap`` and ``alt``. Fixpoint combinators for declaring recursive picklers are also available:

*)

type Peano = Zero | Succ of Peano

let pp : Pickler<Peano> =
    Pickler.fix(fun peano ->
        peano
        |> Pickler.option
        |> Pickler.wrap (function None -> Zero | Some p -> Succ p)
                        (function Zero -> None | Succ p -> Some p))
                        
Succ (Succ Zero) |> Binary.pickle pp |> Binary.unpickle pp

(**

When it comes to generic types, picklers can be created through user-defined combinators:

*)

type Tree<'T> = Leaf | Node of 'T * Tree<'T> list

let mkTree (ep : Pickler<'T>) =
    Pickler.fix(fun tree ->
        tree
        |> Pickler.list
        |> Pickler.pair ep
        |> Pickler.option
        |> Pickler.wrap (function None -> Leaf | Some (t,c) -> Node(t,c))
                        (function Leaf -> None | Node (t,c) -> Some(t,c)))
                        
Node(2,[]) |> Xml.pickle (mkTree Pickler.int)

(**

or it could be done using automatic resolution of type parameters:

*)

let tree<'T> =
    Pickler.fix(fun tree ->
        tree
        |> Pickler.list
        |> Pickler.pair Pickler.auto<'T>
        |> Pickler.option
        |> Pickler.wrap (function None -> Leaf | Some (t,c) -> Node(t,c))
                        (function Leaf -> None | Node (t,c) -> Some(t,c)))


Node([1],[Leaf ; Leaf]) |> Json.pickle tree

(**

### SerializationInfo Picklers

It is possible to define picklers that serialise objects using [SerializationInfo](https://msdn.microsoft.com/en-us/library/system.runtime.serialization.serializationinfo%28v=vs.110%29.aspx).
For example, consider the record:

*)

type Name = { FirstName : string ; MiddleName : string option ; Surname : string }

(**

We can define a SerializationInfo based pickler using the following combinator:

*)

let nameP =
    Pickler.fromSerializationInfo
                (fun si -> 
                    { FirstName = si.Get "First Name"
                      MiddleName = si.TryGet "Middle Name"
                      Surname = si.Get "Last Name" })
                (fun si p -> 
                    si.Add("First Name", p.FirstName)
                    si.Add("Last Name", p.Surname)
                    match p.MiddleName with Some mn -> si.Add("Middle Name", mn) | None -> ())

(**

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
*)

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

(**

#### Unions / Sum Types

*)

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

(**

## Generating custom picklers in type definitions

FsPickler can be instructed to use custom pickler definitions for given types
using the following design pattern:

*)

[<CustomPickler>]
type CustomClass<'T, 'S> (x : 'T, y : 'S) =

    member __.X = x
    member __.Y = y

    static member CreatePickler (resolver : IPicklerResolver) =
        let xp = resolver.Resolve<'T> ()
        let yp = resolver.Resolve<'S> ()

        let writer (ws : WriteState) (c : CustomClass<'T,'S>) =
            xp.Write ws "X" c.X
            yp.Write ws "Y" c.Y

        let reader (rs : ReadState) =
            let x = xp.Read rs "X"
            let y = yp.Read rs "Y"
            new CustomClass<_,_>(x,y)

        Pickler.FromPrimitives(reader, writer)

(**

This tells FsPickler to generate a pickler for the given type using
that particular factory method. It should be noted that the ``Read``/``Write``
operations are not commutative, hence care should be taken so that ordering is matched. 
The ``IPicklerResolver`` argument provides a handle to the pickler generator and can be used recursively:

*)

[<CustomPickler>]
type RecursiveClass(?nested : RecursiveClass) =

    member __.Value = nested

    static member CreatePickler (resolver : IPicklerResolver) =
        let self = resolver.Resolve<RecursiveClass> ()
        self
        |> Pickler.option 
        |> Pickler.wrap (fun x -> RecursiveClass(?nested = x)) (fun rc -> rc.Value)

// a pickler will be generated based on the above definition
let p = FsPickler.GeneratePickler<RecursiveClass> ()

RecursiveClass(RecursiveClass()) |> Json.pickle p |> Json.unpickle p

(**

## Runtime pickler registration

As of FsPickler 1.2.5 it is possible to register custom pickler definitions at runtime.
Consider a type declaration that has not been made serializable:

*)

[<AutoSerializable(false)>]
type NonSerializable(value : int) =
    member __.Value = value

(**

It is now possible to declare and register a pickler factory at a separate location

*)

let mkPickler (resolver : IPicklerResolver) =
    let intP = resolver.Resolve<int> ()

    let writer (w : WriteState) (ns : NonSerializable) =
        intP.Write w "value" ns.Value

    let reader (r : ReadState) =
        let v = intP.Read r "value" in new NonSerializable(v)

    Pickler.FromPrimitives(reader, writer) 

FsPickler.RegisterPicklerFactory mkPickler

(**

When resolving a pickler of this type in the future, the user-supplied implementation will be used:

*)

let pickler = FsPickler.GeneratePickler<NonSerializable> ()

pickler.PicklerInfo // PicklerInfo.UserDefined

(**

Note that factory registrations need to be performed *before* FsPickler uses their types at all in the current AppDomain:

```text
System.InvalidOperationException: A pickler for type 'FSI_0004+NonSerializable' has already been generated.
   at <StartupCode$FsPickler>.$FsPickler.RegisterPicklerFactory@54.Invoke(Unit unitVar0) in c:\Users\eirik\Development\nessos\FsPickler\src\FsPickler\FsPickler\FsPickler.fs:line 60
   at <StartupCode$FsPickler>.$PicklerCache.f@1-2(PicklerCache c, FSharpFunc`2 f, Unit unitVar0) in c:\Users\eirik\Development\nessos\FsPickler\src\FsPickler\PicklerGeneration\PicklerCache.fs:line 73
```

If all that needs to be done is declare to FsPickler the intention of treating a type as serializable,
we can avoid the hassle of defining a full-blown custom pickler as follows:

*)

FsPickler.DeclareSerializable<NonSerializable> ()

(**

## Additional tools

This section describes some of the additional tools offered by the library:

### Object Cloning

FsPickler 1.2 adds support for fast cloning of serializable objects. 
This is done in a node-per-node basis, without the need for serialization formats and intermediate buffers.

*)

let clonedValue = FsPickler.Clone [Choice1Of2 "foo" ; Choice2Of2 ['b';'a';'r']]

(**

### Structural Hashcodes

FsPickler offers experimental support for structural, non-cryptographic, hashcode generation:

*)

let hash = FsPickler.ComputeHash [1 .. 100000]

//val it : HashResult =
//  {Algorithm = "MurMur3";
//   Length = 400008L;
//   Hash =
//    [|52uy; 70uy; 141uy; 214uy; 3uy; 231uy; 11uy; 100uy; 94uy; 250uy; 231uy;
//      97uy; 188uy; 215uy; 70uy; 0uy|];}

(**

This will generate a 128-bit structural hashcode based on the [MurMurHash](http://en.wikipedia.org/wiki/MurmurHash) algorithm.
Implementation is memory efficient, since the hashing algorithm is integrated with
the underlying stream implementation. It is possible for users to define their own hashing
algorithms by inheriting the special `HashStream` class. 

Hashing functionality offered by FsPickler is an ideal replacement to `.GetHashCode()` for
large objects or complex object graphs, but it is not recommended for small values or primitives.

If a hashcode is not required, the size of an object alone can be computed as follows:

*)

FsPickler.ComputeSize [1 .. 1000000]

//val it : int64 = 4000008L

(**

### Typed Serialization

It is possible to create typed picklings of objects:

*)

let typedPickle = jsonSerializer.PickleTyped [1 .. 1000]

(**

this will produce a serialization annotated with the type of the original object.
They can then be easily deserialized like so:

*)

let value = jsonSerializer.UnPickleTyped typedPickle

(**
### Object Sifting

FsPickler 1.2 comes with a 'sifting' functionality which allows serialization/cloning while omitting specified
instances from an object graph. For example, consider the object graph

*)

let small = [|1..10|]
let large = [|1..100000000|]
let graph = Some [small; large; small; large]

(**

The size of the object becomes evident when running

*)

FsPickler.ComputeSize graph

//val it : int64 = 400000203L

(**

Supposing we knew that the size of the graph was being bloated by large arrays,
we can use FsPickler to optimize serialization by sifting away occurences from the object graph.

*)

let sifter : obj -> bool = function :? (int []) as ts -> ts.Length > 100 | _ -> false
let siftedGraph, siftedValues = FsPickler.Sift(graph, sifter)

//val siftedValues : (int64 * obj) [] =
//  [|(4L,
//     [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
//       21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38;
//       39; 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56;
//       57; 58; 59; 60; 61; 62; 63; 64; 65; 66; 67; 68; 69; 70; 71; 72; 73; 74;
//       75; 76; 77; 78; 79; 80; 81; 82; 83; 84; 85; 86; 87; 88; 89; 90; 91; 92;
//       93; 94; 95; 96; 97; 98; 99; 100; ...|])|]
//
//val siftedGraph : Sifted<int [] list option> =
//  Sift: Some
//  [[|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|]; null; [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|];
//   null]

(**

This will return a sifted clone of the original object as well as a collection of all objects that were sifted
from the original input. The sifted copy encapsulated in a wrapper type so that it cannot be consumed while in its partial state.
The original input graph will not be mutated in any way. We can verify that the size of the sifted object has been reduced:

*)

FsPickler.ComputeSize siftedGraph

//val it : int64 = 270L

(**

Sifted objects can be put back together by calling

*)

let reconstructed = FsPickler.UnSift(siftedGraph, siftedValues)

(**

### Object Graph Visitors

FsPickler is capable of efficiently traversing arbitrary object graphs (as long as they are serializable)
by exploiting its pickler infrastructure. This can be done by calling the method:

*)

FsPickler.VisitObject

(**

which takes as input a serializable object graph and a so-called object visitor:

*)

type IObjectVisitor =
  interface
    abstract member Visit : Pickler<'T> * 'T -> bool
  end

(*** hide ***)
open Nessos.FsPickler
open System.IO
open System.Text

(**

A few applications of this are provided by the core library:

*)

let types = FsPickler.GatherTypesInObjectGraph [box 42 ; box (Some (42, "42"))]

//val types : Type [] =
//  [|Microsoft.FSharp.Collections.FSharpList`1[System.Object]; System.Int32;
//    Microsoft.FSharp.Core.FSharpOption`1[System.Tuple`2[System.Int32,System.String]];
//    System.Tuple`2[System.Int32,System.String]; System.String|]

(**

### Disabling Subtype Resolution

For security reasons, it might often be desirable to disable subtype resolution
when serializing classes:

*)

serializer.DisableSubtypeResolution <- true

(**

This essentially disables the serialization of any object whose declaring type
is specified on the serialization itself. Attempting to serialize or deserialize
any such object will result in an exception.

Note that enabling this option prevents serialization of the following types:
  
  * `System.Object` or any abstract class (excluding F# DUs).
  * Any delegate instance or F# function.
  * Any ISerializable class.

As a further precaution, it is also possible to disable implicit assembly loading
when deserializing objects:

*)

serializer.DisableAssemblyLoading <- true

(**

## Defining Custom Pickle Formats

It is possible to create user-defined pickle formats for FsPickler. One simply needs to implement the interface

*)

type IPickleFormatProvider =
  interface
    abstract member Name : string
    abstract DefaultEncoding : Encoding
    abstract member CreateReader : Stream * Encoding * leaveOpen:bool -> IPickleFormatReader
    abstract member CreateWriter : Stream * Encoding * leaveOpen:bool -> IPickleFormatWriter
  end

(**
which can then be bolted on a class that inherits either of the 
[`FsPicklerSerializer`](reference/nessos-fspickler-fspicklerserializer.html) or 
[`FsPicklerTextSerializer`](reference/nessos-fspickler-fspicklertextserializer.html).
*)