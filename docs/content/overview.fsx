(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

#r "Newtonsoft.Json.dll"
#r "FsPickler.dll"
#r "FsPickler.Json.dll"

open System
open Nessos.FsPickler
open Nessos.FsPickler.Combinators

type SomeType = class end

(**

# Technical Overview

This article discusses the general topic of serialization in .NET
and gives an overview of the implementation details in FsPickler.

## Serialization in the .NET framework

Serialization in the .NET framework is something often considered
as being problematic or even broken. This could be tracked to the following reasons:

  * There is no runtime or type system support for serialization. 
    Rather, serializers are library implementations that mostly depend
    on the reflection system to extrapolate serialization rules for
    each type (resulting in runtime errors if this is not possible).

  * There is confusion as to which is the prefered methodology when
    defining serialization semantics for types.
    The BCL itself comes with a multitude of approaches most of which are 
    dated, unsafe and inefficient by design.

### BCL Serialization methods

The BCL comes with the following patterns for defining serializable types:

  * Field-based serialization: objects are pickled by serializing the contents 
    of their fields. This behaviour is tuned by applying an assortment of attributes such as 
    [`NonSerialized`](http://msdn.microsoft.com/en-us/library/system.nonserializedattribute.aspx), 
    [`OnSerializing`](http://msdn.microsoft.com/en-us/library/system.runtime.serialization.onserializingattribute.aspx), 
    [`OnDeserialized`](http://msdn.microsoft.com/en-us/library/system.runtime.serialization.ondeserializedattribute.aspx) 
    and the [`IDeserializationCallback`](http://msdn.microsoft.com/en-us/library/system.runtime.serialization.ideserializationcallback.aspx) 
    interface. This is arguably the most problematic approach, since it is sensitive to internal implementation details.
    Incidentally, this is the pattern of choice for most F# types.

  * Property-based or [DataContract](http://msdn.microsoft.com/en-us/library/ms733127.aspx) 
    serialization: objects are pickled by serializing the contents
    of specially flagged properties. Properties should be settable and are usually required to be
    public. In most cases, a parameterless constructor is also necessary. Property-based serialization
    is probably the most popular approach.

  * [ISerializable](http://msdn.microsoft.com/en-us/library/system.runtime.serialization.iserializable.aspx) 
    types: possibly intended as a type-safe replacement of field-based serialization.
    It fails to live up to this role, mostly due to the nature of interfaces; it still requires a
    special constructor for deserialization which, if missing, would result in a runtime error.
    It requires separate implementations for serialization and deserialization, often leading to bugs
    and introducing lots of boilerplate code.
    Its dynamic nature also makes it impossible to reason about the components it serializes a priori.
    It is however an indispensible pattern since it is used by many BCL types, most important being
    exceptions, whose metadata is impossible to serialize otherwise.

### Serialization Libraries

The BCL ships with a couple of general-purpose serializer libraries but these suffer either
from obsolescence (e.g. [BinaryFormatter](http://msdn.microsoft.com/en-us/library/system.runtime.serialization.formatters.binary.binaryformatter.aspx)) 
or performance ([NetDataContractSerializer](http://msdn.microsoft.com/en-us/library/system.runtime.serialization.netdatacontractserializer.aspx)).

Many third-party libraries have emerged attempting to address these shortcomings, 
some with success. However, most of them seem to suffer when it comes to properly
supporting serialization of .NET objects. In particular many of them:

  * offer sketchy or no support for reflection types, like `MemberInfo`.

  * require library-specific dependencies for serializable type definitions.

  * do not support inaccessible types/constructors/properties.

  * fail to address subtype polymorphism.

  * do not support types like multi-dimensional arrays or delegates.

  * fail to properly handle cyclic objects.

  * have limited or nonexistent support for F# types.

## FsPickler : Motivation

The need for a new serialization library was realized while developing for 
[MBrace](http://nessos.github.io/MBrace/). MBrace is a framework for distributed
computation and big data that is essentially built on the idea of a distributed
continuation monad. To be able to distribute continuations, you need to be able to
serialize closures. The inherently intrinsic and arbitrary nature of closures means 
that they are not supported by most serialization libraries.
Initially we started using NetDataContractSerializer as our library of choice,
but that eventually took its toll on overall performance,
so we decided to build a new library from scratch.

FsPickler was created with two goals in mind: performance and completeness in supported objects.
Importantly, it was conceived as a .NET serializer, rather than an F# serializer:
if it is a .NET object designed to be serialized, it should be serializable in FsPickler.
In that sense, it was designed with a goal to embrace the imperfect world that is
the .NET framework in its totality, albeit with an eye for correctness.

At this point, we should acknowledge Anton Tayanovskyy and his great F#
snippet [Union-Friendly Generic Binary Serializer](http://www.fssnip.net/6u)
which served as the initial inspiration for this library.

## Pickler Combinators

FsPickler is founded on the functional programming notion of picklers and pickler combinators. 
The concept was originally described in Andrew Kennedy's 
[Pickler Combinators](http://research.microsoft.com/pubs/64036/picklercombinators.pdf).
A pickler is essentially the type:

*)

type Pickler<'T> =
    {
        Write : WriteState -> 'T -> unit
        Read  : ReadState  -> 'T
    }

(**

The `Write` and `Read` functions represent serialization and deserialization 
implementations for values of type `'T`. When compared to other .NET serializers, 
picklers prove exceptionally fast since they directly handle the serialized value
without any reliance on reflection-based resolution.

Pickler combinators are functions that operate on picklers, generating picklers of more complex types. 
For instance, the following could be a pickler combinator implementation for rank-1 arrays:

*)

let mkArrayPickler (ip : Pickler<int>) (tp : Pickler<'T>) : Pickler<'T []> =
    {
        Write = fun s a -> ip.Write s a.Length ; Array.iter (tp.Write s) a
        Read = fun s -> let l = ip.Read s in Array.init l (fun _ -> tp.Read s)
    }

(***hide***)
// mask the dummy pickler implementation
open Nessos.FsPickler

(**

Given a sufficient range of primitive picklers and pickler combinators,
it is possible to define pickling rules for most essentially serializable .NET types. 
However, these would still need to be declared manually by their implementor.

FsPickler attempts to solve this by providing an automated pickler generation framework: 
picklers are generated at runtime and on demand using a combination of reflection and 
dynamic IL generation. Picklers are cached for future use, hence the cost of generation 
has a constant price.

Moreover, it extends the concept of picklers so that features particular to the
.NET framework are accommodated: object orientation and subtype polymorphism, 
null references and cyclic objects.

It should be mentioned for the sake of completeness that a 
[similar effort](http://lampwww.epfl.ch/~hmiller/files/pickling.pdf) 
has been undertaken by the Scala community. There are differences of approach however, 
since this mostly relies on the metaprogramming facility offered by scala.

## Serializable Types

So what qualifies as a serializable type in FsPickler?
The short answer would be any type `'T` for which we present an instance of 
[`Pickler<'T>`](reference/nessos-fspickler-pickler-1.html).
To make the question more meaningful: for what types does the library auto-generate picklers? 
This is something that can only be answered at runtime, typically by calling

*)

FsPickler.IsSerializableType<SomeType> ()

(**

Primitive types and `string` are serializable but pointers, COM objects and MarshalByRef types are not.
A managed class or struct is serializable if and only if it carries the `Serializable` attribute
and moreover satisfies either of the following:

  * is array/nullable/tuple/enum of serializable element types.

  * implements the `ISerializable` interface and has a matching constructor implementation.

  * carries the `DataContract` attribute and all designated properties are of 
    serializable type and settable. Parameterless constructors or public properties are not required.

  * carries the [`CustomPickler`](reference/nessos-fspickler-custompicklerattribute.html) attribute and an accompanying pickler factory method.

  * is an F# union type in which all union case fields are serializable.

  * is abstract or a delegate.

  * is neither of the above and all designated fields are of serializable type.
    Parameterless constructors or public fields are not required.

## Object Oriented Picklers

We now discuss how the functional programming concept of picklers can be 
reconciled with the object-oriented .NET framework.
It is useful to note here that reference type serialization essentially
means object graph serialization.
Object graphs in .NET can often contain null references or cycles,
which ought to be fully supported.
FsPickler automatically augments reference types so that such
occurences are handled effectively.

Object graphs are serialized by performing depth-first traversal; 
this approach improves performance by eliminating unnecessary
boxings in object queues but is sensitive to stack overflow conditions.
FsPickler uses
[`ObjectIdGenerator`](http://msdn.microsoft.com/en-us/library/system.runtime.serialization.objectidgenerator.aspx)
for keeping track of traversed references and detecting cycles.
Cyclic objects are re-instantiated lazily through shallow field copying.

### Subtyping

FsPickler discriminates between reference types that are sealed and those that are not sealed. 
A value of non-sealed type can either be instance of this type or of a proper subtype. 
In the context of open hierarchies, it is clear that a type-fixed pickler implementation simply 
cannot cater to arbitrary subtype instances.

For this reason, picklers will only be used to serialize values whose reflected type is identical
to the pickler's. When coming across an object of non-sealed type, FsPickler
will examine its reflected type; if found to differ, a new pickler will be
generated on-the-fly for that particular subtype.

The above implies that `obj` is serializable in FsPickler, but `Pickler<obj>`
will only be used with `obj()` instances. Similarly, picklers for interfaces
or abstract classes are possible, but on their own are incapable of serializing anything.
This is how serialization of F# closures is made possible:
for every internal closure type generated by the F# compiler, 
a special pickler is generated for handling it.

## Pickler Generation

Conceptually, pickler implementations found in the library can roughly be separated into
the following categories:

  * Atomic picklers: self containing pickler implementations for types like primitives,
    strings, `System.DateTime` and `System.Guid`. These do little other than delegating
    serialization to the underlying 
    [pickle format](reference/nessos-fspickler-ipickleformatprovider.html) 
    implementation.

  * Pickler combinators: as described earlier, these are parametric functions generating picklers
    out of simpler components. Examples are option, list, array and tuple picklers.

  * Pickler generators: for nominal types that are not expressions on known types,
    a different generation strategy is required. The type structure is determined using reflection
    and a new pickler is constructed from that information. FsPickler can emit dynamic methods
    to ensure that no reflection or boxing penalty is incurred during actual serialization.
    Pickler generators can be further separated into the following kinds:
        - Field-based pickler generation.
        - DataContract pickler generation.
        - ISerializable pickler generation.
        - F# union/record/exception pickler generation.

### Putting it all together

So how does FsPickler generate a pickler (or fail trying) for a given type parameter?
I will attempt to explain by giving a simplified example:

*)

[<AbstractClass>]
type TypeShape<'T> () =
    abstract Accept : ITypeShapeVisitor<'R> -> 'R

and ShapeInt () =
    inherit TypeShape<int> ()
    override __.Accept (v : ITypeShapeVisitor<'R>) = v.VisitInt ()

and ShapeString () =
    inherit TypeShape<string> ()
    override __.Accept (v : ITypeShapeVisitor<'R>) = v.VisitString ()

and ShapeTuple<'T, 'S> () =
    inherit TypeShape<'T * 'S> ()
    override __.Accept (v : ITypeShapeVisitor<'R>) = v.VisitTuple<'T, 'S> ()

and ShapeList<'T> () =
    inherit TypeShape<'T list> ()
    override __.Accept (v : ITypeShapeVisitor<'R>) = v.VisitList<'T> ()

and ShapeSet<'T when 'T : comparison> () =
    inherit TypeShape<Set<'T>> ()
    override __.Accept (v : ITypeShapeVisitor<'R>) = v.VisitSet<'T> ()

and ITypeShapeVisitor<'R> =
    abstract VisitInt : unit -> 'R
    abstract VisitString : unit -> 'R
    abstract VisitTuple<'T, 'S> : unit -> 'R
    abstract VisitList<'T> : unit -> 'R
    abstract VisitSet<'T when 'T : comparison> : unit -> 'R

(**

The above declarations define a notion of "type shape", a partitioning of types according
to certain traits satisfied. This pattern gives naturally rise to a "type shape visitor",
which we will be making use of later. Given an arbitrary type `'T`, we can use reflection to extract
and construct the shape it fits in, albeit in a packaged manner:

*)

let getShape<'T> =
    if typeof<'T> = typeof<int> then new ShapeInt() :> obj :?> TypeShape<'T>
    elif typeof<'T> = typeof<string> then new ShapeString() :> obj :?> TypeShape<'T>
    elif typeof<'T>.IsGenericType then
        let gt = typeof<'T>.GetGenericTypeDefinition()
        let tparams = typeof<'T>.GetGenericArguments()
        if gt = typedefof<list<_>> then
            let st = typedefof<ShapeList<_>>.MakeGenericType tparams
            Activator.CreateInstance st :?> TypeShape<'T>
        elif gt = typedefof<_ * _> then
            let st = typedefof<ShapeTuple<_,_>>.MakeGenericType tparams
            Activator.CreateInstance st :?> TypeShape<'T>
        elif gt = typedefof<Set<_>> then
            let st = typedefof<ShapeSet<_>>.MakeGenericType tparams
            Activator.CreateInstance st :?> TypeShape<'T>
        else
            failwith "unsupported type shape."
    else
        failwith "unsupported type shape."

(**

We can now use the type shape visitor to plug in all required pickler combinators.
This will result in a generator function that recursively calls appropriate
pickler combinators in a strongly typed manner:

*)

let rec generate<'T> () : Pickler<'T> =
    let shape = getShape<'T>
    shape.Accept factory :?> Pickler<'T>
            
and factory : ITypeShapeVisitor<Pickler> =
    {
        new ITypeShapeVisitor<Pickler> with
            member __.VisitInt () = Pickler.int :> Pickler
            member __.VisitString () = Pickler.string :> Pickler
            member __.VisitList<'T> () =
                let tp = generate<'T> () in Pickler.list tp :> Pickler

            member __.VisitTuple<'T, 'S> () =
                let tp = generate<'T> ()
                let sp = generate<'S> ()
                Pickler.pair tp sp :> Pickler

            member __.VisitSet<'T when 'T : comparison> () =
                let tp = generate<'T> () in Pickler.set tp :> Pickler
    }

(**

The `generate` function can now be used for auto-generating derivative picklers:

*)

let p1 = generate<int * string> ()
let p2 = generate<int * string list list> () 
let p3 = generate<(int * Set<string>) * (string * (int * Set<string>) list)> ()
