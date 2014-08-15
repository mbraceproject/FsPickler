(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

#r "Newtonsoft.Json.dll"
#r "FsPickler.dll"
#r "FsPickler.Json.dll"

open Nessos.FsPickler
open Nessos.FsPickler.Combinators

type SomeType = class end

(**

# Technical Overview

This article discusses the general topic of serialization in .NET
and gives an overview of the implementation details in FsPickler.

## Serialization in the .NET framework

Serialization in the .NET framework is something often considered
as being problematic or even broken. This is mainly for the following reasons:

  * There is no type or runtime support for serialization. 
    Rather, serializers are library implementations that mostly depend
    on the reflection system to extrapolate serialization rules for
    each type (resulting in runtime errors if this is not possible).

  * There is confusion as to which is the prefered design pattern for serialization.
    The BCL comes with a multitude of dated, unsafe, mutually exclusive
    and naturally inefficient patterns.

### BCL Serialization patterns

The BCL comes with the following patterns for defining serializable types:

  * Field-based serialization: objects are pickled by serializing the contents 
    of their fields. This behaviour is tuned by applying an assortment of attributes
    such as `Serialized`, `NonSerialized`, `OnSerializing`, `OnDeserialized`, etc and
    implementing the `IDeserializationCallback` interface. This is arguably the most
    loathed serialization pattern, since it is extremely sensitive to internal implementation details.
    Incidentally, this is the pattern of choice for most F# generated types.

  * Property-based or [DataContract](http://msdn.microsoft.com/en-us/library/ms733127.aspx) 
    serialization: objects are pickled by serializing the contents
    of specially flagged properties. Properties should be settable and are usually required to be
    public. In most cases, a parameterless constructor is also necessary. Property-based serialization
    is probably the most popular pattern.

  * [ISerializable](http://msdn.microsoft.com/en-us/library/system.runtime.serialization.iserializable.aspx) 
    types: possibly intended as a type-safe replacement of field-based serialization.
    It fails to live up to this role, mostly due to the nature of interfaces; it still requires a
    special constructor for deserialization which, if missing, would result in a runtime error.
    It is a particularly verbose pattern, resulting in a lot of boilerplate code.
    Its dynamic nature also makes it impossible to reason about the components it serializes a priori.
    It is however an indispensible pattern, since it is used by many BCL types, most important being
    exception types, whose metadata is impossible to replicate without `ISerializable`.

### Serialization Libraries

The BCL ships with a couple of general-purpose serializer libraries but these either suffer
from obsolescence (e.g. [BinaryFormatter](http://msdn.microsoft.com/en-us/library/system.runtime.serialization.formatters.binary.binaryformatter.aspx)) 
or are painfully slow ([NetDataContractSerializer](http://msdn.microsoft.com/en-us/library/system.runtime.serialization.netdatacontractserializer.aspx)).

Many third-party libraries have emerged attempting to address the issue of performance, some with success.
However, experience shows that such libraries hardly achieve the level of generalization
offered by their BCL counterparts. In particular most of them:

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
but that eventually took its toll on overall framework performance,
so we decided to build a new library from scratch.

FsPickler was created with two goals in mind: performance and completeness.
Importantly, it was conceived as a .NET serializer, rather than an F# serializer:
if it is a .NET object designed to be serialized, it should be serializable in FsPickler.
In that sense, it was designed with a goal to embrace the imperfect world that is
the .NET framework in its totality, albeit with an eye for correctness.

At this point, I should acknowledge Anton Tayanovskyy and his great little F#
snippet [Union-Friendly Generic Binary Serializer](http://www.fssnip.net/6u)
which served as the initial inspiration for this library.

## Picklers and Pickler Combinators

FsPickler is founded on the functional programming notion of picklers and
pickler combinators. A *pickler* is essentially the type:

*)

type Pickler<'T> =
    {
        Write : WriteState -> 'T -> unit
        Read  : ReadState  -> 'T
    }

(**

The `Write` and `Read` functions represent serialization and deserialization implementations
for values of type `'T`. The `WriteState` and `ReadState` objects enclose serialization state,
such target/source streams and things like object id generators.

*Pickler combinators* are functions that operate on picklers, generating picklers of more complex types.
For instance, the following could be a pickler combinator for rank-1 arrays:

*)

let mkArrayPickler (ip : Pickler<int>) (tp : Pickler<'T>) : Pickler<'T []> =
    {
        Write = fun s a -> ip.Write s a.Length ; Array.iter (tp.Write s) a
        Read = fun s -> let l = ip.Read s in Array.init l (fun _ -> tp.Read s)
    }

(***hide***)
open Nessos.FsPickler

(**

When compared to prevailing .NET serialization patterns, 
picklers are exceptionally fast since they are given direct access to the pickled value
without any reliance on reflection.

Pickler combinators were originally described in Andrew Kennedy's 
[paper of the same name](http://research.microsoft.com/pubs/64036/picklercombinators.pdf).
The combinators presented are sufficient for things like algebraic data types,
but picklers would still have to be composed manually by the implementor.

FsPickler extends the concept of picklers so that object-oriented features
of the .NET framework are accommodated: null references, cyclic objects
and subtype polymorphism.

FsPickler is essentially an automated pickler generation framework: picklers are generated at runtime
and on demand using a combination of reflection and dynamic IL generation. Picklers are cached for 
future use, hence the cost of generation has a constant price.

It should be mentioned for the sake of completeness that a 
[similar effort](http://lampwww.epfl.ch/~hmiller/files/pickling.pdf) 
has been undertaken by the Scala community. There are differences of approach however, 
since that library utilizes the metaprogramming facility offered by scala.

## Serializable Types

So what qualifies as a serializable type in FsPickler?
The short answer would be any type `'T` for which we present an instance of type `Pickler<'T>`.
To make the question more meaningful, it should be rephrased to this:
for what types does FsPickler auto-generate picklers?
This is something that can only be answered at runtime, 
typically by calling

*)

let pickler = FsPickler.GeneratePickler<SomeType> ()

(** or by asking *)

FsPickler.IsSerializableType<SomeType>()

(**

Primitive types and `string` are serializable
but pointers, COM objects and `MarshalByRefObject` types are not.
A composite type is serializable if and only if it carries the `Serializable` attribute
and moreover satisfies either of the following:

  * is abstract or a delegate.

  * is array/nullable/tuple/enum of serializable element types.

  * implements the `ISerializable` attribute and has a matching constructor implementation.

  * carries the `DataContract` attribute and all designated properties are of 
    serializable type and settable. Parameterless constructors or public properties are not required.

  * carries the `CustomPickler` attribute and an accompanying pickler factory method.

  * is neither of the above and all designated fields are of serializable type.
    Parameterless constructors or public fields are not required.
*)