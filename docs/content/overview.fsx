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

  * There is confusion as to which is the prefered methodology for when
    defining serialization semantics for new types.
    The BCL comes with a multitude of dated, unsafe, mutually exclusive
    and naturally inefficient approaches.

### BCL Serialization methods

The BCL comes with the following patterns for defining serializable types:

  * Field-based serialization: objects are pickled by serializing the contents 
    of their fields. This behaviour is tuned by applying an assortment of attributes
    such as `Serialized`, `NonSerialized`, `OnSerializing`, `OnDeserialized` and
    the `IDeserializationCallback` interface. This is arguably the most
    problematic approach, since it is sensitive to internal implementation details.
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
but that eventually took its toll on overall performance,
so we decided to build a new library from scratch.

FsPickler was created with two goals in mind: performance and completeness.
Importantly, it was conceived as a .NET serializer, rather than an F# serializer:
if it is a .NET object designed to be serialized, it should be serializable in FsPickler.
In that sense, it was designed with a goal to embrace the imperfect world that is
the .NET framework in its totality, albeit with an eye for correctness.

At this point, we should acknowledge Anton Tayanovskyy and his great little F#
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
The short answer would be any type `'T` for which we present an instance of `Pickler<'T>`.
To make the question more meaningful: for what types does the library auto-generate picklers? 
This is something that can only be answered at runtime, typically by calling

*)

FsPickler.IsSerializableType<SomeType> ()

(**

Primitive types and `string` are serializable but pointers, COM objects and MarshalByRef types are not.
A managed class or struct is serializable if and only if it carries the `Serializable` attribute
and moreover satisfies either of the following:

  * is abstract or a delegate.

  * is array/nullable/tuple/enum of serializable element types.

  * implements the `ISerializable` attribute and has a matching constructor implementation.

  * carries the `DataContract` attribute and all designated properties are of 
    serializable type and settable. Parameterless constructors or public properties are not required.

  * carries the [`CustomPickler`](reference/nessos-fspickler-custompicklerattribute.html) attribute and an accompanying pickler factory method.

  * is neither of the above and all designated fields are of serializable type.
    Parameterless constructors or public fields are not required.
*)