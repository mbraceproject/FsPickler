(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**

# FsPickler : A fast .NET object serializer

FsPickler is a serialization library that facilitates the distribution of .NET objects.
The implementation focuses on performance and completeness in supported types, including F# types.
It supports multiple, pluggable serialization formats including Binary, Xml, JSON and BSON.
The library is based on the functional programming concept of 
[pickler combinators](http://lambda-the-ultimate.org/node/2243)
which has been adapted to accommodate the object oriented nature of the .NET framework.

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The FsPickler library can be <a href="https://nuget.org/packages/FsPickler">installed from NuGet</a>:
      <pre>PM> Install-Package FsPickler
PM> Install-Package FsPickler.Json
PM> Install-Package FsPickler.CSharp</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

## Example

This example demonstrates a basic serialization roundtrip using the library

*)
#r "FsPickler.dll"
open Nessos.FsPickler

let binary = FsPickler.CreateBinary()

let pickle = binary.Pickle [Some 1; None ; Some -1]
binary.UnPickle<int option list> pickle

(**

## Why FsPickler?

The principal motivation behind creating FsPickler is the need for a
library that provides efficient, correct and complete serialization of
objects in the CLR and mono runtime. It is aimed at providing a foundation for
large-scale distributed computation that is based on the .NET framework.

FsPickler is ideally suited for serializing:

 * Large and complex objects, such as dictionaries, trees and cyclic objects.

 * Abstract classes, subtypes, nulls, delegates and closures.

 * ISerializable, DataContract or attribute-based serialization.

 * F# unions, records and quotations.

 * Inaccessible types or types unknown at compile time.

FsPickler is NOT:

 * a general-purpose Xml/JSON/BSON framework.

 * a library designed for cross-platform communication.

## Documentation & Technical Overview

A collection of tutorials, technical overviews and API references of the library.

 * [Tutorial](tutorial.html) A short introduction to FsPickler.

 * [Technical Overview](overview.html) A walkthrough of the library's implementation details.

 * [Performance](benchmarks.html) Benchmarks comparing FsPickler
   to other established serialization libraries.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library.
 
## Contributing and copyright

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests.

The library is available under the MIT License. 
For more information see the [License file][license] in the GitHub repository. 

  [gh]: https://github.com/nessos/FsPickler
  [issues]: https://github.com/nessos/FsPickler/issues
  [license]: https://github.com/nessos/FsPickler/blob/master/License.md
*)
