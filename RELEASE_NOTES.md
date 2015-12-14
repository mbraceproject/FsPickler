### 1.7.0
* Extend serialization support to System.Linq IEnumerables.

### 1.6.2
* Tweak ScriptCS serialization support.

### 1.6.1
* Tweak ScriptCS serialization support.

### 1.6.0
* Add serialization support for ScriptCs bindings.

### 1.5.2
* Fix ReflectionCache binding redirect issue.

### 1.5.1
* Revise ITypeNameConverter.compose definition.

### 1.5.0
* Give additional ITypeNameConverter implementations.

### 1.4.0
* Revisions and updates in DateTime/DateTimeOffset serialization formats.

### 1.3.7
* Fix reference equality issue in object sifting.

### 1.3.6
* Fix block read bug in binary format.

### 1.3.5
* Fix System.Runtime.Remoting.ObjRef serialization.

### 1.3.4
* Fix DataContract struct deserialization in NoEmit setting.

### 1.3.3
* Fix FsPickler.CSharp dependency issue.

### 1.3.2
* Expose type annotated pickle constructor; Remove HashSift methods.

### 1.3.1
* Add support for immutable F# structs implementing DataContract serialization.

### 1.3.0
* Bugfix in sifting of boxed values; renamings in core API.

### 1.2.23
* Bugfix in sifting of boxed values.

### 1.2.22
* Changes to ObjectSizeCounter API.

### 1.2.21
* Sanitize ObjectSizeCounter API.

### 1.2.20
* Fix HashResult.Parse bug.

### 1.2.19
* Add net35 support in FsPickler.CSharp package.

### 1.2.18
* Bugfix.

### 1.2.17
* Implement aggregated object size calculator.

### 1.2.16
* Add hash id extention methods.

### 1.2.15
* Fix MurMur3 implementation bug.

### 1.2.14
* Support non-generic serializable type declarations.

### 1.2.13
* Add support for cloneable non-serializable types.

### 1.2.12
* Add support for .net35 runtimes

### 1.2.11
* Add missing overload in FsPickler.ComputeSize

### 1.2.10
* Add support for Json.NET 7.0

### 1.2.9
* Visitor Bugfix.

### 1.2.8
* Add EnsureSerializable utility.

### 1.2.7
* Fix null interface issue in object visitor.

### 1.2.6
* Fix nuget dependency issue.

### 1.2.5
* Implement user-provided pickler factory registration.

### 1.2.4
* Implement Hashcode-based sifting extensions.

### 1.2.3
* Revise IObjectSifter API.

### 1.2.2
* Rewrite visitor implementation and API.
* Minor API changes.
* Bugfixes.

### 1.2.1
* Bugfixes.

### 1.2.0
* Add fast cloning support.
* Add object sifting support.
* Serializer API overhaul.

### 1.0.19
* Implement IObjectReference support.

### 1.0.18
* Sanitize hashcode generation logic.

### 1.0.17
* Fix support for ISerializable structs.

### 1.0.16
* Refine assembly loading code.

### 1.0.15
* Improve SerializationInfo extension methods.

### 1.0.14
* Implement Pickler.fromSerializationInfo combinator.

### 1.0.13
* Performance fix related to serialization of primitive arrays to binary format.

### 1.0.12
* Fix issue relating to deserialization of ISerializable structs inside large object graphs.
* Revise .IsRecursive and .IsOfFixedSize predicate resolution algorithms.

### 1.0.11
* Fine tune ObjectIdGenerator reset interval at top-level sequence serializations.

### 1.0.10
* Add clone method to FsPicklerSerializer.

### 1.0.9
* Re-enable serialization reset intervals on top-level sequence serialization.

### 1.0.8
* Changed C# project to use quasi-official FSharp.Core NuGet package

### 1.0.7
* Support parameterless constructors in DataContract types.
* Improve exception messages.

### 1.0.6
* Change caching semantics for Custom equality DUs.
* Improve error messages in subtypes of nonserializable types.
* Add Pickle<T> type annotated binary serializations.

### 1.0.5
* Use correct serialization rules DataContract types.

### 1.0.4
* Add support for ExceptionDispatchInfo serialization in the CLR.

### 1.0.3
* Fix support for compiler generated '__' fields.

### 1.0.2
* Add .IsSerializable checks for F# Union/Record/Exception types.

### 1.0.1
* Treat C# compiler generated types as serializable by default.

### 1.0.0
* Add support for IgnoreDataMemberAttribute.

### 0.9.11
* Bug fixes.

### 0.9.10
* Improve recursive type detection.

### 0.9.9
* Add DataContract serialization support.

### 0.9.8
* API improvements.
* Added XML documentation.

#### 0.9.7
* Add support for BSON format.

#### 0.9.6
* Pickle format refinements.
* Add C# library.
* Bug fixes.

#### 0.9.5.1-alpha
* Pickle format refinements.
* Bug fixes.

#### 0.9.5-alpha
* Pickle format refinements.
* Add .NET 4.0 release.
* Overall improvements and fixes.

#### 0.9.4-alpha
* Major rewrite of the library.
* Include support for multiple pickle formats.
* Add support for object visitors.

#### 0.8.6.2
* Add StreamingContext option to Pickle/Unpickle methods

#### 0.8.6.1
* Support .NET 4.0 runtimes
* Add FAKE support

#### 0.8.6.0
* Update namespace to Nessos.FsPickler
* Improve pickler cache performance

#### 0.8.5.3
* Minor corrections

#### 0.8.5.2
* Minor corrections

#### 0.8.5.1
* Fix array deserialization issue.

#### 0.8.5
* Fix serialization issue in F# exceptions.

#### 0.8.4
* Fix field resolution in auto pickler generation.

#### 0.8.3
* Add support for System.Nullable.

#### 0.8.2
* Bug fixes related to serialization of reflection types.

#### 0.8.1
* Rewritten serialization of reflection types. 
* Changes to serialization format. 
* Bug fixes. 

#### 0.7.2
* C# friendly API 
* Bug fixes

#### 0.7.1
 * Improved performance for F# immutable collections. 

#### 0.7.0
* Support for structural hashcode generation. 
* Switched to dynamic methods from expression trees, resulting in a 2x speedup in emitted code.

#### 0.6.1
* Bugfixes and extensions to the client API.

#### 0.6.0
* Renamed to FsPickler from FsCoreSerializer.
* Picklers now strongly typed and part of the public API.