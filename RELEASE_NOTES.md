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