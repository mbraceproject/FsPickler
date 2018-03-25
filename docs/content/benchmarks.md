# Performance

THIS PAGE IS OUTDATED:- please see the updated benchmarks page [here](https://github.com/mbraceproject/FsPickler/wiki/.NET-Core-Benchmarks).

How does FsPickler compare against other .Net serialization libraries in terms of performance? 
In this section we present a few microbenchmarks: we compared FsPickler against 
[BinaryFormatter](http://msdn.microsoft.com/en-us/library/system.runtime.serialization.formatters.binary.binaryformatter.aspx), 
[NetDataContractSerializer](http://msdn.microsoft.com/en-us/library/system.runtime.serialization.netdatacontractserializer.aspx), 
[Json.Net 7.0](http://james.newtonking.com/json), [ProtoBuf-Net 2.0](https://code.google.com/p/protobuf-net/)
and [Wire 0.0.5](https://www.nuget.org/packages/Wire).
Code used to run the tests is available in the 
[FsPickler.PerfTests](https://github.com/mbraceproject/FsPickler/tree/master/tests/FsPickler.PerfTests) project.
Benchmarks were run on an Intel Core i7-7200K CPU @ 3.50GHz with 16GB RAM.

Last updated on October 12, 2015.

## Execution Time

The first set of tests measures serialization roundtrip time and GC collections.
Performance testing was done using the [PerfUtil](https://github.com/eiriktsarpalis/PerfUtil) library. 
Libraries not appearing in charts failed to serialize the tested objects.
Benchmarks are indicative and in no way scientific.

### Simple POCO

<img src="benchmarks/time/poco.png" />
<img src="benchmarks/gc/poco.png" />

### ISerializable Object

<img src="benchmarks/time/iserializable.png" />
<img src="benchmarks/gc/iserializable.png" />

### DataContract Object

<img src="benchmarks/time/datacontract.png" />
<img src="benchmarks/gc/datacontract.png" />

### System.Tuple

<img src="benchmarks/time/tuple6.png" />
<img src="benchmarks/gc/tuple6.png" />

### Exception with Stacktrace

<img src="benchmarks/time/exception.png" />
<img src="benchmarks/gc/exception.png" />

### Array of Tuples

<img src="benchmarks/time/tuplearray.png" />
<img src="benchmarks/gc/tuplearray.png" />

### Array of System.Type

<img src="benchmarks/time/typearray.png" />
<img src="benchmarks/gc/typearray.png" />

### Generic Dictionary

<img src="benchmarks/time/dict.png" />
<img src="benchmarks/gc/dict.png" />

### Binary Tree

<img src="benchmarks/time/tree.png" />
<img src="benchmarks/gc/tree.png" />

### F# list of integers

<img src="benchmarks/time/intlist.png" />
<img src="benchmarks/gc/intlist.png" />

### F# list of tuples

<img src="benchmarks/time/pairlist.png" />
<img src="benchmarks/gc/pairlist.png" />

### 3D float Array (200 x 200 x 200)

<img src="benchmarks/time/3darray.png" />
<img src="benchmarks/gc/3darray.png" />

### F# mutual recursive types

<img src="benchmarks/time/fsmutual.png" />
<img src="benchmarks/gc/fsmutual.png" />

### F# Quotation

<img src="benchmarks/time/quotation.png" />
<img src="benchmarks/gc/quotation.png" />

### Random object graph (n = 500, Pb = 20%)

<img src="benchmarks/time/graph.png" />
<img src="benchmarks/gc/graph.png" />

## Serialization Size

The second set of tests compare the size (in bytes) of serialized objects.
Libraries not appearing in charts failed to serialize the tested objects.

### System.Integer

<img src="benchmarks/size/integer.png" />

### System.Tuple

<img src="benchmarks/size/pair.png" />

### Integer Array (10^6 elements)

<img src="benchmarks/size/intarray.png" />

### Pair Array (10000 elements)

<img src="benchmarks/size/pairarray.png" />

### 3D float Array (100x100x100)

<img src="benchmarks/size/3darray.png" />

### F# List of Integers (1000 elements)

<img src="benchmarks/size/intlist.png" />

### F# List of Pairs (1000 elements)

<img src="benchmarks/size/pairlist.png" />

### Simple POCO

<img src="benchmarks/size/poco.png" />

### Dictionary of POCOs (1000 entries)

<img src="benchmarks/size/dict.png" />

### Exception (with stacktrace)

<img src="benchmarks/size/exception.png" />

### Binary tree (balanced, depth = 10)

<img src="benchmarks/size/tree.png" />

### F# Quotation

<img src="benchmarks/size/quotation.png" />