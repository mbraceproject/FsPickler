# Performance

How does FsPickler compare against other .Net serialization libraries in terms of performance? 
In this section we present a few microbenchmarks: we compared FsPickler against 
[BinaryFormatter](http://msdn.microsoft.com/en-us/library/system.runtime.serialization.formatters.binary.binaryformatter.aspx), 
[NetDataContractSerializer](http://msdn.microsoft.com/en-us/library/system.runtime.serialization.netdatacontractserializer.aspx), 
[Json.Net](http://james.newtonking.com/json), [ServiceStack.Text](https://www.nuget.org/packages/ServiceStack.Text/4.0.15) and
[ProtoBuf-Net](https://code.google.com/p/protobuf-net/). 
Code used to run the tests is available in the 
[FsPickler.PerfTests](https://github.com/nessos/FsPickler/tree/master/tests/FsPickler.PerfTests) project.

## Execution Time

The first set of tests measures execution time.
Performance testing was done using the [PerfUtil](https://github.com/eiriktsarpalis/PerfUtil) library. 
Libraries not appearing in charts failed to serialize the tested objects.
Benchmarks are indicative and in no way scientific.

### System.Tuple

<img src="benchmarks/time/tuple.png" />

### Array of Tuples

<img src="benchmarks/time/tupleArray.png" />

### Simple Record Class

<img src="benchmarks/time/class.png" />

### ISerializable Class

<img src="benchmarks/time/iserializable.png" />

### Generic Dictionary

<img src="benchmarks/time/dictionary.png" />

### Binary Tree

<img src="benchmarks/time/binaryTree.png" />

### F# list of integers

<img src="benchmarks/time/list.png" />

### F# list of tuples

<img src="benchmarks/time/pairList.png" />

### 3D float Array (200 x 200 x 200)

<img src="benchmarks/time/array3D.png" />

### F# mutual recursive types

<img src="benchmarks/time/fsharpForest.png" />

### F# Quotation

<img src="benchmarks/time/quotation.png" />

### Random object graph (n = 500, Pb = 20%)

<img src="benchmarks/time/random.png" />

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

<img src="benchmarks/size/array.png" />

### F# List of Integers (1000 elements)

<img src="benchmarks/size/intlist.png" />

### F# List of Pairs (1000 elements)

<img src="benchmarks/size/pairlist.png" />

### Record

<img src="benchmarks/size/record.png" />

### Dictionary of Records (1000 entries)

<img src="benchmarks/size/dictionary.png" />

### Exception (with stacktrace)

<img src="benchmarks/size/exception.png" />

### Binary tree (balanced, depth = 10)

<img src="benchmarks/size/bintree.png" />

### F# Quotation

<img src="benchmarks/size/quotation.png" />