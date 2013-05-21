FsCoreSerializer (v0.5)
================

A general purpose binary serializer for .NET objects written in F#.
* Serializers are precomputed in a compositional manner.
* Uses dynamic methods over reflection for performance.
* Supports most serializable .NET types, including classes implementing 
  the ISerializable interface.
* Highly optimized for the ML types used in F#.
* Observed performance about 5-20x faster than most serializers bundled with .NET.

For more information, see http://egtsrp.blogspot.com
Nuget package: https://nuget.org/packages/FsCoreSerializer/0.5
