FsCoreSerializer
================

A general purpose serializer for F# core types.
Based on Kurt Schelftout's FsReflect (https://bitbucket.org/kurt/fsreflect)
and the serializer implementation of Anton Tayanovskyy (http://fssnip.net/6u)
Observed performance is about 10-20x faster than that of .NET serializers for
recursive algebraic datatypes.
