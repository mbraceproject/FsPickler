#r "bin/Debug/FsCoreSerializer.dll"

open FsCoreSerializer
open System
open System.IO
open System.Reflection

let fsc = new FsCoreSerializer()

let loop (x : 'T) =
    use mem = new MemoryStream()
    fsc.Serialize<obj>(mem, x)
    mem.Position <- 0L
    fsc.Deserialize<obj>(mem) :?> 'T