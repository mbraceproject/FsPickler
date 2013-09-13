#r "bin/Debug/FsCoreSerializer.dll"

open FsCoreSerializer
open System
open System.IO

let fsc = new FsCoreSerializer()

let loop (x : 'T) =
    use mem = new MemoryStream()
    fsc.Serialize(mem, x)
    mem.Position <- 0L
    fsc.Deserialize<'T>(mem)

loop <@ 1 @>

let d = Func<int, string>(fun x -> string x)

loop d