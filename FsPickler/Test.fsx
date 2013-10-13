#r "bin/Debug/FsPickler.dll"

open FsPickler
open FsPickler.Combinators

let fsp = new FsPickler()

let p = fsp.GeneratePickler<int * string []> ()

pickle p (42, [| "test" ; "" ; null |]) |> unpickle p


//fsp.Pickle<obj> 12 |> fun t -> fsp.UnPickle<obj> t
pickle Pickler.int 12 |> unpickle Pickler.int