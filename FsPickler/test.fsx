#r "bin/Debug/FsPickler.dll"

open FsPickler
open FsPickler.Combinators
open System
open System.IO
open System.Reflection

let fsc = new FsPickler()

let loop (x : 'T) =
    use mem = new MemoryStream()
    fsc.Serialize<obj>(mem, x)
    mem.Position <- 0L
    fsc.Deserialize<obj>(mem) :?> 'T


type Peano = Zero | Succ of Peano


let f (x : Pickler<'T>) = x |> Pickler.option |> Pickler.list |> Pickler.choice2 Pickler.int

let peano =
    Pickler.fix(fun peano ->
        peano
        |> Pickler.option
        |> Pickler.wrap (function None -> Zero | Some p -> Succ p)
                        (function Zero -> None | Succ p -> Some p))

peano

(Succ (Succ Zero)) |> pickle peano |> unpickle peano