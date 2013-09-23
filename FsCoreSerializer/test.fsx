#r "bin/Debug/FsCoreSerializer.dll"

open FsCoreSerializer
open System
open System.IO
open System.Reflection
open System.Linq.Expressions

let fsc = new FsCoreSerializer()

let loop (x : 'T) =
    use mem = new MemoryStream()
    fsc.Serialize<obj>(mem, x)
    mem.Position <- 0L
    fsc.Deserialize<obj>(mem) :?> 'T


let types = typeof<int>.Assembly.GetTypes()
let getByName (name : string) = types |> Array.find (fun t -> t.FullName.Contains name)

let t = getByName "System.Text.EUCJPEncoding"
let o = let ctor = t.GetConstructors(BindingFlags.NonPublic ||| BindingFlags.Instance) in ctor.[0].Invoke [||]
let o = System.Activator.CreateInstance t

let t' = getByName "System.Text.BaseCodePageEncoding"
loop o


open Microsoft.FSharp.Reflection

exception Foo of int * string

let ctors = typeof<Foo>.GetConstructors()

let es = FSharpType.GetTupleElements t