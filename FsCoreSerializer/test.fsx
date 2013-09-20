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



type Rec = Rec of (Rec -> Rec)

type Foo = { Foo : Foo }

let rec f = Rec (fun g -> let (Rec f) = f in f g)

loop f

open Microsoft.FSharp.Reflection

type Foo = A | B of string 

let t = FSharpType.GetUnionCases typeof<Foo>

let u = t.[1]

u.GetFields().[0].DeclaringType