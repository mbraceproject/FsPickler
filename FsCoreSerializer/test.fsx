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


loop 12

let types = typeof<int>.Assembly.GetTypes()
let getByName (name : string) = types |> Array.find (fun t -> t.FullName.Contains name)

let t = getByName "System.Text.EUCJPEncoding"
let o = let ctor = t.GetConstructors(BindingFlags.NonPublic ||| BindingFlags.Instance) in ctor.[0].Invoke [||]
let o = System.Activator.CreateInstance t

let t' = getByName "System.Text.BaseCodePageEncoding"
loop o


typeof<FsCoreSerializer>.Assembly.GetType "FsCoreSerializer.IFsCoreSerializable"

open Microsoft.FSharp.Reflection

type A = B | C of int * string


let ctors = typeof<Foo>.GetConstructors()

let uci = FSharpType.GetUnionCases typeof<A> |> fun x -> x.[1]
let es = FSharpValue.PreComputeUnionConstructorInfo uci

uci.DeclaringType
uci.GetFields().[0].DeclaringType

type A() =
    let b = new B()
    member __.Value = b

and B() = inherit A()


loop (A())