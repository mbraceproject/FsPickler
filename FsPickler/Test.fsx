#r "bin/Release/FsPickler.dll"

open FsPickler
open FsPickler.Combinators

let fsp = new FsPickler()

let p = fsp.GeneratePickler<int * string []> ()

pickle p (42, [| "test" ; "" ; null |]) |> unpickle p

pickle Pickler.obj (42 :> _) |> unpickle Pickler.obj

open System

let tp = Pickler.auto<Type list>

let tp' = Pickler.FromPrimitives((fun r -> Type.GetType(r.BinaryReader.ReadString(), true)), (fun w t -> w.BinaryWriter.Write t.AssemblyQualifiedName), true, true)
let tp'' = Pickler.list tp'

pickle tp' typeof<int> |> unpickle tp'

#time

let tt = 
    typeof<int option>.Assembly.GetTypes() 
    |> Seq.filter(fun t -> t.IsPublic)
    |> Seq.take 100
    |> Seq.map (fun t -> let s = typedefof<int option>.MakeGenericType [|t|] in typedefof<_ * _>.MakeGenericType [| typeof<int> ; s|])
    |> Seq.filter (fun t -> t.AssemblyQualifiedName <> null)
    |> Seq.toList
//    |> Array.map (fun t -> typedefof<int option>.MakeGenericType [|t|])

// ~170
for i = 0 to 100 do
    pickle tp tt |> unpickle tp |> ignore

// 518
for i = 0 to 100 do
    pickle tp'' tt |> unpickle tp'' |> ignore