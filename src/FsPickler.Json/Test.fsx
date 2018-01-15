open System
open FSharp.Reflection

let tty = Array.init 8 (fun _ -> typeof<int>) |> FSharpType.MakeTupleType

let tuple = Array.init 8 (fun _ -> box 42) |> fun v -> FSharpValue.MakeTuple(v,tty) :?> System.Tuple<int,int,int,int,int,int,int,Tuple<int>>

let foo() = System.Tuple<_,_,_,_,_,_,_,_>(1,1,1,1,1,1,1,1,1)

let x = new System.Tuple<int,string>(42,1)