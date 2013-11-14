#r "bin/Debug/FsPickler.dll"

open FsPickler
open FsPickler.Combinators

let fsp = new FsPickler()

let p = fsp.GeneratePickler<int * string []> ()

pickle p (42, [| "test" ; "" ; null |]) |> unpickle p


open System
open System.Reflection
open System.Reflection.Emit

type private Marker = class end

let private voidType = Type.GetType("System.Void")

let compile<'Dele when 'Dele :> Delegate> (name : string) (argTypes : Type []) (returnType : Type) (builderF : ILGenerator -> unit) =

    let dMeth = new DynamicMethod(name, MethodAttributes.Static ||| MethodAttributes.Public, 
                    CallingConventions.Standard, returnType, argTypes, typeof<Marker>, skipVisibility = true)

    let ilGen = dMeth.GetILGenerator()
    do builderF ilGen
    dMeth.CreateDelegate(typeof<'Dele>) :?> 'Dele

let compileFunc<'T> (name : string) (builderF : ILGenerator -> unit) =
    compile<Func<'T>> name [| |] typeof<'T> builderF

[<Struct>]
type Foo =

    val mutable public x : int

    new (x) = { x = x }


let (f : Foo) = fsp.Pickle (Foo(1821)) |> fsp.UnPickle

f.x