#r "bin/Release/FsPickler.dll"

open FsPickler
open FsPickler.Combinators

let fsp = new FsPickler()

let p = fsp.GeneratePickler<int * string []> ()

pickle p (42, [| "test" ; "" ; null |]) |> unpickle p

open System.Globalization

let ci = CultureInfo(42)

ci.ToString()

let an = typeof<int>.Assembly.GetName()
let name = an.CultureInfo.ToString()

//new CultureInfo = new CultureInfo("")

let a = typeof<int>.Assembly


#time
open System

for i = 0 to 100000 do
//    new CultureInfo(42) |> ignore
    Version("1.2.3.4") |> ignore
//    new Version(1,2,3,4) |> ignore
//    ((42,32, 1821 , Some (1,33)), "hello", "bye") |> ignore
//    a.GetType("System.Int32") |> ignore



for i = 0 to 100000 do
    

for i = 0 to 100000 do
    42 = ci.LCID |> ignore