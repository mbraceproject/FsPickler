#r "bin/Release/FsPickler.dll"

open FsPickler
open FsPickler.Combinators

let fsp = new FsPickler()

let p = fsp.GeneratePickler<int * string []> ()

pickle p (42, [| "test" ; "" ; null |]) |> unpickle p


open System
open System.Reflection
open System.IO

type TypeNameConverter (?strongAssemblyNames) =
    let strongAssemblyNames = defaultArg strongAssemblyNames true

    let rec write (bw : BinaryWriter) (t : Type) =
        if t.IsGenericType && not t.IsGenericTypeDefinition then
            bw.Write true
            t.GetGenericTypeDefinition() |> write bw
            let gas = t.GetGenericArguments() 
            bw.Write gas.Length
            for ga in gas do write bw ga
        else
            bw.Write false
//            if strongAssemblyNames then bw.Write t.Assembly.FullName
//            else bw.Write(t.Assembly.GetName().Name)
            bw.Write(t.AssemblyQualifiedName)

    let rec read (br : BinaryReader) =
        if br.ReadBoolean() then
            let gt = read br
            let n = br.ReadInt32()
            let gas = Array.zeroCreate<Type> n
            for i = 0 to n - 1 do
                gas.[i] <- read br
            gt.MakeGenericType gas
        else
            Type.GetType(br.ReadString())
//            let assembly = Assembly.Load(br.ReadString())
//            assembly.GetType(br.ReadString())

    interface ITypeNameConverter with
        member tc.WriteQualifiedName bw t = write bw t
        member tc.ReadQualifiedName br = read br


#time

let m = new MemoryStream()
let bw = new BinaryWriter(m)
let br = new BinaryReader(m)

let loop (tyConv : ITypeNameConverter) n (t : Type) =
    for i = 1 to n do
        m.Position <- 0L
        tyConv.WriteQualifiedName bw t
//        m.Position <- 0L
//        let t = tyConv.ReadQualifiedName br in ()


loop (DefaultTypeNameConverter()) 100000 typeof<int * string option list>

loop (TypeNameConverter(true)) 100000 typeof<int * string option list>