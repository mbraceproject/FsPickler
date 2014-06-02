#r "bin/Debug/FsPickler.CSharp.dll"

open System.IO
open Nessos.FsPickler.CSharp


let m = new MemoryStream()

let bw = new BufferedBinaryWriter(m, true)


bw.Write 12 ; bw.Write true ; bw.Write 47uy ; bw.Write "hi!" ; bw.Write 3.1415926m ; bw.Dispose()

m.Position <- 0L

m.ToArray()

let br = new BufferedBinaryReader(m)

br.ReadInt32()
br.ReadBoolean()
br.ReadByte()
br.ReadString()
br.ReadDecimal()
br.ReadUInt64()

[<Struct>]
type Foo(x : int, y : float) =
    member __.A = x
    member __.B = y

sizeof<Foo>