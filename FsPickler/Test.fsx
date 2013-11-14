#r "bin/Debug/FsPickler.dll"

open FsPickler
open FsPickler.Combinators

let fsp = new FsPickler()

let p = fsp.GeneratePickler<int * string []> ()

pickle p (42, [| "test" ; "" ; null |]) |> unpickle p

#r "../FsPickler.Tests/bin/Debug/FsPickler.Tests.exe"

open FsPickler
open FsPickler.Tests

let p = TestTypes.int2Peano 100

let (p : TestTypes.Peano) = fsp.Pickle p |> fsp.UnPickle

let fp = new TestFsPickler() :> ISerializer

Serializer.write fp p |> Serializer.read<TestTypes.Peano> fp