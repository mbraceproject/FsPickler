#r "bin/Debug/FsPickler.CSharp.dll"
//#r "../packages/FsCheck.0.9.2.0/lib/net40-Client/FsCheck.dll"

open System.IO
open Nessos.FsPickler.Binary

//open FsCheck


type TestCase =
    | Bool of bool
    | Byte of byte
    | Bytes of byte []
    | Char of char
    | Int32 of int
    | Int64 of int64
//    | UInt64 of uint64
    | String of string

let write (bw : BinaryWriter) =
    function
    | Bool b -> bw.Write b
    | Byte b -> bw.Write b
    | Bytes bs -> bw.Write bs
    | Char c -> bw.Write c
    | Int32 n -> bw.Write n
    | Int64 n -> bw.Write n
//    | UInt64 n -> bw.Write n
    | String s -> bw.Write s

let read (br : BinaryReader) =
    let inline check expected got = 
        if expected <> got then 
            failwithf "expected '%A' but got '%A'." expected got

    function
    | Bool b -> let b' = br.ReadBoolean() in check b b'
    | Byte b -> let b' = br.ReadByte() in check b b'
    | Bytes bs -> let bs' = br.ReadBytes() in check bs bs'
    | Char c -> let c' = br.ReadChar() in check c c'
    | Int32 n -> let n' = br.ReadInt32() in check n n'
    | Int64 n -> let n' = br.ReadInt64() in check n n'
    | String s -> let s' = br.ReadString() in check s s'

let test (inputs : TestCase list) =
    use m = new MemoryStream()
    let bw = new BinaryWriter(m)
    for i in inputs do write bw i
    bw.Flush()
    m.Position <- 0L
    let br = new BinaryReader(m)
    for i in inputs do read br i

let case = [Bytes [|1uy .. 3uy |] ; Int32 1231 ; Int64 92838231L ; String "abcdefgijklmnopqrstuvwxyz!12312!@#$%^&*(" ; Int32 1123 ; ]

let huge = [1..100] |> List.collect (fun _ -> case)

test case
test huge


#time

let getValue value () = value
let getDefault<'T> = fun () -> Unchecked.defaultof<'T>
    

let clo1 = getValue ()

for i = 1 to 1000000000 do
    clo1 ()

let clo2 = getDefault<unit>

for i = 1 to 1000000000 do
    clo2 ()