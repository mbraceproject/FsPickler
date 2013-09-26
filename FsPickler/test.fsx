#r "bin/Debug/FsPickler.dll"

open FsPickler
open System
open System.IO
open System.Reflection

let fsc = new FsPickler()

let loop (x : 'T) =
    use mem = new MemoryStream()
    fsc.Serialize<obj>(mem, x)
    mem.Position <- 0L
    fsc.Deserialize<obj>(mem) :?> 'T
    
    
//M<T> -> (T -> M<U>) -> M<U>
//M<T> -> (M<T> -> U) -> M<U>

open System.IO
 
type Format<'T> =
    {
        Read : BinaryReader -> 'T
        Write : BinaryWriter -> 'T -> unit
    }
 
type FormatUnionPart<'X,'U> =
    {
        R : list<BinaryReader -> 'U>
        W : 'X -> 'U -> BinaryWriter -> unit
    }
 
let Case (ctor: 'C -> 'U) (fmt: Format<'C>) 
        (part: FormatUnionPart<'X,'U>) : FormatUnionPart<('C -> unit) -> 'X,'U> =

    let tag = char part.R.Length
    let write (w: BinaryWriter) x =
        w.Write(tag)
        fmt.Write w x
    {
        R = (fmt.Read >> ctor) :: part.R
        W = fun f u w -> part.W (f (write w)) u w
    }
 
let End<'U> : FormatUnionPart<('U -> unit),'U> =
    {
        R = []
        W = fun f u w -> f u
    }
 
let Union (proof: 'X) (part: FormatUnionPart<'X,'U>) : Format<'U> =
    let rs = Array.rev (List.toArray part.R)
    {
        Read = fun r ->
            let tag = int (r.ReadChar())
            rs.[tag] r
        Write = fun w x -> part.W proof x w
    }
 
let IntFormat : Format<int> =
    {
        Read = fun r -> r.ReadInt32()
        Write = fun w x -> w.Write(x)
    }
 
let FloatFormat : Format<float> =
    {
        Read = fun r -> r.ReadDouble()
        Write = fun w x -> w.Write(x)
    }
 
let StringFormat : Format<string> =
    {
        Read = fun r -> r.ReadString()
        Write = fun w x -> w.Write(x)
    }
 
type U =
    | A of int
    | B of float
    | C of string
 
let UFormat =
    Union (fun a b c x ->
        match x with
        | A x -> a x
        | B x -> b x
        | C x -> c x)
    << Case A IntFormat
    << Case B FloatFormat
    << Case C StringFormat
    <| End
 
let test (fmt: Format<'T>) (value: 'T) =
    let bytes =
        use s = new MemoryStream()
        use w = new BinaryWriter(s)
        fmt.Write w value
        s.ToArray()
    let roundtrip =
        use s = new MemoryStream(bytes, false)
        use r = new BinaryReader(s)
        fmt.Read r
    roundtrip = value


let func = System.Func<int,int,int>(fun x y -> x + y)
let f = FuncConvert.FuncFromTupled func