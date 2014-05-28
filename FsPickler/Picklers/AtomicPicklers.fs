module internal Nessos.FsPickler.BasePicklers

    open System
    open System.Reflection
    open System.Collections.Generic
    open System.Runtime.Serialization

    open Nessos.FsPickler
    open Nessos.FsPickler.Utils
    open Nessos.FsPickler.PicklerUtils

    // misc atomic picklers

    let mkObjPickler () = mkPickler PicklerInfo.Atomic false true (fun _ -> obj ()) (fun _ _ -> ())
    
    let mkUnitP () = mkPickler PicklerInfo.Atomic false false ignore (fun _ _ -> ())
    let mkDBNullPickler () = mkPickler PicklerInfo.Atomic false false (fun _ -> DBNull.Value) (fun _ _ -> ())

    let mkGuidPickler () = mkPickler PicklerInfo.Atomic false false (fun br -> Guid(br.Formatter.ReadBytesFixed "bytes" 16)) 
                                                                    (fun bw g -> bw.Formatter.WriteBytesFixed "bytes" (g.ToByteArray()))

    let mkTimeSpanPickler () = mkPickler PicklerInfo.Atomic false false (fun br -> TimeSpan(br.Formatter.ReadInt64 "ticks")) 
                                                                        (fun bw x -> bw.Formatter.WriteInt64 "ticks" x.Ticks)

    let mkDateTimePickler () = mkPickler PicklerInfo.Atomic false false (fun br -> DateTime(br.Formatter.ReadInt64 "ticks")) 
                                                                        (fun bw x -> bw.Formatter.WriteInt64 "ticks" x.Ticks)

    let mkBigIntPickler () = mkPickler PicklerInfo.Atomic false false (fun r -> System.Numerics.BigInteger(r.Formatter.ReadBytes "bytes"))
                                                                        (fun w x -> let bytes = x.ToByteArray() in w.Formatter.WriteBytes "bytes" bytes)

    let mkAtomicPicklers () =
        [| 
            mkObjPickler () :> Pickler ; mkUnitP () :> Pickler ; mkDBNullPickler () :> Pickler
            mkGuidPickler () :> Pickler ; mkTimeSpanPickler () :> Pickler ; mkDateTimePickler () :> Pickler
            mkDateTimePickler () :> Pickler ; mkBigIntPickler () :> Pickler
        |]