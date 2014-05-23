module internal Nessos.FsPickler.BasePicklers

    open System
    open System.Reflection
    open System.Collections.Generic
    open System.Runtime.Serialization

    open Nessos.FsPickler
    open Nessos.FsPickler.Utils
    open Nessos.FsPickler.PicklerUtils

    // primitive picklers : nativeint & unativeint excluded
//
//    let mkByteP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadByte null) (fun bw x -> bw.Formatter.WriteByte null x)
//    let mkSByteP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadSByte null) (fun bw x -> bw.Formatter.WriteSByte null x)
//    let mkCharP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadChar null) (fun bw x -> bw.Formatter.WriteChar null x)
//    let mkBoolP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadBoolean null) (fun bw x -> bw.Formatter.WriteBoolean null x)
//    let mkDecimalP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadDecimal null) (fun bw x -> bw.Formatter.WriteDecimal null x)
//    let mkSingleP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadSingle null) (fun bw x -> bw.Formatter.WriteSingle null x)
//    let mkFloatP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadDouble null) (fun bw x -> bw.Formatter.WriteDouble null x)
//    let mkInt16P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadInt16 null) (fun bw x -> bw.Formatter.WriteInt16 null x)
//    let mkInt32P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadInt32 null) (fun bw x -> bw.Formatter.WriteInt32 null x)
//    let mkInt64P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadInt64 null) (fun bw x -> bw.Formatter.WriteInt64 null x)
//    let mkUInt16P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadUInt16 null) (fun bw x -> bw.Formatter.WriteUInt16 null x)
//    let mkUInt32P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadUInt32 null) (fun bw x -> bw.Formatter.WriteUInt32 null x)
//    let mkUInt64P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadUInt64 null) (fun bw x -> bw.Formatter.WriteUInt64 null x)


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

    let mkByteArrayPickler () = mkPickler PicklerInfo.Atomic false true (fun r -> r.Formatter.ReadBytes "data")
                                                                        (fun w x -> w.Formatter.WriteBytes "data" x)

    let mkAtomicPicklers () =
        [| 
            mkObjPickler () :> Pickler ; mkUnitP () :> Pickler ; mkDBNullPickler () :> Pickler
            mkGuidPickler () :> Pickler ; mkTimeSpanPickler () :> Pickler ; mkDateTimePickler () :> Pickler
            mkDateTimePickler () :> Pickler ; mkBigIntPickler () :> Pickler ; mkByteArrayPickler () :> Pickler
        |]