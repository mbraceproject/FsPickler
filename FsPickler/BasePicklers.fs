module internal Nessos.FsPickler.BasePicklers

    open System
    open System.Reflection
    open System.Collections.Generic
    open System.Runtime.Serialization

    open Nessos.FsPickler
    open Nessos.FsPickler.Utils
    open Nessos.FsPickler.PicklerUtils

    // primitive picklers : nativeint & unativeint excluded

    let mkByteP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadByte null) (fun bw x -> bw.Formatter.WriteByte null x)
    let mkSByteP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadSByte null) (fun bw x -> bw.Formatter.WriteSByte null x)
    let mkCharP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadChar null) (fun bw x -> bw.Formatter.WriteChar null x)
    let mkBoolP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadBoolean null) (fun bw x -> bw.Formatter.WriteBoolean null x)
    let mkDecimalP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadDecimal null) (fun bw x -> bw.Formatter.WriteDecimal null x)
    let mkSingleP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadSingle null) (fun bw x -> bw.Formatter.WriteSingle null x)
    let mkFloatP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadDouble null) (fun bw x -> bw.Formatter.WriteDouble null x)
    let mkInt16P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadInt16 null) (fun bw x -> bw.Formatter.WriteInt16 null x)
    let mkInt32P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadInt32 null) (fun bw x -> bw.Formatter.WriteInt32 null x)
    let mkInt64P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadInt64 null) (fun bw x -> bw.Formatter.WriteInt64 null x)
    let mkUInt16P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadUInt16 null) (fun bw x -> bw.Formatter.WriteUInt16 null x)
    let mkUInt32P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadUInt32 null) (fun bw x -> bw.Formatter.WriteUInt32 null x)
    let mkUInt64P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadUInt64 null) (fun bw x -> bw.Formatter.WriteUInt64 null x)


    // misc atomic picklers

    let mkObjPickler () = mkPickler PicklerInfo.Atomic false true (fun _ -> obj ()) (fun _ _ -> ())
    let mkUnitP () = mkPickler PicklerInfo.Atomic false false ignore (fun _ _ -> ())
    let mkDBNullPickler () = mkPickler PicklerInfo.Atomic false false (fun _ -> DBNull.Value) (fun _ _ -> ())

    let mkStringPickler () = mkPickler PicklerInfo.Atomic false false (fun br -> br.Formatter.ReadString null) 
                                                                            (fun bw x -> bw.Formatter.WriteString null x)

    let mkGuidPickler () = mkPickler PicklerInfo.Atomic false false (fun br -> Guid(br.Formatter.ReadBytesFixed null 16)) 
                                                                    (fun bw g -> bw.Formatter.WriteBytesFixed null (g.ToByteArray()))

    let mkTimeSpanPickler () = mkPickler PicklerInfo.Atomic false false (fun br -> TimeSpan(br.Formatter.ReadInt64 "ticks")) 
                                                                        (fun bw x -> bw.Formatter.WriteInt64 "ticks" x.Ticks)

    let mkDateTimePickler () = mkPickler PicklerInfo.Atomic false false (fun br -> DateTime(br.Formatter.ReadInt64 "ticks")) 
                                                                        (fun bw x -> bw.Formatter.WriteInt64 "ticks" x.Ticks)

    let mkByteArrayPickler () = mkPickler PicklerInfo.Atomic false true (fun r -> r.Formatter.ReadBytes null) (fun w x -> w.Formatter.WriteBytes null x)

    let mkBigIntPickler () = 
        mkPickler PicklerInfo.Atomic false false 
                (fun _ -> failwith "not atomic, move")
                (fun _ _ -> failwith "not atomic, move")
//                (fun r -> System.Numerics.BigInteger(r.Formatter.ReadBytes(r.Formatter.ReadInt32 "size")))
//                (fun w x -> let bs = x.ToByteArray() in w.Formatter.Write bs.Length ; w.Formatter.Write bs)


    let mkAtomicPicklers () =
        let make (f : unit -> Pickler<_>) = f () :> Pickler
        [| 
            make mkByteP ; make mkSByteP ; make mkCharP ; make mkBoolP
            make mkDecimalP ; make mkSingleP ; make mkFloatP ; make mkInt16P
            make mkInt32P ; make mkInt64P ; make mkUInt16P ; make mkUInt32P
            make mkUInt64P ; make mkObjPickler ; make mkUnitP ; make mkDBNullPickler
            make mkStringPickler ; make mkGuidPickler ; make mkTimeSpanPickler
            make mkDateTimePickler ; make mkByteArrayPickler ; make mkBigIntPickler
        |]