module internal Nessos.FsPickler.BasePicklers

    open System
    open System.Reflection
    open System.Collections.Generic
    open System.Runtime.Serialization

    open Nessos.FsPickler
    open Nessos.FsPickler.Utils
    open Nessos.FsPickler.PicklerUtils

    // primitive picklers : nativeint & unativeint excluded

    let mkByteP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.BinaryReader.ReadByte()) (fun bw x -> bw.BinaryWriter.Write x)
    let mkSByteP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.BinaryReader.ReadSByte()) (fun bw x -> bw.BinaryWriter.Write x)
    let mkCharP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.BinaryReader.ReadChar()) (fun bw x -> bw.BinaryWriter.Write x)
    let mkBoolP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.BinaryReader.ReadBoolean()) (fun bw x -> bw.BinaryWriter.Write x)
    let mkDecimalP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.BinaryReader.ReadDecimal()) (fun bw x -> bw.BinaryWriter.Write x)
    let mkSingleP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.BinaryReader.ReadSingle()) (fun bw x -> bw.BinaryWriter.Write x)
    let mkFloatP () = mkPickler PicklerInfo.Atomic false false (fun br -> br.BinaryReader.ReadDouble()) (fun bw x -> bw.BinaryWriter.Write x)
    let mkInt16P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.BinaryReader.ReadInt16()) (fun bw x -> bw.BinaryWriter.Write x)
    let mkInt32P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.BinaryReader.ReadInt32()) (fun bw x -> bw.BinaryWriter.Write x)
    let mkInt64P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.BinaryReader.ReadInt64()) (fun bw x -> bw.BinaryWriter.Write x)
    let mkUInt16P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.BinaryReader.ReadUInt16()) (fun bw x -> bw.BinaryWriter.Write x)
    let mkUInt32P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.BinaryReader.ReadUInt32()) (fun bw x -> bw.BinaryWriter.Write x)
    let mkUInt64P () = mkPickler PicklerInfo.Atomic false false (fun br -> br.BinaryReader.ReadUInt64()) (fun bw x -> bw.BinaryWriter.Write x)


    // misc atomic picklers

    let mkObjPickler () = mkPickler PicklerInfo.Atomic false true (fun _ -> obj ()) (fun _ _ -> ())
    let mkUnitP () = mkPickler PicklerInfo.Atomic false false ignore (fun _ _ -> ())
    let mkDBNullPickler () = mkPickler PicklerInfo.Atomic false false (fun _ -> DBNull.Value) (fun _ _ -> ())

    let mkStringPickler () = mkPickler PicklerInfo.Atomic false false (fun br -> br.BinaryReader.ReadString()) 
                                                                            (fun bw x -> bw.BinaryWriter.Write x)

    let mkGuidPickler () = mkPickler PicklerInfo.Atomic false false (fun br -> Guid(br.BinaryReader.ReadBytes(16))) 
                                                                    (fun bw x -> bw.BinaryWriter.Write(x.ToByteArray()))

    let mkTimeSpanPickler () = mkPickler PicklerInfo.Atomic false false (fun br -> TimeSpan(br.BinaryReader.ReadInt64())) 
                                                                        (fun bw x -> bw.BinaryWriter.Write(x.Ticks))

    let mkDateTimePickler () = mkPickler PicklerInfo.Atomic false false (fun br -> DateTime(br.BinaryReader.ReadInt64())) 
                                                                        (fun bw x -> bw.BinaryWriter.Write(x.Ticks))

    let mkByteArrayPickler () = mkPickler PicklerInfo.Atomic false true 
                                            (fun br -> br.BinaryReader.ReadBytes(br.BinaryReader.ReadInt32())) 
                                            (fun bw x -> bw.BinaryWriter.Write x.Length ; bw.BinaryWriter.Write x)

    let mkBigIntPickler () = 
        mkPickler PicklerInfo.Atomic false false 
                (fun r -> System.Numerics.BigInteger(r.BinaryReader.ReadBytes(r.BinaryReader.ReadInt32())))
                (fun w x -> let bs = x.ToByteArray() in w.BinaryWriter.Write bs.Length ; w.BinaryWriter.Write bs)


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