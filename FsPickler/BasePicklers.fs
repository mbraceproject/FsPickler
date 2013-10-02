module internal FsPickler.BasePicklers

    open System
    open System.Reflection
    open System.Collections.Generic
    open System.Runtime.Serialization

    open FsPickler
    open FsPickler.Utils
    open FsPickler.PicklerUtils

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
        [ 
            // primitives
            mkByteP () :> Pickler ; mkSByteP () :> _ ; mkCharP () :> _ ; mkBoolP () :> _ ; mkDecimalP () :> _ ;
            mkSingleP () :> _ ; mkFloatP () :> _ ; mkInt16P () :> _ ; mkInt32P () :> _ ; mkInt64P () :> _ ;
            mkUInt16P () :> _ ; mkUInt32P () :> _ ; mkUInt64P () :> _
            // misc atomic picklers
            mkObjPickler () :> _ ; mkUnitP () :> _ ; mkDBNullPickler () :> _ ; mkStringPickler () :> _ ;
            mkGuidPickler () :> _ ; mkTimeSpanPickler () :> _ ; mkDateTimePickler () :> _ ;
            mkByteArrayPickler () :> _ ; mkBigIntPickler () :> _
        ]


    //
    //  Reflection type picklers
    //

    let mkTypePickler (tyConv : ITypeNameConverter) =
        let writer (w : Writer) (t : Type) =
            if t.IsGenericParameter then
                tyConv.WriteQualifiedName w.BinaryWriter t.ReflectedType
                w.BinaryWriter.Write true
                w.BinaryWriter.Write t.Name
            else
                tyConv.WriteQualifiedName w.BinaryWriter t
                w.BinaryWriter.Write false

        let reader (r : Reader) =
            let t = tyConv.ReadQualifiedName r.BinaryReader
            if r.BinaryReader.ReadBoolean() then
                // is generic parameter
                let pname = r.BinaryReader.ReadString()
                try t.GetGenericArguments() |> Array.find(fun a -> a.Name = pname)
                with :? KeyNotFoundException -> 
                    raise <| new SerializationException(sprintf "cannot deserialize type '%s'." pname)
            else
                t

        mkPickler PicklerInfo.ReflectionType true true reader writer

    let mkMemberInfoPickler typePickler =
        let writer (w : Writer) (m : MemberInfo) =
            w.Write(typePickler, m.ReflectedType)
            match m with
            | :? MethodInfo as m when m.IsGenericMethod && not m.IsGenericMethodDefinition ->
                let gm = m.GetGenericMethodDefinition()
                let ga = m.GetGenericArguments()
                w.BinaryWriter.Write (gm.ToString())

                w.BinaryWriter.Write true
                w.BinaryWriter.Write ga.Length
                for a in ga do w.Write(typePickler, a)
            | _ ->
                w.BinaryWriter.Write (m.ToString())
                w.BinaryWriter.Write false

        let reader (r : Reader) =
            let t = r.Read typePickler
            let mname = r.BinaryReader.ReadString()
            let m = 
                try t.GetMembers(allMembers) |> Array.find (fun m -> m.ToString() = mname)
                with :? KeyNotFoundException ->
                    raise <| new SerializationException(sprintf "Could not deserialize member '%O.%s'" t.Name mname)

            if r.BinaryReader.ReadBoolean() then
                let n = r.BinaryReader.ReadInt32()
                let ga = Array.zeroCreate<Type> n
                for i = 0 to n - 1 do ga.[i] <- r.Read typePickler
                (m :?> MethodInfo).MakeGenericMethod ga :> MemberInfo
            else
                m

        mkPickler PicklerInfo.ReflectionType true true reader writer

    let mkTypeHandlePickler (typePickler : Pickler<Type>) =
        mkPickler PicklerInfo.ReflectionType true true 
                (fun r -> let t = r.Read typePickler in t.TypeHandle)
                (fun w th -> w.Write(typePickler, Type.GetTypeFromHandle th))

    let mkFieldHandlePickler (memberInfoPickler : Pickler<MemberInfo>) =
        mkPickler PicklerInfo.ReflectionType true true
                (fun r -> let f = r.Read memberInfoPickler :?> FieldInfo in f.FieldHandle)
                (fun w fh -> w.Write(memberInfoPickler, FieldInfo.GetFieldFromHandle fh :> _))

    let mkMethodHandlePickler (memberInfoPickler : Pickler<MemberInfo>) =
        mkPickler PicklerInfo.ReflectionType true true
                (fun r -> let m = read false r memberInfoPickler :?> MethodInfo in m.MethodHandle)
                (fun w mh -> w.Write(memberInfoPickler, MethodInfo.GetMethodFromHandle mh :> _))

    let mkReflectionPicklers tyConv =
        let typePickler = mkTypePickler tyConv
        let memberPickler = mkMemberInfoPickler typePickler
        [
            typePickler :> Pickler ; memberPickler :> _ ; mkTypeHandlePickler typePickler :> _; 
            mkFieldHandlePickler memberPickler :> _; mkMethodHandlePickler memberPickler :> _
        ]