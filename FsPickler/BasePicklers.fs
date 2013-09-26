module internal FsPickler.BasePicklers

    open System
    open System.Reflection
    open System.Collections.Generic
    open System.Runtime.Serialization

    open FsPickler
    open FsPickler.Utils
    open FsPickler.PicklerUtils

    let mkPrimitivePicklers () =
        [   
            mkPickler PicklerInfo.Atomic false false ignore (fun _ _ -> ()) :> Pickler
            mkPickler PicklerInfo.Atomic false false (fun br -> br.BR.ReadByte()) (fun bw x -> bw.BW.Write x) :> _
            mkPickler PicklerInfo.Atomic false false (fun br -> br.BR.ReadSByte()) (fun bw x -> bw.BW.Write x) :> _
            mkPickler PicklerInfo.Atomic false false (fun br -> br.BR.ReadChar()) (fun bw x -> bw.BW.Write x) :> _
            mkPickler PicklerInfo.Atomic false false (fun br -> br.BR.ReadBoolean()) (fun bw x -> bw.BW.Write x) :> _
            mkPickler PicklerInfo.Atomic false false (fun br -> br.BR.ReadDecimal()) (fun bw x -> bw.BW.Write x) :> _
            mkPickler PicklerInfo.Atomic false false (fun br -> br.BR.ReadSingle()) (fun bw x -> bw.BW.Write x) :> _
            mkPickler PicklerInfo.Atomic false false (fun br -> br.BR.ReadDouble()) (fun bw x -> bw.BW.Write x) :> _
            mkPickler PicklerInfo.Atomic false false (fun br -> br.BR.ReadInt16()) (fun bw x -> bw.BW.Write x) :> _
            mkPickler PicklerInfo.Atomic false false (fun br -> br.BR.ReadInt32()) (fun bw x -> bw.BW.Write x) :> _
            mkPickler PicklerInfo.Atomic false false (fun br -> br.BR.ReadInt64()) (fun bw x -> bw.BW.Write x) :> _
            mkPickler PicklerInfo.Atomic false false (fun br -> br.BR.ReadUInt16()) (fun bw x -> bw.BW.Write x) :> _
            mkPickler PicklerInfo.Atomic false false (fun br -> br.BR.ReadUInt32()) (fun bw x -> bw.BW.Write x) :> _ 
            mkPickler PicklerInfo.Atomic false false (fun br -> br.BR.ReadUInt64()) (fun bw x -> bw.BW.Write x) :> _
        ]


    let mkAtomicPicklers () =
        [
            mkPickler PicklerInfo.Atomic false true (fun _ -> obj ()) (fun _ _ -> ()) :> Pickler
            mkPickler PicklerInfo.Atomic false false (fun br -> br.BR.ReadString()) (fun bw x -> bw.BW.Write x) :> Pickler
            mkPickler PicklerInfo.Atomic false false (fun br -> Guid(br.BR.ReadBytes(16))) 
                                                            (fun bw x -> bw.BW.Write(x.ToByteArray())) :> _
            mkPickler PicklerInfo.Atomic false false (fun br -> TimeSpan(br.BR.ReadInt64())) 
                                                            (fun bw x -> bw.BW.Write(x.Ticks)) :> _
            mkPickler PicklerInfo.Atomic false false (fun br -> DateTime(br.BR.ReadInt64())) 
                                                            (fun bw x -> bw.BW.Write(x.Ticks)) :> _
            mkPickler PicklerInfo.Atomic false true (fun br -> br.BR.ReadBytes(br.BR.ReadInt32())) 
                                                        (fun bw x -> bw.BW.Write x.Length ; bw.BW.Write x) :> _
            mkPickler PicklerInfo.Atomic false false (fun _ -> DBNull.Value) (fun _ _ -> ()) :> _
            mkPickler PicklerInfo.Atomic false false (fun r -> System.Numerics.BigInteger(r.BR.ReadBytes(r.BR.ReadInt32())))
                                                         (fun w x -> let bs = x.ToByteArray() in w.BW.Write bs.Length ; w.BW.Write bs) :> _
        ]


    //
    //  Reflection formatters
    //

    let mkTypePickler (tyConv : ITypeNameConverter) =
        mkPickler PicklerInfo.ReflectionType true true 
                    (fun r -> TypePickler.read tyConv r.BR) 
                    (fun w t -> TypePickler.write tyConv w.BW t)

    let mkMemberInfoPickler typePickler =
        let writer (w : Writer) (m : MemberInfo) =
            w.Write(typePickler, m.ReflectedType)
            match m with
            | :? MethodInfo as m when m.IsGenericMethod && not m.IsGenericMethodDefinition ->
                let gm = m.GetGenericMethodDefinition()
                let ga = m.GetGenericArguments()
                w.BW.Write (gm.ToString())

                w.BW.Write true
                w.BW.Write ga.Length
                for a in ga do w.Write(typePickler, a)
            | _ ->
                w.BW.Write (m.ToString())
                w.BW.Write false

        let reader (r : Reader) =
            let t = r.Read typePickler
            let mname = r.BR.ReadString()
            let m = 
                try t.GetMembers(allMembers) |> Array.find (fun m -> m.ToString() = mname)
                with :? KeyNotFoundException ->
                    raise <| new SerializationException(sprintf "Could not deserialize member '%O.%s'" t.Name mname)

            if r.BR.ReadBoolean() then
                let n = r.BR.ReadInt32()
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