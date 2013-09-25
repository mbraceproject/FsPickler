module internal FsCoreSerializer.BaseFormatters

    open System
    open System.Reflection
    open System.Collections.Generic
    open System.Runtime.Serialization

    open FsCoreSerializer
    open FsCoreSerializer.Utils
    open FsCoreSerializer.FormatterUtils

    let mkPrimitiveFormatters () =
        [   
            mkFormatter FormatterInfo.Atomic false false ignore (fun _ _ -> ()) :> Formatter
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadByte()) (fun bw x -> bw.BW.Write x) :> _
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadSByte()) (fun bw x -> bw.BW.Write x) :> _
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadChar()) (fun bw x -> bw.BW.Write x) :> _
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadBoolean()) (fun bw x -> bw.BW.Write x) :> _
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadDecimal()) (fun bw x -> bw.BW.Write x) :> _
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadSingle()) (fun bw x -> bw.BW.Write x) :> _
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadDouble()) (fun bw x -> bw.BW.Write x) :> _
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadInt16()) (fun bw x -> bw.BW.Write x) :> _
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadInt32()) (fun bw x -> bw.BW.Write x) :> _
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadInt64()) (fun bw x -> bw.BW.Write x) :> _
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadUInt16()) (fun bw x -> bw.BW.Write x) :> _
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadUInt32()) (fun bw x -> bw.BW.Write x) :> _ 
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadUInt64()) (fun bw x -> bw.BW.Write x) :> _
        ]


    let mkAtomicFormatters () =
        [
            mkFormatter FormatterInfo.Atomic false true (fun _ -> obj ()) (fun _ _ -> ()) :> Formatter
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadString()) (fun bw x -> bw.BW.Write x) :> Formatter
            mkFormatter FormatterInfo.Atomic false false (fun br -> Guid(br.BR.ReadBytes(16))) 
                                                            (fun bw x -> bw.BW.Write(x.ToByteArray())) :> _
            mkFormatter FormatterInfo.Atomic false false (fun br -> TimeSpan(br.BR.ReadInt64())) 
                                                            (fun bw x -> bw.BW.Write(x.Ticks)) :> _
            mkFormatter FormatterInfo.Atomic false false (fun br -> DateTime(br.BR.ReadInt64())) 
                                                            (fun bw x -> bw.BW.Write(x.Ticks)) :> _
            mkFormatter FormatterInfo.Atomic false true (fun br -> br.BR.ReadBytes(br.BR.ReadInt32())) 
                                                        (fun bw x -> bw.BW.Write x.Length ; bw.BW.Write x) :> _
            mkFormatter FormatterInfo.Atomic false false (fun _ -> DBNull.Value) (fun _ _ -> ()) :> _
            mkFormatter FormatterInfo.Atomic false false (fun r -> System.Numerics.BigInteger(r.BR.ReadBytes(r.BR.ReadInt32())))
                                                         (fun w x -> let bs = x.ToByteArray() in w.BW.Write bs.Length ; w.BW.Write bs) :> _
        ]


    //
    //  Reflection formatters
    //

    let mkTypeFormatter (tyConv : ITypeNameConverter) =
        mkFormatter FormatterInfo.ReflectionType true true 
                    (fun r -> TypeFormatter.read tyConv r.BR) 
                    (fun w t -> TypeFormatter.write tyConv w.BW t)

    let mkMemberInfoFormatter typeFormatter =
        let writer (w : Writer) (m : MemberInfo) =
            w.Write(typeFormatter, m.ReflectedType)
            match m with
            | :? MethodInfo as m when m.IsGenericMethod && not m.IsGenericMethodDefinition ->
                let gm = m.GetGenericMethodDefinition()
                let ga = m.GetGenericArguments()
                w.BW.Write (gm.ToString())

                w.BW.Write true
                w.BW.Write ga.Length
                for a in ga do w.Write(typeFormatter, a)
            | _ ->
                w.BW.Write (m.ToString())
                w.BW.Write false

        let reader (r : Reader) =
            let t = r.Read typeFormatter
            let mname = r.BR.ReadString()
            let m = 
                try t.GetMembers(allMembers) |> Array.find (fun m -> m.ToString() = mname)
                with :? KeyNotFoundException ->
                    raise <| new SerializationException(sprintf "Could not deserialize member '%O.%s'" t.Name mname)

            if r.BR.ReadBoolean() then
                let n = r.BR.ReadInt32()
                let ga = Array.zeroCreate<Type> n
                for i = 0 to n - 1 do ga.[i] <- r.Read typeFormatter
                (m :?> MethodInfo).MakeGenericMethod ga :> MemberInfo
            else
                m

        mkFormatter FormatterInfo.ReflectionType true true reader writer

    let mkTypeHandleFormatter (typeFormatter : Formatter<Type>) =
        mkFormatter FormatterInfo.ReflectionType true true 
                (fun r -> let t = r.Read typeFormatter in t.TypeHandle)
                (fun w th -> w.Write(typeFormatter, Type.GetTypeFromHandle th))

    let mkFieldHandleFormatter (memberInfoFormatter : Formatter<MemberInfo>) =
        mkFormatter FormatterInfo.ReflectionType true true
                (fun r -> let f = r.Read memberInfoFormatter :?> FieldInfo in f.FieldHandle)
                (fun w fh -> w.Write(memberInfoFormatter, FieldInfo.GetFieldFromHandle fh :> _))

    let mkMethodHandleFormatter (memberInfoFormatter : Formatter<MemberInfo>) =
        mkFormatter FormatterInfo.ReflectionType true true
                (fun r -> let m = read false r memberInfoFormatter :?> MethodInfo in m.MethodHandle)
                (fun w mh -> w.Write(memberInfoFormatter, MethodInfo.GetMethodFromHandle mh :> _))

    let mkReflectionFormatters tyConv =
        let typeFormatter = mkTypeFormatter tyConv
        let memberFormatter = mkMemberInfoFormatter typeFormatter
        [
            typeFormatter :> Formatter ; memberFormatter :> _ ; mkTypeHandleFormatter typeFormatter :> _; 
            mkFieldHandleFormatter memberFormatter :> _; mkMethodHandleFormatter memberFormatter :> _
        ]