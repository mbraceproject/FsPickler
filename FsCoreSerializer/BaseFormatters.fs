module internal FsCoreSerializer.BaseFormatters

    open System
    open System.Reflection
    open System.Linq.Expressions
    open System.IO
    open System.Collections.Generic
    open System.Collections.Concurrent
    open System.Runtime.Serialization
    open System.Runtime.CompilerServices

    open FsCoreSerializer
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
            mkFormatter FormatterInfo.Atomic false false (fun br -> Guid(br.BR.ReadBytes(16))) (fun bw x -> bw.BW.Write(x.ToByteArray())) :> _
            mkFormatter FormatterInfo.Atomic false false (fun br -> TimeSpan(br.BR.ReadInt64())) (fun bw x -> bw.BW.Write(x.Ticks)) :> _
            mkFormatter FormatterInfo.Atomic false false (fun br -> DateTime(br.BR.ReadInt64())) (fun bw x -> bw.BW.Write(x.Ticks)) :> _
            mkFormatter FormatterInfo.Atomic false true (fun br -> br.BR.ReadBytes(br.BR.ReadInt32())) (fun bw x -> bw.BW.Write x.Length ; bw.BW.Write x) :> _
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
                try t.GetMembers(memberBindings) |> Array.find (fun m -> m.ToString() = mname)
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

    let mkAssemblyNameFormatter () =
        mkFormatter FormatterInfo.ReflectionType true true
                (fun r -> AssemblyName(r.BR.ReadString()))
                (fun w a -> w.BW.Write a.FullName)

    let mkAssemblyFormatter () =
        let writer (w : Writer) (a : Assembly) = w.BW.Write(a.FullName)
        let reader (r : Reader) = Assembly.Load(r.BR.ReadString())

        mkFormatter FormatterInfo.ReflectionType true true reader writer

    // Quotations serialization code

//    open Microsoft.FSharp.Quotations
//    open Microsoft.FSharp.Quotations.ExprShape
//
//    let mkVarFormatter (typeFormatter : Formatter<Type>) =
//        let writer (w : Writer) (v : Var) =
//            w.BW.Write v.Name
//            w.Write(typeFormatter, v.Type)
//            w.BW.Write v.IsMutable
//
//        let reader (r : Reader) =
//            let name = r.BR.ReadString()
//            let t = r.Read typeFormatter
//            let isMutable = r.BR.ReadBoolean()
//            Var(name, t, isMutable)
//
//        // caching vars by reference is essential for quotation deserialization!
//        mkFormatter FormatterInfo.Custom false true reader writer
//    
//    let mkExprFormatter (varFormatter : Formatter<Var>) =
//        let combType =
//            match Expr.Value 2 with
//            | ShapeCombination(o,_) -> o.GetType()
//            | _ -> failwith "impossible"
//
//        let rec writer (w : Writer) (e : Expr) =
//            match e with
//            | ShapeVar v -> 
//                w.BW.Write 0uy
//                w.Write(varFormatter, v)
//            | ShapeLambda(v, e) ->
//                w.BW.Write 1uy
//                w.Write(varFormatter, v)
//                writer w e
//            | ShapeCombination(o, exprs) ->
//                w.BW.Write 2uy
//                w.WriteObj(combType, o)
//                w.BW.Write exprs.Length
//                for e in exprs do writer w e
//
//        let rec reader (r : Reader) =
//            match r.BR.ReadByte() with
//            | 0uy -> let v = read r varFormatter :?> Var in Expr.Var v
//            | 1uy -> 
//                let v = read r varFormatter :?> Var
//                let e = reader r
//                Expr.Lambda(v, e)
//            | 2uy ->
//                let o = r.ReadObj combType
//                let n = r.BR.ReadInt32()
//                let arr = Array.zeroCreate<Expr> n
//                for i = 0 to n - 1 do arr.[i] <- reader r
//                RebuildShapeCombination(o, Array.toList arr)
//            | _ -> raise <| new SerializationException("Stream error: cannot deserialize quotation.")
//
//        mkFormatter FormatterInfo.Custom false true reader writer

    let mkReflectionFormatters tyConv =
        let typeFormatter = mkTypeFormatter tyConv
        let miFmt = mkMemberInfoFormatter typeFormatter
//        let varFormatter = mkVarFormatter typeFormatter
        [
            typeFormatter :> Formatter ; miFmt :> _ ; mkAssemblyFormatter () :> _ ; mkAssemblyNameFormatter () :> _
            mkTypeHandleFormatter typeFormatter :> _; mkFieldHandleFormatter miFmt :> _; mkMethodHandleFormatter miFmt :> _
//            varFormatter; mkExprFormatter varFormatter
        ]