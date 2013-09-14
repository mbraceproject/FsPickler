module internal FsCoreSerializer.FSharpTypeFormatters

    open System
    open System.Collections.Generic
    open System.Runtime.Serialization
    open System.Runtime.Serialization.Formatters.Binary

    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.ExprShape

    open FsCoreSerializer
    open FsCoreSerializer.Utils
    open FsCoreSerializer.Reflection
    open FsCoreSerializer.FormatterUtils
    open FsCoreSerializer.BaseFormatters

    //
    //  ML core type serializers
    //

    let mkUnionFormatter (resolver : Type -> Lazy<Formatter>) (t : Type) =
        let union = new FsUnion(t, memberBindings)

        let branchFormatters = 
            union.UCIs 
            |> Array.map (fun uci -> uci.GetFields() |> Array.map (fun f -> resolver f.PropertyType))

        let inline writer (w : Writer) (o : obj) =
            let tag, fields = union.Decompose o
            w.BW.Write tag
            zipWrite w branchFormatters.[tag] fields

        let inline reader (r : Reader) =
            let tag = r.BR.ReadInt32()
            let fields = zipRead r branchFormatters.[tag]
            union.Compose(tag, fields)

        {
            Type = union.Type
            TypeInfo = getTypeInfo union.Type
            TypeHash = ObjHeader.computeHash t

            Write = writer
            Read = reader

            FormatterInfo = FormatterInfo.FSharpValue
            UseWithSubtypes = true
            CacheObj = true
        }

    let mkRecordFormatter (resolver : Type -> Lazy<Formatter>) (t : Type) =
        let record = new FsRecord(t, memberBindings)

        let formatters = record.Fields |> Array.map (fun f -> resolver f.PropertyType)

        let writer (w : Writer) (o : obj) =
            let fields = record.Decompose o
            zipWrite w formatters fields

        let reader (r : Reader) =
            let fields = zipRead r formatters
            record.Compose fields

        {
            Type = t
            TypeInfo = getTypeInfo t
            TypeHash = ObjHeader.computeHash t

            Write = writer
            Read = reader

            FormatterInfo = FormatterInfo.FSharpValue
            UseWithSubtypes = false
            CacheObj = true
        }


    let mkTupleFormatter (resolver : Type -> Lazy<Formatter>) (t : Type) =
        let tuple = new FsTuple(t)
        let formatters = tuple.Elements |> Array.map resolver

        let writer (w : Writer) (o : obj) =
            let fields = tuple.Decompose o
            zipWrite w formatters fields

        let reader (r : Reader) =
            let fields = zipRead r formatters
            tuple.Compose fields

        {
            Type = t
            TypeInfo = getTypeInfo t
            TypeHash = ObjHeader.computeHash t

            Write = writer
            Read = reader

#if OPTIMIZE_FSHARP
            FormatterInfo = FormatterInfo.FSharpValue
            // ignore subtype polymorphism checks in System.Tuple types for performance
            UseWithSubtypes = true
            CacheObj = formatters.Length > 4
#else
            FormatterInfo = FormatterInfo.Custom
            UseWithSubtypes = false
            CacheObj = true
#endif
        }

#if EMIT_IL
    let mkExceptionFormatter (resolver : Type -> Lazy<Formatter>) (t : Type) =
        let exn = new FsException(t, memberBindings)
        let formatters = exn.Fields |> Array.map (fun f -> resolver f.PropertyType)

        let writer (w : Writer) (o : obj) =
            let fields = exn.Decompose o
            zipWrite w formatters fields

        let reader (r : Reader) =
            let fields = zipRead r formatters
            exn.Compose fields

        {
            Type = t
            TypeInfo = getTypeInfo t
            TypeHash = ObjHeader.computeHash t

            Write = writer
            Read = reader

            FormatterInfo = FormatterInfo.FSharpValue
            UseWithSubtypes = false
            CacheObj = true
        }
#endif

    //
    //  F# generic types
    //

    type ListFormatter () =
        interface IGenericFormatterFactory1 with
            member __.Create<'T> (resolver : IFormatterResolver) =
                let ef = resolver.Resolve<'T> ()

                let writer (w : Writer) (l : 'T list) =
                    let ef = unpack ef

                    if ef.TypeInfo = TypeInfo.Primitive then
                        let arr = List.toArray l
                        w.BW.Write arr.Length
                        do Stream.WriteArray(w.BW.BaseStream, arr)
                    else
                        let rec writeL (xs : 'T list) =
                            match xs with
                            | hd :: tl -> write w ef hd ; writeL tl
                            | [] -> ()

                        w.BW.Write l.Length
                        writeL l

                let reader (r : Reader) =
                    let length = r.BR.ReadInt32 ()
                    let ef = unpack ef

                    if ef.TypeInfo = TypeInfo.Primitive then
                        let array = Array.zeroCreate<'T> length
                        Stream.CopyToArray(r.BR.BaseStream, array)
                        Array.toList array
                    else
                        let array = Array.zeroCreate<'T> length
                        for i = 0 to length - 1 do
                            array.[i] <- read r ef :?> 'T
                                    
                        Array.toList array

                mkFormatter FormatterInfo.Custom false true reader writer

    type PairFormatter () =
        interface IGenericFormatterFactory2 with
            member __.Create<'T,'S> (resolver : IFormatterResolver) =
                let tf, sf = resolver.Resolve<'T> (), resolver.Resolve<'S> ()
                
                let writer (w : Writer) ((t,s) : 'T * 'S) =
                    write w (unpack tf) t ; write w (unpack sf) s

                let reader (r : Reader) =
                    (read r (unpack tf) :?> 'T, read r (unpack sf) :?> 'S)

#if OPTIMIZE_FSHARP
                // do not cache or apply subtype polymorphism for performance
                mkFormatter FormatterInfo.FSharpValue true false reader writer
#else
                mkFormatter FormatterInfo.Custom false false reader writer
#endif

    type TripleFormatter () =
        interface IGenericFormatterFactory3 with
            member __.Create<'T1, 'T2, 'T3> (resolver : IFormatterResolver) =
                let f1, f2, f3 = resolver.Resolve<'T1>(), resolver.Resolve<'T2>(), resolver.Resolve<'T3>()
                let inline writer (w : Writer) ((t1,t2,t3) : 'T1 * 'T2 * 'T3) =
                    write w (unpack f1) t1 ; write w (unpack f2) t2 ; write w (unpack f3) t3

                let inline reader (r : Reader) =
                    (read r (unpack f1) :?> 'T1, read r (unpack f2) :?> 'T2, read r (unpack f3) :?> 'T3)

#if OPTIMIZE_FSHARP
                // do not cache or apply subtype polymorphism for performance
                mkFormatter FormatterInfo.FSharpValue true false reader writer
#else
                mkFormatter FormatterInfo.Custom false false reader writer
#endif 

    type OptionFormatter () =
        interface IGenericFormatterFactory1 with
            member __.Create<'T> (resolver : IFormatterResolver) =
                let ef = resolver.Resolve<'T> ()

                let writer (w : Writer) (x : 'T option) =
                    match x with
                    | None -> w.BW.Write true
                    | Some v -> w.BW.Write false ; write w (unpack ef) v

                let reader (r : Reader) =
                    if r.BR.ReadBoolean() then None
                    else
                        Some(read r (unpack ef) :?> 'T)

#if OPTIMIZE_FSHARP
                // do not cache or apply subtype polymorphism for performance
                mkFormatter FormatterInfo.FSharpValue true false reader writer
#else
                mkFormatter FormatterInfo.Custom false false reader writer
#endif 

    type RefFormatter () =
        interface IGenericFormatterFactory1 with
            member __.Create<'T> (resolver : IFormatterResolver) =
                let ef = resolver.Resolve<'T> ()

                let writer (w : Writer) (r : 'T ref) =
                    write w (unpack ef) r.Value

                let reader (r : Reader) =
                    read r (unpack ef) :?> 'T |> ref

                // do not cache for performance
                mkFormatter FormatterInfo.FSharpValue false false reader writer


    type SetFormatter () =
        interface IGenericFormatterFactory
        member __.Create<'T when 'T : comparison> (resolver : IFormatterResolver) =
            let ef = resolver.Resolve<'T>()

            let writer (w : Writer) (s : Set<'T>) = 
                writeSeq w (unpack ef) s.Count s

            let reader (r : Reader) =
                readSeq<'T> r (unpack ef) |> Set.ofArray

            mkFormatter FormatterInfo.Custom false true reader writer 

    type MapFormatter () =
        interface IGenericFormatterFactory
        member __.Create<'K, 'V when 'K : comparison> (resolver : IFormatterResolver) =
            let kf, vf = resolver.Resolve<'K> (), resolver.Resolve<'V> ()
                
            let writer (w : Writer) (m : Map<'K,'V>) =
                writeKVPair w (unpack kf) (unpack vf) m.Count (Map.toSeq m)

            let reader (r : Reader) =
                readKVPair<'K,'V> r (unpack kf) (unpack vf) |> Map.ofArray

            mkFormatter FormatterInfo.Custom false true reader writer


    type DictionaryFormatter () =
        interface IGenericFormatterFactory
        member __.Create<'K, 'V when 'K : comparison> (resolver : IFormatterResolver) =
            let kf, vf = resolver.Resolve<'K>(), resolver.Resolve<'V>()
                
            let writer (w : Writer) (d : Dictionary<'K,'V>) =
                let kvs = Seq.map (fun (KeyValue (k,v)) -> k,v) d
                writeKVPair w (unpack kf) (unpack vf) d.Count kvs
            let reader (r : Reader) =
                let kvs = readKVPair<'K,'V> r (unpack kf) (unpack vf)
                let d = new Dictionary<'K,'V>()
                for i = 0 to kvs.Length - 1 do
                    let k,v = kvs.[i]
                    d.Add(k,v)
                d

            mkFormatter FormatterInfo.Custom false true reader writer


    type ExprFormatter () =
        interface IGenericFormatterFactory1 with
            member __.Create<'T> (resolver : IFormatterResolver) =
                let exprFormatter = resolver.Resolve<Expr> ()
                mkFormatter FormatterInfo.Custom false true 
                                (fun r -> Expr.Cast<'T>(r.Read exprFormatter))
                                (fun w e -> w.Write(exprFormatter, e :> _))


    let mkFSharpGenericFormatters () =
        [
            new ListFormatter() :> IGenericFormatterFactory
            new PairFormatter() :> _
            new TripleFormatter() :> _
            new OptionFormatter() :> _
            new RefFormatter() :> _
            new ExprFormatter() :> _
            new SetFormatter() :> _
            new MapFormatter() :> _
            new DictionaryFormatter() :> _
        ]