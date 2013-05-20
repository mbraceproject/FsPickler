module internal FsCoreSerializer.FSharpFormatters

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
    open FsCoreSerializer.BaseFormatters
    open FsCoreSerializer.BaseFormatters.Utils
    open FsCoreSerializer

    //
    //  ML core type serializers
    //

    let mkUnionFormatter (resolver : Type -> Lazy<Formatter>) (t : Type) =
        let union = new FsUnion(t, allMembers)

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
            Type = t
            TypeInfo = getTypeInfo t
            TypeHash = ObjHeader.getTruncatedHash t

            Write = writer
            Read = reader

            FormatterInfo = FormatterInfo.FSharpValue
            UseWithSubtypes = true
            CacheObj = true
        }

    let mkRecordFormatter (resolver : Type -> Lazy<Formatter>) (t : Type) =
        let record = new FsRecord(t, allMembers)

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
            TypeHash = ObjHeader.getTruncatedHash t

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
            TypeHash = ObjHeader.getTruncatedHash t

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

    let mkExceptionFormatter (resolver : Type -> Lazy<Formatter>) (t : Type) =
        let exn = new FsException(t, allMembers)
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
            TypeHash = ObjHeader.getTruncatedHash t

            Write = writer
            Read = reader

            FormatterInfo = FormatterInfo.FSharpValue
            UseWithSubtypes = false
            CacheObj = true
        }

    //
    //  F# generic types
    //

    type ListFormatter () =
        interface IGenericFormatterFactory1 with
            member __.Create<'T> (resolver : Type -> Lazy<Formatter>) =
                let ef = resolver typeof<'T>

                let writer (w : Writer) (l : 'T list) =
                    let ef = ef.Value

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
                    let ef = ef.Value

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
            member __.Create<'T,'S> (resolver : Type -> Lazy<Formatter>) =
                let tf, sf = resolver typeof<'T>, resolver typeof<'S>
                
                let writer (w : Writer) ((t,s) : 'T * 'S) =
                    write w tf.Value t ; write w sf.Value s

                let reader (r : Reader) =
                    (read r tf.Value :?> 'T, read r sf.Value :?> 'S)

#if OPTIMIZE_FSHARP
                // do not cache or apply subtype polymorphism for performance
                mkFormatter FormatterInfo.FSharpValue true false reader writer
#else
                mkFormatter FormatterInfo.Custom false false reader writer
#endif

    type TripleFormatter () =
        interface IGenericFormatterFactory3 with
            member __.Create<'T1, 'T2, 'T3> (resolver : Type -> Lazy<Formatter>) =
                let t1f, t2f, t3f = resolver typeof<'T1>, resolver typeof<'T2>, resolver typeof<'T3>
                let inline writer (w : Writer) ((t1,t2,t3) : 'T1 * 'T2 * 'T3) =
                    write w t1f.Value t1 ; write w t2f.Value t2 ; write w t3f.Value t3

                let inline reader (r : Reader) =
                    (read r t1f.Value :?> 'T1, read r t2f.Value :?> 'T2, read r t3f.Value :?> 'T3)

#if OPTIMIZE_FSHARP
                // do not cache or apply subtype polymorphism for performance
                mkFormatter FormatterInfo.FSharpValue true false reader writer
#else
                mkFormatter FormatterInfo.Custom false false reader writer
#endif 

    type OptionFormatter () =
        interface IGenericFormatterFactory1 with
            member __.Create<'T> (resolver : Type -> Lazy<Formatter>) =
                let ef = resolver typeof<'T>

                let writer (w : Writer) (x : 'T option) =
                    match x with
                    | None -> w.BW.Write true
                    | Some v -> w.BW.Write false ; write w ef.Value v

                let reader (r : Reader) =
                    if r.BR.ReadBoolean() then None
                    else
                        Some(read r ef.Value :?> 'T)

#if OPTIMIZE_FSHARP
                // do not cache or apply subtype polymorphism for performance
                mkFormatter FormatterInfo.FSharpValue true false reader writer
#else
                mkFormatter FormatterInfo.Custom false false reader writer
#endif 

    type RefFormatter () =
        interface IGenericFormatterFactory1 with
            member __.Create<'T> (resolver : Type -> Lazy<Formatter>) =
                let ef = resolver typeof<'T>

                let writer (w : Writer) (r : 'T ref) =
                    write w ef.Value r.Value

                let reader (r : Reader) =
                    read r ef.Value :?> 'T |> ref

                // do not cache for performance
                mkFormatter FormatterInfo.FSharpValue false false reader writer


    type SetFormatter () =
        interface IGenericFormatterFactory
        member __.Create<'T when 'T : comparison> (resolver : Type -> Lazy<Formatter>) =
            let ef = resolver typeof<'T>

            let writer (w : Writer) (s : Set<'T>) = 
                writeSeq w ef.Value s.Count s

            let reader (r : Reader) =
                readSeq<'T> r ef.Value |> Set.ofArray

            mkFormatter FormatterInfo.Custom false true reader writer 

    type MapFormatter () =
        interface IGenericFormatterFactory
        member __.Create<'K, 'V when 'K : comparison> (resolver : Type -> Lazy<Formatter>) =
            let kf, vf = resolver typeof<'K>, resolver typeof<'V>
                
            let writer (w : Writer) (m : Map<'K,'V>) =
                writeKVPair w kf.Value vf.Value m.Count (Map.toSeq m)

            let reader (r : Reader) =
                readKVPair<'K,'V> r kf.Value vf.Value |> Map.ofArray

            mkFormatter FormatterInfo.Custom false true reader writer


    type DictionaryFormatter () =
        interface IGenericFormatterFactory
        member __.Create<'K, 'V when 'K : comparison> (resolver : Type -> Lazy<Formatter>) =
            let kf, vf = resolver typeof<'K>, resolver typeof<'V>
                
            let writer (w : Writer) (d : Dictionary<'K,'V>) =
                let kvs = Seq.map (fun (KeyValue (k,v)) -> k,v) d
                writeKVPair w kf.Value vf.Value d.Count kvs
            let reader (r : Reader) =
                let kvs = readKVPair<'K,'V> r kf.Value vf.Value
                let d = new Dictionary<'K,'V>()
                for i = 0 to kvs.Length - 1 do
                    let k,v = kvs.[i]
                    d.Add(k,v)
                d

            mkFormatter FormatterInfo.Custom false true reader writer


    let varFormatter =
        let writer (w : Writer) (v : Var) =
            w.BW.Write v.Name
            write w typeFormatter v.Type
            w.BW.Write v.IsMutable

        let reader (r : Reader) =
            let name = r.BR.ReadString()
            let t = read r typeFormatter :?> Type
            let isMutable = r.BR.ReadBoolean()
            Var(name, t, isMutable)

        // caching vars by reference is essential for quotation deserialization!
        mkFormatter FormatterInfo.Custom false true reader writer

    
    let exprFormatter =
        let combType =
            match Expr.Value 2 with
            | ShapeCombination(o,_) -> o.GetType()
            | _ -> failwith "impossible"

        let rec writer (w : Writer) (e : Expr) =
            match e with
            | ShapeVar v -> 
                w.BW.Write 0uy
                write w varFormatter v
            | ShapeLambda(v, e) ->
                w.BW.Write 1uy
                write w varFormatter v
                writer w e
            | ShapeCombination(o, exprs) ->
                w.BW.Write 2uy
                w.WriteObj(combType, o)
                w.BW.Write exprs.Length
                for e in exprs do writer w e

        let rec reader (r : Reader) =
            match r.BR.ReadByte() with
            | 0uy -> let v = read r varFormatter :?> Var in Expr.Var v
            | 1uy -> 
                let v = read r varFormatter :?> Var
                let e = reader r
                Expr.Lambda(v, e)
            | 2uy ->
                let o = r.ReadObj combType
                let n = r.BR.ReadInt32()
                let arr = Array.zeroCreate<Expr> n
                for i = 0 to n - 1 do arr.[i] <- reader r
                RebuildShapeCombination(o, Array.toList arr)
            | _ -> raise <| new SerializationException("stream error")

        mkFormatter FormatterInfo.Custom false true reader writer


    type ExprFormatter () =
        interface IGenericFormatterFactory1 with
            member __.Create<'T> (resolver : Type -> Lazy<Formatter>) =
                mkFormatter FormatterInfo.Custom false true 
                                (fun r -> Expr.Cast<'T>(exprFormatter.Read r :?> Expr))
                                (fun w e -> exprFormatter.Write w (e :> obj))


    let genericFormatters =
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

    let fsFormatters = [ varFormatter ; exprFormatter ]