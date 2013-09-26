module internal FsPickler.FSharpCombinators

    open System
    open System.Collections.Generic

    open FsPickler
    open FsPickler.Utils
    open FsPickler.FormatterUtils
    open FsPickler.BaseFormatters

    //
    //  F# pickler combinators
    //

    type ListFormatter () =
        static member Create (ef : Formatter<'T>) =
            let writer (w : Writer) (l : 'T list) =

                if ef.TypeInfo = TypeInfo.Primitive then
                    let arr = List.toArray l
                    w.BW.Write arr.Length
                    do Stream.WriteArray(w.BW.BaseStream, arr)
                else
                    let isValue = ef.TypeInfo <= TypeInfo.Value
                    let rec writeL (xs : 'T list) =
                        match xs with
                        | hd :: tl -> write isValue w ef hd ; writeL tl
                        | [] -> ()

                    w.BW.Write l.Length
                    writeL l

            let reader (r : Reader) =
                let length = r.BR.ReadInt32 ()

                if ef.TypeInfo = TypeInfo.Primitive then
                    let array = Array.zeroCreate<'T> length
                    Stream.CopyToArray(r.BR.BaseStream, array)
                    Array.toList array
                else
                    let isValue = ef.TypeInfo <= TypeInfo.Value
                    let array = Array.zeroCreate<'T> length
                    for i = 0 to length - 1 do
                        array.[i] <- read isValue r ef
                                    
                    Array.toList array

            new Formatter<_>(reader, writer, FormatterInfo.FSharpValue, cacheObj = true, useWithSubtypes = false)
            

        interface IGenericFormatterFactory1 with
            member __.Create<'T> (resolver : IFormatterResolver) =
                let ef = resolver.Resolve<'T> ()
                ListFormatter.Create ef :> Formatter


    type PairFormatter () =
        static member Create(tf : Formatter<'T>, sf : Formatter<'S>) =

            let writer (w : Writer) ((t,s) : 'T * 'S) =
                write (isValue tf) w tf t ; write (isValue sf) w sf s

            let reader (r : Reader) = (read (isValue tf) r tf, read (isValue sf) r sf)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            new Formatter<_>(reader, writer, FormatterInfo.FSharpValue, cacheObj = false, useWithSubtypes = true)
#else
            new Formatter<_>(reader, writer, FormatterInfo.FSharpValue, cacheObj = true, useWithSubtypes = false)
#endif
            
        interface IGenericFormatterFactory2 with
            member __.Create<'T,'S> (resolver : IFormatterResolver) =
                let tf, sf = resolver.Resolve<'T> (), resolver.Resolve<'S> ()
                
                PairFormatter.Create(tf,sf) :> Formatter


    type TripleFormatter () =

        static member Create(f1 : Formatter<'T1>, f2 : Formatter<'T2>, f3 : Formatter<'T3>) =
            let writer (w : Writer) ((t1,t2,t3) : 'T1 * 'T2 * 'T3) =
                write (isValue f1) w f1 t1 ; write (isValue f2) w f2 t2 ; write (isValue f3) w f3 t3

            let reader (r : Reader) =
                (read (isValue f1) r f1, read (isValue f2) r f2, read (isValue f3) r f3)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            new Formatter<_>(reader, writer, FormatterInfo.FSharpValue, cacheObj = false, useWithSubtypes = true)
#else
            new Formatter<_>(reader, writer, FormatterInfo.FSharpValue, cacheObj = true, useWithSubtypes = false)
#endif    

        interface IGenericFormatterFactory3 with
            member __.Create<'T1, 'T2, 'T3> (resolver : IFormatterResolver) =
                let f1, f2, f3 = resolver.Resolve<'T1>(), resolver.Resolve<'T2>(), resolver.Resolve<'T3>()
                TripleFormatter.Create(f1, f2, f3) :> Formatter

    type QuadFormatter () =

        static member Create(f1 : Formatter<'T1>, f2 : Formatter<'T2>, f3 : Formatter<'T3>, f4 : Formatter<'T4>) =
            let writer (w : Writer) ((t1,t2,t3,t4) : 'T1 * 'T2 * 'T3 * 'T4) =
                write (isValue f1) w f1 t1 ; write (isValue f2) w f2 t2 ;
                write (isValue f3) w f3 t3 ; write (isValue f4) w f4 t4 ; 

            let reader (r : Reader) =
                (read (isValue f1) r f1, read (isValue f2) r f2, read (isValue f3) r f3, read (isValue f4) r f4)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            new Formatter<_>(reader, writer, FormatterInfo.FSharpValue, cacheObj = false, useWithSubtypes = true)
#else
            new Formatter<_>(reader, writer, FormatterInfo.FSharpValue, cacheObj = true, useWithSubtypes = false)
#endif    

        interface IGenericFormatterFactory4 with
            member __.Create<'T1, 'T2, 'T3, 'T4> (resolver : IFormatterResolver) =
                let f1, f2 = resolver.Resolve<'T1>(), resolver.Resolve<'T2>()
                let f3, f4 = resolver.Resolve<'T3>(), resolver.Resolve<'T4>()
                QuadFormatter.Create(f1, f2, f3, f4) :> Formatter


    type OptionFormatter () =

        static member Create (ef : Formatter<'T>) =
            let writer (w : Writer) (x : 'T option) =
                match x with
                | None -> w.BW.Write true
                | Some v -> w.BW.Write false ; write (isValue ef) w ef v

            let reader (r : Reader) =
                if r.BR.ReadBoolean() then None
                else
                    Some(read (isValue ef) r ef)

            new Formatter<_>(reader, writer, FormatterInfo.FSharpValue, cacheObj = false, useWithSubtypes = true)

        interface IGenericFormatterFactory1 with
            member __.Create<'T> (resolver : IFormatterResolver) =
                let ef = resolver.Resolve<'T> ()
                OptionFormatter.Create ef :> Formatter


    type Choice2Formatter () =
        static member Create(f1 : Formatter<'T1>, f2 : Formatter<'T2>) =
            let writer (w : Writer) (c : Choice<'T1, 'T2>) =
                match c with
                | Choice1Of2 t1 -> 
                    w.BW.Write 0uy
                    write (isValue f1) w f1 t1
                | Choice2Of2 t2 -> 
                    w.BW.Write 1uy
                    write (isValue f2) w f2 t2

            let reader (r : Reader) =
                match r.BR.ReadByte() with
                | 0uy -> read (isValue f1) r f1 |> Choice1Of2
                | _ -> read (isValue f2) r f2 |> Choice2Of2

            new Formatter<_>(reader, writer, FormatterInfo.FSharpValue, cacheObj = false, useWithSubtypes = true)

        interface IGenericFormatterFactory2 with
            member __.Create<'T1, 'T2> (resolver : IFormatterResolver) =
                let f1, f2 = resolver.Resolve<'T1> (), resolver.Resolve<'T2> ()
                Choice2Formatter.Create(f1, f2) :> Formatter


    type Choice3Formatter () =
        static member Create(f1 : Formatter<'T1>, f2 : Formatter<'T2>, f3 : Formatter<'T3>) =
            let writer (w : Writer) (c : Choice<'T1, 'T2, 'T3>) =
                match c with
                | Choice1Of3 t1 -> 
                    w.BW.Write 0uy
                    write (isValue f1) w f1 t1
                | Choice2Of3 t2 -> 
                    w.BW.Write 1uy
                    write (isValue f2) w f2 t2
                | Choice3Of3 t3 -> 
                    w.BW.Write 2uy
                    write (isValue f3) w f3 t3

            let reader (r : Reader) =
                match r.BR.ReadByte() with
                | 0uy -> read (isValue f1) r f1 |> Choice1Of3
                | 1uy -> read (isValue f2) r f2 |> Choice2Of3
                | _   -> read (isValue f3) r f3 |> Choice3Of3

            new Formatter<_>(reader, writer, FormatterInfo.FSharpValue, cacheObj = false, useWithSubtypes = true)

        interface IGenericFormatterFactory3 with
            member __.Create<'T1, 'T2, 'T3> (resolver : IFormatterResolver) =
                let f1, f2, f3 = resolver.Resolve<'T1> (), resolver.Resolve<'T2> (), resolver.Resolve<'T3> ()
                Choice3Formatter.Create(f1, f2, f3) :> Formatter


    type Choice4Formatter () =
        static member Create(f1 : Formatter<'T1>, f2 : Formatter<'T2>, f3 : Formatter<'T3>, f4 : Formatter<'T4>) =
            let writer (w : Writer) (c : Choice<'T1, 'T2, 'T3, 'T4>) =
                match c with
                | Choice1Of4 t1 -> 
                    w.BW.Write 0uy
                    write (isValue f1) w f1 t1
                | Choice2Of4 t2 -> 
                    w.BW.Write 1uy
                    write (isValue f2) w f2 t2
                | Choice3Of4 t3 -> 
                    w.BW.Write 2uy
                    write (isValue f3) w f3 t3
                | Choice4Of4 t4 -> 
                    w.BW.Write 3uy
                    write (isValue f4) w f4 t4

            let reader (r : Reader) =
                match r.BR.ReadByte() with
                | 0uy -> read (isValue f1) r f1 |> Choice1Of4
                | 1uy -> read (isValue f2) r f2 |> Choice2Of4
                | 2uy -> read (isValue f3) r f3 |> Choice3Of4
                | _   -> read (isValue f4) r f4 |> Choice4Of4

            new Formatter<_>(reader, writer, FormatterInfo.FSharpValue, cacheObj = false, useWithSubtypes = true)

        interface IGenericFormatterFactory4 with
            member __.Create<'T1, 'T2, 'T3, 'T4> (resolver : IFormatterResolver) =
                let f1, f2 = resolver.Resolve<'T1> (), resolver.Resolve<'T2> ()
                let f3, f4 = resolver.Resolve<'T3> (), resolver.Resolve<'T4> ()
                Choice4Formatter.Create(f1, f2, f3, f4) :> Formatter


    type FSharpRefFormatter () =
        static member Create (ef : Formatter<'T>) =
            let writer (w : Writer) (r : 'T ref) =
                write (isValue ef) w ef r.Value

            let reader (r : Reader) =
                read (isValue ef) r ef |> ref

            // do not cache for performance
            new Formatter<_>(reader, writer, FormatterInfo.FSharpValue, cacheObj = false, useWithSubtypes = false)
            
        interface IGenericFormatterFactory1 with
            member __.Create<'T> (resolver : IFormatterResolver) =
                let ef = resolver.Resolve<'T> ()
                FSharpRefFormatter.Create ef :> Formatter


    type FSharpSetFormatter () =
        static member Create<'T when 'T : comparison>(ef : Formatter<'T>) =
            let writer (w : Writer) (s : Set<'T>) = 
                writeSeq w ef s.Count s

            let reader (r : Reader) =
                readSeq r ef |> Set.ofArray

            new Formatter<_>(reader, writer, FormatterInfo.FSharpValue, cacheObj = true, useWithSubtypes = false)
            
        interface IGenericFormatterFactory
        member __.Create<'T when 'T : comparison> (resolver : IFormatterResolver) =
            let ef = resolver.Resolve<'T>()
            FSharpSetFormatter.Create ef :> Formatter


    type FSharpMapFormatter () =
        static member Create<'K, 'V when 'K : comparison> (kf : Formatter<'K>, vf : Formatter<'V>) =
            
            let writer (w : Writer) (m : Map<'K,'V>) =
                writeKVPairs w kf vf m.Count (Map.toSeq m)

            let reader (r : Reader) =
                readKVPairs r kf vf |> Map.ofArray

            new Formatter<_>(reader, writer, FormatterInfo.FSharpValue, cacheObj = true, useWithSubtypes = false)
            
        interface IGenericFormatterFactory
        member __.Create<'K, 'V when 'K : comparison> (resolver : IFormatterResolver) =
            let kf, vf = resolver.Resolve<'K> (), resolver.Resolve<'V> ()
            FSharpMapFormatter.Create(kf, vf) :> Formatter


    type DictionaryFormatter () =
        static member Create<'K, 'V when 'K : comparison> (kf : Formatter<'K>, vf : Formatter<'V>) =

            let writer (w : Writer) (d : Dictionary<'K,'V>) =
                let kvs = Seq.map (fun (KeyValue (k,v)) -> k,v) d
                writeKVPairs w kf vf d.Count kvs

            let reader (r : Reader) =
                let kvs = readKVPairs r kf vf
                let d = new Dictionary<'K,'V>()
                for i = 0 to kvs.Length - 1 do
                    let k,v = kvs.[i]
                    d.Add(k,v)
                d

            Formatter<_>(reader, writer, FormatterInfo.Custom, cacheObj = true, useWithSubtypes = false)
            
        interface IGenericFormatterFactory
        member __.Create<'K, 'V when 'K : comparison> (resolver : IFormatterResolver) =
            let kf, vf = resolver.Resolve<'K>(), resolver.Resolve<'V>()
            DictionaryFormatter.Create (kf, vf) :> Formatter


    type AltFormatter =
        
        static member Create(tagReader : 'T -> int, formatters : Formatter<'T> list) =
            let writer (w : Writer) (t : 'T) =
                let tag = tagReader t
                do w.BW.Write tag
                formatters.[tag].Write w t

            let reader (r : Reader) = 
                let tag = r.BR.ReadInt32()
                formatters.[tag].Read r

            new Formatter<_>(reader, writer, FormatterInfo.Custom, cacheObj = true, useWithSubtypes = false)


    type WrapFormatter =
        static member Create(origin : Formatter<'T>, convert : 'S -> 'T, recover : 'T -> 'S) =
            new Formatter<_>(origin.Read >> recover, (fun w t -> origin.Write w (convert t)), 
                                    FormatterInfo.Custom, cacheObj = true, useWithSubtypes = false)


    type SeqFormatter =
        static member Create(ef : Formatter<'T>) =
            new Formatter<_>(readSeq' ef, writeSeq' ef, FormatterInfo.Custom, cacheObj = true, useWithSubtypes = false)

    type KeyValueSeqFormatter =
        static member Create(kf : Formatter<'K>, vf : Formatter<'V>) =
            new Formatter<_>(readKVPairs' kf vf, writeKVPairs' kf vf, FormatterInfo.Custom, cacheObj = true, useWithSubtypes = false)

                

    let mkGenericFormatters () =
        [
            new ListFormatter() :> IGenericFormatterFactory
            new PairFormatter() :> _
            new TripleFormatter() :> _
            new QuadFormatter() :> _
            new OptionFormatter() :> _
            new Choice2Formatter() :> _
            new Choice3Formatter() :> _
            new Choice4Formatter() :> _
            new FSharpRefFormatter() :> _
            new FSharpSetFormatter() :> _
            new FSharpMapFormatter() :> _
            new DictionaryFormatter() :> _
        ]