module internal Nessos.FsPickler.CombinatorImpls

    open System
    open System.Reflection
    open System.Collections.Generic

    open Microsoft.FSharp.Reflection

    open Nessos.FsPickler
    open Nessos.FsPickler.Utils
    open Nessos.FsPickler.PicklerUtils
    open Nessos.FsPickler.BasePicklers

    //
    //  Pickler combinator Implementations
    //

    // F# list pickler combinator

    type ListPickler () =
        static member Create (ef : Pickler<'T>) =
            let writer (w : Writer) (l : 'T list) =

                if ef.TypeKind = TypeKind.Primitive && keepEndianness then
                    let arr = List.toArray l
                    w.BinaryWriter.Write arr.Length
                    do Stream.ReadFromArray(w.BinaryWriter.BaseStream, arr)
                else
                    let isValue = ef.TypeKind <= TypeKind.Value
                    let rec writeL (xs : 'T list) =
                        match xs with
                        | hd :: tl -> write isValue w ef hd ; writeL tl
                        | [] -> ()

                    w.BinaryWriter.Write l.Length
                    writeL l

            let reader (r : Reader) =
                let length = r.BinaryReader.ReadInt32 ()

                if ef.TypeKind = TypeKind.Primitive && keepEndianness then
                    let array = Array.zeroCreate<'T> length
                    Stream.WriteToArray(r.BinaryReader.BaseStream, array)
                    Array.toList array
                else
                    let isValue = ef.TypeKind <= TypeKind.Value
                    let array = Array.zeroCreate<'T> length
                    for i = 0 to length - 1 do
                        array.[i] <- read isValue r ef
                                    
                    Array.toList array

            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = true)
            

        interface IGenericPicklerFactory1 with
            member __.Create<'T1> (resolver : IPicklerResolver) =
                let p1 = resolver.Resolve<'T1> ()
                ListPickler.Create p1 :> Pickler


    type OptionPickler () =

        static member Create (ef : Pickler<'T>) =
            let writer (w : Writer) (x : 'T option) =
                match x with
                | None -> w.BinaryWriter.Write true
                | Some v -> w.BinaryWriter.Write false ; write (isValue ef) w ef v

            let reader (r : Reader) =
                if r.BinaryReader.ReadBoolean() then None
                else
                    Some(read (isValue ef) r ef)

            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

        interface IGenericPicklerFactory1 with
            member __.Create<'T> (resolver : IPicklerResolver) =
                let ef = resolver.Resolve<'T> ()
                OptionPickler.Create ef :> Pickler


    type Choice2Pickler () =
        static member Create(f1 : Pickler<'T1>, f2 : Pickler<'T2>) =
            let writer (w : Writer) (c : Choice<'T1, 'T2>) =
                match c with
                | Choice1Of2 t1 -> 
                    w.BinaryWriter.Write 0uy
                    write (isValue f1) w f1 t1
                | Choice2Of2 t2 -> 
                    w.BinaryWriter.Write 1uy
                    write (isValue f2) w f2 t2

            let reader (r : Reader) =
                match r.BinaryReader.ReadByte() with
                | 0uy -> read (isValue f1) r f1 |> Choice1Of2
                | _ -> read (isValue f2) r f2 |> Choice2Of2

            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

        interface IGenericPicklerFactory2 with
            member __.Create<'T1, 'T2> (resolver : IPicklerResolver) =
                let f1, f2 = resolver.Resolve<'T1> (), resolver.Resolve<'T2> ()
                Choice2Pickler.Create(f1, f2) :> Pickler


    type Choice3Pickler () =
        static member Create(f1 : Pickler<'T1>, f2 : Pickler<'T2>, f3 : Pickler<'T3>) =
            let writer (w : Writer) (c : Choice<'T1, 'T2, 'T3>) =
                match c with
                | Choice1Of3 t1 -> 
                    w.BinaryWriter.Write 0uy
                    write (isValue f1) w f1 t1
                | Choice2Of3 t2 -> 
                    w.BinaryWriter.Write 1uy
                    write (isValue f2) w f2 t2
                | Choice3Of3 t3 -> 
                    w.BinaryWriter.Write 2uy
                    write (isValue f3) w f3 t3

            let reader (r : Reader) =
                match r.BinaryReader.ReadByte() with
                | 0uy -> read (isValue f1) r f1 |> Choice1Of3
                | 1uy -> read (isValue f2) r f2 |> Choice2Of3
                | _   -> read (isValue f3) r f3 |> Choice3Of3

            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

        interface IGenericPicklerFactory3 with
            member __.Create<'T1, 'T2, 'T3> (resolver : IPicklerResolver) =
                let f1, f2, f3 = resolver.Resolve<'T1> (), resolver.Resolve<'T2> (), resolver.Resolve<'T3> ()
                Choice3Pickler.Create(f1, f2, f3) :> Pickler


    type Choice4Pickler () =
        static member Create(f1 : Pickler<'T1>, f2 : Pickler<'T2>, f3 : Pickler<'T3>, f4 : Pickler<'T4>) =
            let writer (w : Writer) (c : Choice<'T1, 'T2, 'T3, 'T4>) =
                match c with
                | Choice1Of4 t1 -> 
                    w.BinaryWriter.Write 0uy
                    write (isValue f1) w f1 t1
                | Choice2Of4 t2 -> 
                    w.BinaryWriter.Write 1uy
                    write (isValue f2) w f2 t2
                | Choice3Of4 t3 -> 
                    w.BinaryWriter.Write 2uy
                    write (isValue f3) w f3 t3
                | Choice4Of4 t4 -> 
                    w.BinaryWriter.Write 3uy
                    write (isValue f4) w f4 t4

            let reader (r : Reader) =
                match r.BinaryReader.ReadByte() with
                | 0uy -> read (isValue f1) r f1 |> Choice1Of4
                | 1uy -> read (isValue f2) r f2 |> Choice2Of4
                | 2uy -> read (isValue f3) r f3 |> Choice3Of4
                | _   -> read (isValue f4) r f4 |> Choice4Of4

            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

        interface IGenericPicklerFactory4 with
            member __.Create<'T1, 'T2, 'T3, 'T4> (resolver : IPicklerResolver) =
                let f1, f2 = resolver.Resolve<'T1> (), resolver.Resolve<'T2> ()
                let f3, f4 = resolver.Resolve<'T3> (), resolver.Resolve<'T4> ()
                Choice4Pickler.Create(f1, f2, f3, f4) :> Pickler


    type FSharpRefPickler () =
        static member Create (ef : Pickler<'T>) =
            let writer (w : Writer) (r : 'T ref) =
                write (isValue ef) w ef r.Value

            let reader (r : Reader) =
                read (isValue ef) r ef |> ref

            // do not cache for performance
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = false)
            
        interface IGenericPicklerFactory1 with
            member __.Create<'T> (resolver : IPicklerResolver) =
                let ef = resolver.Resolve<'T> ()
                FSharpRefPickler.Create ef :> Pickler

    type FSharpSetPickler () =
        static member Create<'T when 'T : comparison>(ef : Pickler<'T>) =
            let writer (w : Writer) (s : Set<'T>) = 
                writeSeq w ef s.Count s

            let reader (r : Reader) =
                readSeq r ef |> Set.ofArray

            new Pickler<_>(reader, writer, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = false)
            
        interface IPicklerFactory
        member __.Create<'T when 'T : comparison> (resolver : IPicklerResolver) =
            let ef = resolver.Resolve<'T>()
            FSharpSetPickler.Create ef :> Pickler

    type FSharpMapPickler () =
        static member Create<'K, 'V when 'K : comparison> (kf : Pickler<'K>, vf : Pickler<'V>) =
            
            let writer (w : Writer) (m : Map<'K,'V>) =
                writeKVPairs w kf vf m.Count (Map.toSeq m)

            let reader (r : Reader) =
                readKVPairs r kf vf |> Map.ofArray

            new Pickler<_>(reader, writer, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = false)
            
        interface IPicklerFactory
        member __.Create<'K, 'V when 'K : comparison> (resolver : IPicklerResolver) =
            let kf, vf = resolver.Resolve<'K> (), resolver.Resolve<'V> ()
            FSharpMapPickler.Create(kf, vf) :> Pickler


    type DictionaryPickler () =
        static member Create<'K, 'V when 'K : comparison> (kf : Pickler<'K>, vf : Pickler<'V>) =

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

            Pickler<_>(reader, writer, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = false)
            
        interface IPicklerFactory
        member __.Create<'K, 'V when 'K : comparison> (resolver : IPicklerResolver) =
            let kf, vf = resolver.Resolve<'K>(), resolver.Resolve<'V>()
            DictionaryPickler.Create (kf, vf) :> Pickler


    type AltPickler =
        
        static member Create(tagReader : 'T -> int, picklers : Pickler<'T> list) =
            
            let picklers = List.toArray picklers
            let cacheByRef = picklers |> Array.exists (fun p -> p.IsCacheByRef)
            let useWithSubtypes = picklers |> Array.forall (fun p -> p.UseWithSubtypes)

            let writer (w : Writer) (t : 'T) =
                let tag = tagReader t
                do w.BinaryWriter.Write tag
                picklers.[tag].Write w t

            let reader (r : Reader) =
                let tag = r.BinaryReader.ReadInt32()
                picklers.[tag].Read r

            new Pickler<_>(reader, writer, PicklerInfo.Combinator, 
                                cacheByRef = cacheByRef, useWithSubtypes = useWithSubtypes)


    type WrapPickler =
        static member Create(origin : Pickler<'T>, recover : 'T -> 'S, convert : 'S -> 'T) =
#if OPTIMIZE_FSHARP
            // disable subtype resolution if F# union or tuple
            let useWithSubtypes = FSharpType.IsUnion(typeof<'S>, allMembers) || FSharpType.IsTuple typeof<'S>
#else
            let useWithSubtypes = FSharpType.IsUnion(typeof<'S>, allMembers)
#endif
            new Pickler<_>(origin.Read >> recover, (fun w t -> origin.Write w (convert t)), 
                                    PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = useWithSubtypes)

    type SeqPickler =
        static member Create(ef : Pickler<'T>) =
            new Pickler<_>(readSeq' ef, writeSeq' ef, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = true)

    type KeyValueSeqPickler =
        static member Create(kf : Pickler<'K>, vf : Pickler<'V>) =
            new Pickler<_>(readKVPairs' kf vf, writeKVPairs' kf vf, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = true)

                

    let getDefaultPicklerFactories () =
        [
            new ListPickler() :> IPicklerFactory
            new OptionPickler() :> _
            new Choice2Pickler() :> _
            new Choice3Pickler() :> _
            new Choice4Pickler() :> _
            new FSharpRefPickler() :> _
            new DictionaryPickler() :> _
            new FSharpSetPickler() :> _
            new FSharpMapPickler() :> _
        ]