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

                if ef.TypeKind = TypeKind.Primitive then //&& keepEndianness then
                    let arr = List.toArray l
                    w.Formatter.WriteInt32 "length" arr.Length
                    w.Formatter.WritePrimitiveArray "data" arr
//                    do Stream.ReadFromArray(w.Formatter.BaseStream, arr)
                else
                    let isPrimitive = isPrimitive ef
                    let rec writeL (xs : 'T list) =
                        match xs with
                        | hd :: tl -> write isPrimitive w ef "item" hd ; writeL tl
                        | [] -> ()

                    w.Formatter.WriteInt32 "length" l.Length
                    writeL l

            let reader (r : Reader) =
                let length = r.Formatter.ReadInt32 "length"

                if ef.TypeKind = TypeKind.Primitive then //&& keepEndianness then
                    let array = Array.zeroCreate<'T> length
                    r.Formatter.ReadToPrimitiveArray "data" array
                    Array.toList array
                else
                    let isPrimitive = isPrimitive ef
                    let array = Array.zeroCreate<'T> length
                    for i = 0 to length - 1 do
                        array.[i] <- read isPrimitive r ef "item"
                                    
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
                | None -> w.Formatter.WriteBoolean "isNone" true
                | Some v -> w.Formatter.WriteBoolean "isNone" false ; write (isPrimitive ef) w ef "value" v

            let reader (r : Reader) =
                if r.Formatter.ReadBoolean "isNone" then None
                else
                    Some(read (isPrimitive ef) r ef "value")

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
                    w.Formatter.WriteByte "tag" 0uy
                    write (isPrimitive f1) w f1 "value" t1
                | Choice2Of2 t2 -> 
                    w.Formatter.WriteByte "tag" 1uy
                    write (isPrimitive f2) w f2 "value" t2

            let reader (r : Reader) =
                match r.Formatter.ReadByte "tag" with
                | 0uy -> read (isPrimitive f1) r f1 "value" |> Choice1Of2
                | _ -> read (isPrimitive f2) r f2 "value" |> Choice2Of2

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
                    w.Formatter.WriteByte "tag" 0uy
                    write (isPrimitive f1) w f1 "value" t1
                | Choice2Of3 t2 -> 
                    w.Formatter.WriteByte "tag" 1uy
                    write (isPrimitive f2) w f2 "value" t2
                | Choice3Of3 t3 -> 
                    w.Formatter.WriteByte "tag" 2uy
                    write (isPrimitive f3) w f3 "value" t3

            let reader (r : Reader) =
                match r.Formatter.ReadByte "tag" with
                | 0uy -> read (isPrimitive f1) r f1 "value" |> Choice1Of3
                | 1uy -> read (isPrimitive f2) r f2 "value" |> Choice2Of3
                | _   -> read (isPrimitive f3) r f3 "value" |> Choice3Of3

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
                    w.Formatter.WriteByte "tag" 0uy
                    write (isPrimitive f1) w f1 "value" t1
                | Choice2Of4 t2 -> 
                    w.Formatter.WriteByte "tag" 1uy
                    write (isPrimitive f2) w f2 "value" t2
                | Choice3Of4 t3 -> 
                    w.Formatter.WriteByte "tag" 2uy
                    write (isPrimitive f3) w f3 "value" t3
                | Choice4Of4 t4 -> 
                    w.Formatter.WriteByte "tag" 3uy
                    write (isPrimitive f4) w f4 "value" t4

            let reader (r : Reader) =
                match r.Formatter.ReadByte "tag" with
                | 0uy -> read (isPrimitive f1) r f1 "value" |> Choice1Of4
                | 1uy -> read (isPrimitive f2) r f2 "value" |> Choice2Of4
                | 2uy -> read (isPrimitive f3) r f3 "value" |> Choice3Of4
                | _   -> read (isPrimitive f4) r f4 "value" |> Choice4Of4

            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

        interface IGenericPicklerFactory4 with
            member __.Create<'T1, 'T2, 'T3, 'T4> (resolver : IPicklerResolver) =
                let f1, f2 = resolver.Resolve<'T1> (), resolver.Resolve<'T2> ()
                let f3, f4 = resolver.Resolve<'T3> (), resolver.Resolve<'T4> ()
                Choice4Pickler.Create(f1, f2, f3, f4) :> Pickler


    type FSharpRefPickler () =
        static member Create (ef : Pickler<'T>) =
            let writer (w : Writer) (r : 'T ref) =
                write (isPrimitive ef) w ef "contents" r.Value

            let reader (r : Reader) =
                { contents = read (isPrimitive ef) r ef "contents" }

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
                do w.Formatter.WriteInt32 "tag" tag
                picklers.[tag].Write w t

            let reader (r : Reader) =
                let tag = r.Formatter.ReadInt32 "tag"
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