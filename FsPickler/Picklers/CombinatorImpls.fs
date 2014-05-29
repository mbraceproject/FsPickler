module internal Nessos.FsPickler.CombinatorImpls

    open System
    open System.Reflection
    open System.Collections.Generic

    open Microsoft.FSharp.Reflection

    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection
    open Nessos.FsPickler.PicklerUtils

    //
    //  Pickler combinator Implementations
    //

    // F# list pickler combinator

    type ListPickler () =
        static member Create (ep : Pickler<'T>) =
            let writer (w : WriteState) (list : 'T list) =

                if ep.TypeInfo = TypeInfo.Primitive && w.Formatter.IsPrimitiveArraySerializationSupported then
                    let arr = List.toArray list
                    w.Formatter.WriteInt32 "length" arr.Length
                    w.Formatter.WritePrimitiveArray "data" arr
                else
                    let rec writeL (ts : 'T list) =
                        match ts with
                        | t :: tl -> ep.Write w "elem" t ; writeL tl
                        | [] -> ()

                    w.Formatter.BeginWriteBoundedSequence list.Length
                    writeL list
                    w.Formatter.EndWriteBoundedSequence ()

            let reader (r : ReadState) =
                if ep.TypeInfo = TypeInfo.Primitive && r.Formatter.IsPrimitiveArraySerializationSupported then
                    let length = r.Formatter.ReadInt32 "length"
                    let array = Array.zeroCreate<'T> length
                    r.Formatter.ReadPrimitiveArray "data" array
                    Array.toList array
                else
                    let length = r.Formatter.BeginReadBoundedSequence ()
                    let array = Array.zeroCreate<'T> length
                    for i = 0 to length - 1 do
                        array.[i] <- ep.Read r "elem"

                    r.Formatter.EndReadBoundedSequence ()
                                    
                    Array.toList array

            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = true)
            

        interface IGenericPicklerFactory1 with
            member __.Create<'T1> (resolver : IPicklerResolver) =
                let p1 = resolver.Resolve<'T1> ()
                ListPickler.Create p1 :> Pickler


    type OptionPickler () =

        static member Create (ep : Pickler<'T>) =
            // Composite pickler filters None values by default
            let writer (w : WriteState) (x : 'T option) = ep.Write w "value" x.Value

            let reader (r : ReadState) =
                let value = ep.Read r "value"
                Some value

            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

        interface IGenericPicklerFactory1 with
            member __.Create<'T> (resolver : IPicklerResolver) =
                let ep = resolver.Resolve<'T> ()
                OptionPickler.Create ep :> Pickler


    type Choice2Pickler () =
        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>) =
            let writer (w : WriteState) (c : Choice<'T1, 'T2>) =
                match c with
                | Choice1Of2 t1 -> 
                    w.Formatter.WriteByte "case" 0uy
                    p1.Write w "value" t1
                | Choice2Of2 t2 -> 
                    w.Formatter.WriteByte "case" 1uy
                    p2.Write w "value" t2

            let reader (r : ReadState) =
                match r.Formatter.ReadByte "case" with
                | 0uy -> p1.Read r "value" |> Choice1Of2
                | _ -> p2.Read r "value" |> Choice2Of2

            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

        interface IGenericPicklerFactory2 with
            member __.Create<'T1, 'T2> (resolver : IPicklerResolver) =
                let p1, p2 = resolver.Resolve<'T1> (), resolver.Resolve<'T2> ()
                Choice2Pickler.Create(p1, p2) :> Pickler


    type Choice3Pickler () =
        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>) =
            let writer (w : WriteState) (c : Choice<'T1, 'T2, 'T3>) =
                match c with
                | Choice1Of3 t1 -> 
                    w.Formatter.WriteByte "case" 0uy
                    p1.Write w "value" t1
                | Choice2Of3 t2 -> 
                    w.Formatter.WriteByte "case" 1uy
                    p2.Write w "value" t2
                | Choice3Of3 t3 -> 
                    w.Formatter.WriteByte "case" 2uy
                    p3.Write w "value" t3

            let reader (r : ReadState) =
                match r.Formatter.ReadByte "case" with
                | 0uy -> p1.Read r "value" |> Choice1Of3
                | 1uy -> p2.Read r "value" |> Choice2Of3
                | _   -> p3.Read r "value" |> Choice3Of3

            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

        interface IGenericPicklerFactory3 with
            member __.Create<'T1, 'T2, 'T3> (resolver : IPicklerResolver) =
                let p1, p2, p3 = resolver.Resolve<'T1> (), resolver.Resolve<'T2> (), resolver.Resolve<'T3> ()
                Choice3Pickler.Create(p1, p2, p3) :> Pickler


    type Choice4Pickler () =
        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, p4 : Pickler<'T4>) =
            let writer (w : WriteState) (c : Choice<'T1, 'T2, 'T3, 'T4>) =
                match c with
                | Choice1Of4 t1 -> 
                    w.Formatter.WriteByte "case" 0uy
                    p1.Write w "value" t1
                | Choice2Of4 t2 -> 
                    w.Formatter.WriteByte "case" 1uy
                    p2.Write w "value" t2
                | Choice3Of4 t3 -> 
                    w.Formatter.WriteByte "case" 2uy
                    p3.Write w "value" t3
                | Choice4Of4 t4 -> 
                    w.Formatter.WriteByte "case" 3uy
                    p4.Write w "value" t4

            let reader (r : ReadState) =
                match r.Formatter.ReadByte "case" with
                | 0uy -> p1.Read r "value" |> Choice1Of4
                | 1uy -> p2.Read r "value" |> Choice2Of4
                | 2uy -> p3.Read r "value" |> Choice3Of4
                | _   -> p4.Read r "value" |> Choice4Of4

            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

        interface IGenericPicklerFactory4 with
            member __.Create<'T1, 'T2, 'T3, 'T4> (resolver : IPicklerResolver) =
                let p1, p2 = resolver.Resolve<'T1> (), resolver.Resolve<'T2> ()
                let p3, p4 = resolver.Resolve<'T3> (), resolver.Resolve<'T4> ()
                Choice4Pickler.Create(p1, p2, p3, p4) :> Pickler


    type FSharpRefPickler () =
        static member Create (ep : Pickler<'T>) =
            let writer (w : WriteState) (r : 'T ref) =
                ep.Write w "contents" r.Value

            let reader (r : ReadState) =
                { contents = ep.Read r "contents" }

            // do not cache for performance
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = false)
            
        interface IGenericPicklerFactory1 with
            member __.Create<'T> (resolver : IPicklerResolver) =
                let ep = resolver.Resolve<'T> ()
                FSharpRefPickler.Create ep :> Pickler

    type FSharpSetPickler () =
        static member Create<'T when 'T : comparison>(ep : Pickler<'T>) =
            let writer (w : WriteState) (s : Set<'T>) = 
                writeBoundedSequence w ep s.Count s

            let reader (r : ReadState) =
                readBoundedSequence r ep |> Set.ofArray

            CompositePickler.Create<_>(reader, writer, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = false)
            
        interface IPicklerFactory
        member __.Create<'T when 'T : comparison> (resolver : IPicklerResolver) =
            let ep = resolver.Resolve<'T>()
            FSharpSetPickler.Create ep :> Pickler

    type FSharpMapPickler () =
        static member Create<'K, 'V when 'K : comparison> (kp : Pickler<'K>, vp : Pickler<'V>) =
            
            let writer (w : WriteState) (m : Map<'K,'V>) =
                writeBoundedPairSequence w kp vp m.Count (Map.toSeq m)

            let reader (r : ReadState) =
                readBoundedPairSequence r kp vp |> Map.ofArray

            CompositePickler.Create<_>(reader, writer, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = false)
            
        interface IPicklerFactory
        member __.Create<'K, 'V when 'K : comparison> (resolver : IPicklerResolver) =
            let kp, vp = resolver.Resolve<'K> (), resolver.Resolve<'V> ()
            FSharpMapPickler.Create(kp, vp) :> Pickler


    type DictionaryPickler () =
        static member Create<'K, 'V when 'K : comparison> (kp : Pickler<'K>, vp : Pickler<'V>) =

            let writer (w : WriteState) (d : Dictionary<'K,'V>) =
                let kvs = Seq.map (fun (KeyValue (k,v)) -> k,v) d
                writeBoundedPairSequence w kp vp d.Count kvs

            let reader (r : ReadState) =
                let kvs = readBoundedPairSequence r kp vp
                let d = new Dictionary<'K,'V>()
                for i = 0 to kvs.Length - 1 do
                    let k,v = kvs.[i]
                    d.Add(k,v)
                d

            CompositePickler.Create<_>(reader, writer, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = false)
            
        interface IPicklerFactory
        member __.Create<'K, 'V when 'K : comparison> (resolver : IPicklerResolver) =
            let kp, vp = resolver.Resolve<'K>(), resolver.Resolve<'V>()
            DictionaryPickler.Create (kp, vp) :> Pickler


    type AltPickler =
        
        static member Create(tagReader : 'T -> int, picklers : Pickler<'T> list) =
            
            let picklers = List.toArray picklers
            let cacheByRef = picklers |> Array.exists (fun p -> p.IsCacheByRef)
            let useWithSubtypes = picklers |> Array.forall (fun p -> p.UseWithSubtypes)

            let writer (w : WriteState) (t : 'T) =
                let tag = tagReader t
                do w.Formatter.WriteInt32 "tag" tag
                picklers.[tag].Write w "branch" t

            let reader (r : ReadState) =
                let tag = r.Formatter.ReadInt32 "tag"
                picklers.[tag].Read r "branch"

            CompositePickler.Create<_>(reader, writer, PicklerInfo.Combinator, 
                                cacheByRef = cacheByRef, useWithSubtypes = useWithSubtypes)


    type WrapPickler =
        static member Create(origin : Pickler<'T>, recover : 'T -> 'S, convert : 'S -> 'T) =
#if OPTIMIZE_FSHARP
            // disable subtype resolution if F# union or tuple
            let useWithSubtypes = FSharpType.IsUnion(typeof<'S>, allMembers) || FSharpType.IsTuple typeof<'S>
#else
            let useWithSubtypes = FSharpType.IsUnion(typeof<'S>, allMembers)
#endif
            let writer (w : WriteState) (s : 'S) =
                let t = convert s
                origin.Write w "wrapped" t

            let reader (r : ReadState) =
                let t = origin.Read r "wrapped"
                recover t

            CompositePickler.Create<_>(reader, writer, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = useWithSubtypes)

    type SeqPickler =
        static member Create(ep : Pickler<'T>) =
            CompositePickler.Create<_>(readSequence ep, writeSequence ep, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = true)

    type KeyValueSeqPickler =
        static member Create(kp : Pickler<'K>, vp : Pickler<'V>) =
            CompositePickler.Create<_>(readPairSequenceNoLength kp vp, writePairSequenceNoLength kp vp, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = true)

                

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