module internal Nessos.FsPickler.CombinatorImpls

    open System
    open System.IO
    open System.Reflection
    open System.Collections.Generic

    open Microsoft.FSharp.Reflection

    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection
    open Nessos.FsPickler.PicklerUtils
    open Nessos.FsPickler.TuplePicklers

    //
    //  Pickler combinator implementations for common generic types
    //

    // F# list pickler combinator

    type ListPickler =
        static member Create (ep : Pickler<'T>) =
            let writer (w : WriteState) (tag : string) (list : 'T list) =

                if ep.TypeKind = TypeKind.Primitive && w.Formatter.IsPrimitiveArraySerializationSupported then
                    let arr = List.toArray list
                    w.Formatter.WriteInt32 "length" arr.Length
                    w.Formatter.WritePrimitiveArray "data" arr
                else
                    let rec writeL (ts : 'T list) =
                        match ts with
                        | t :: tl -> ep.Write w "elem" t ; writeL tl
                        | [] -> ()

                    w.Formatter.BeginWriteBoundedSequence "list" list.Length
                    writeL list
                    w.Formatter.EndWriteBoundedSequence ()

            let reader (r : ReadState) (tag : string) =
                if ep.TypeKind = TypeKind.Primitive && r.Formatter.IsPrimitiveArraySerializationSupported then
                    let length = r.Formatter.ReadInt32 "length"
                    let array = Array.zeroCreate<'T> length
                    r.Formatter.ReadPrimitiveArray "data" array
                    Array.toList array
                else
                    let length = r.Formatter.BeginReadBoundedSequence "list"
                    let array = Array.zeroCreate<'T> length
                    for i = 0 to length - 1 do
                        array.[i] <- ep.Read r "elem"

                    r.Formatter.EndReadBoundedSequence ()
                                    
                    Array.toList array

            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = true)

        static member Create<'T>(resolver : IPicklerResolver) =
            let ep = resolver.Resolve<'T> ()
            ListPickler.Create<'T> ep


    type OptionPickler =

        static member Create (ep : Pickler<'T>) =
            // Composite pickler filters None values by default
            let writer (w : WriteState) (tag : string) (x : 'T option) = ep.Write w "Some" x.Value

            let reader (r : ReadState) (tag : string) =
                let value = ep.Read r "Some"
                Some value

            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

        static member Create<'T> (resolver : IPicklerResolver) =
            let ep = resolver.Resolve<'T> ()
            OptionPickler.Create ep


    type ChoicePickler =
        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>) =
            let writer (w : WriteState) (tag : string) (c : Choice<'T1, 'T2>) =
                match c with
                | Choice1Of2 t1 -> 
                    w.Formatter.WriteByte "case" 0uy
                    p1.Write w "Item" t1
                | Choice2Of2 t2 -> 
                    w.Formatter.WriteByte "case" 1uy
                    p2.Write w "Item" t2

            let reader (r : ReadState) (tag : string) =
                match r.Formatter.ReadByte "case" with
                | 0uy -> p1.Read r "Item" |> Choice1Of2
                | 1uy -> p2.Read r "Item" |> Choice2Of2
                | _ -> raise <| new InvalidDataException()

            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)


        static member Create<'T1, 'T2> (resolver : IPicklerResolver) =
            let p1, p2 = resolver.Resolve<'T1> (), resolver.Resolve<'T2> ()
            ChoicePickler.Create(p1, p2)

        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>) =
            let writer (w : WriteState) (tag : string) (c : Choice<'T1, 'T2, 'T3>) =
                match c with
                | Choice1Of3 t1 -> 
                    w.Formatter.WriteByte "case" 0uy
                    p1.Write w "Item" t1
                | Choice2Of3 t2 -> 
                    w.Formatter.WriteByte "case" 1uy
                    p2.Write w "Item" t2
                | Choice3Of3 t3 -> 
                    w.Formatter.WriteByte "case" 2uy
                    p3.Write w "Item" t3

            let reader (r : ReadState) (tag : string) =
                match r.Formatter.ReadByte "case" with
                | 0uy -> p1.Read r "Item" |> Choice1Of3
                | 1uy -> p2.Read r "Item" |> Choice2Of3
                | 2uy -> p3.Read r "Item" |> Choice3Of3
                | _ -> raise <| new InvalidDataException()

            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)


        static member Create<'T1, 'T2, 'T3> (resolver : IPicklerResolver) =
            let p1, p2, p3 = resolver.Resolve<'T1> (), resolver.Resolve<'T2> (), resolver.Resolve<'T3> ()
            ChoicePickler.Create(p1, p2, p3)

        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, p4 : Pickler<'T4>) =
            let writer (w : WriteState) (tag : string) (c : Choice<'T1, 'T2, 'T3, 'T4>) =
                match c with
                | Choice1Of4 t1 -> 
                    w.Formatter.WriteByte "case" 0uy
                    p1.Write w "Item" t1
                | Choice2Of4 t2 -> 
                    w.Formatter.WriteByte "case" 1uy
                    p2.Write w "Item" t2
                | Choice3Of4 t3 -> 
                    w.Formatter.WriteByte "case" 2uy
                    p3.Write w "Item" t3
                | Choice4Of4 t4 -> 
                    w.Formatter.WriteByte "case" 3uy
                    p4.Write w "Item" t4

            let reader (r : ReadState) (tag : string) =
                match r.Formatter.ReadByte "case" with
                | 0uy -> p1.Read r "Item" |> Choice1Of4
                | 1uy -> p2.Read r "Item" |> Choice2Of4
                | 2uy -> p3.Read r "Item" |> Choice3Of4
                | 3uy -> p4.Read r "Item" |> Choice4Of4
                | _ -> raise <| new InvalidDataException()

            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

        static member Create<'T1, 'T2, 'T3, 'T4> (resolver : IPicklerResolver) =
            let p1, p2 = resolver.Resolve<'T1> (), resolver.Resolve<'T2> ()
            let p3, p4 = resolver.Resolve<'T3> (), resolver.Resolve<'T4> ()
            ChoicePickler.Create(p1, p2, p3, p4)


    type FSharpRefPickler =
        static member Create (ep : Pickler<'T>) =
            let writer (w : WriteState) (tag : string) (r : 'T ref) =
                ep.Write w "contents" r.Value

            let reader (r : ReadState) (tag : string) =
                { contents = ep.Read r "contents" }

            // do not cache for performance
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = false)
            
        static member Create<'T> (resolver : IPicklerResolver) =
            let ep = resolver.Resolve<'T> ()
            FSharpRefPickler.Create ep

    type FSharpSetPickler =
        static member Create<'T when 'T : comparison>(ep : Pickler<'T>) =
            let writer (w : WriteState) (tag : string) (s : Set<'T>) = 
                writeBoundedSequence w ep s.Count "set" s

            let reader (r : ReadState) (tag : string) =
                readBoundedSequence r ep "set" |> Set.ofArray

            CompositePickler.Create<_>(reader, writer, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = false)
            
        static member Create<'T when 'T : comparison> (resolver : IPicklerResolver) =
            let ep = resolver.Resolve<'T>()
            FSharpSetPickler.Create ep

    type FSharpMapPickler =
        static member Create<'K, 'V when 'K : comparison> (kp : Pickler<'K>, vp : Pickler<'V>) =

            let tp = TuplePickler.Create<'K,'V>(kp, vp)
            
            let writer (w : WriteState) (tag : string) (m : Map<'K,'V>) =
                writeBoundedSequence w tp m.Count "keyvalues" (Map.toSeq m)

            let reader (r : ReadState) (tag : string) =
                readBoundedSequence r tp "keyvalues" |> Map.ofArray

            CompositePickler.Create<_>(reader, writer, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = false)

        static member Create<'K, 'V when 'K : comparison> (resolver : IPicklerResolver) =
            let kp, vp = resolver.Resolve<'K> (), resolver.Resolve<'V> ()
            FSharpMapPickler.Create(kp, vp)


    type DictionaryPickler =
        static member Create<'K, 'V when 'K : equality> (kp : Pickler<'K>, vp : Pickler<'V>) =

            let tp = TuplePickler.Create<'K,'V>(kp, vp)

            let writer (w : WriteState) (tag : string) (d : Dictionary<'K,'V>) =
                let kvs = Seq.map (fun (KeyValue (k,v)) -> k,v) d
                writeBoundedSequence w tp d.Count "dict" kvs

            let reader (r : ReadState) (tag : string) =
                let kvs = readBoundedSequence r tp "dict"
                let d = new Dictionary<'K,'V>()
                for i = 0 to kvs.Length - 1 do
                    let k,v = kvs.[i]
                    d.Add(k,v)
                d

            CompositePickler.Create<_>(reader, writer, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = false)

        static member Create<'K, 'V when 'K : equality> (resolver : IPicklerResolver) =
            let kp, vp = resolver.Resolve<'K>(), resolver.Resolve<'V>()
            DictionaryPickler.Create (kp, vp)


    type AltPickler =
        
        static member Create(tagReader : 'T -> int, picklers : Pickler<'T> list) =
            
            let picklers = List.toArray picklers
            let cacheByRef = picklers |> Array.exists (fun p -> p.IsCacheByRef)
            let useWithSubtypes = picklers |> Array.forall (fun p -> p.UseWithSubtypes)

            let writer (w : WriteState) (tag : string) (t : 'T) =
                let tag = tagReader t
                do w.Formatter.WriteInt32 "tag" tag
                picklers.[tag].Write w "branch" t

            let reader (r : ReadState) (tag : string) =
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
            let writer (w : WriteState) (tag : string) (s : 'S) =
                let t = convert s
                origin.Write w tag t

            let reader (r : ReadState) (tag : string) =
                let t = origin.Read r tag
                recover t

            CompositePickler.Create<_>(reader, writer, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = useWithSubtypes)

    type SeqPickler =
        static member Create(ep : Pickler<'T>) =
            CompositePickler.Create<_>(readUnboundedSequence ep, writeUnboundedSequence ep, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = true)