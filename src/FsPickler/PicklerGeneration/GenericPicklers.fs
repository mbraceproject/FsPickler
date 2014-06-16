module internal Nessos.FsPickler.CombinatorImpls

    open System
    open System.IO
    open System.Reflection
    open System.Collections.Generic

    open Microsoft.FSharp.Reflection

    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection
    open Nessos.FsPickler.TuplePicklers

    //
    //  Pickler combinator implementations for common generic types
    //

    // F# list pickler combinator

    type ListPickler =
        static member Create (ep : Pickler<'T>) =
            let writer (w : WriteState) (tag : string) (list : 'T list) =

                let formatter = w.Formatter

                if ep.TypeKind = TypeKind.Primitive && formatter.IsPrimitiveArraySerializationSupported then
                    let arr = List.toArray list
                    formatter.BeginWriteObject tag ObjectFlags.None
                    formatter.WriteInt32 "length" arr.Length
                    formatter.WritePrimitiveArray "data" arr
                    formatter.EndWriteObject ()

                elif ep.IsRecursiveType || formatter.PreferLengthPrefixInSequences then

                    let rec writeL (ts : 'T list) =
                        match ts with
                        | t :: tl -> ep.Write w "elem" t ; writeL tl
                        | [] -> ()

                    formatter.BeginWriteObject tag ObjectFlags.None
                    formatter.WriteInt32 "length" list.Length
                    formatter.BeginWriteObject "list" ObjectFlags.IsSequenceHeader
                    do writeL list
                    formatter.EndWriteObject ()
                    formatter.EndWriteObject ()

                else
                    let rec writeL (ts : 'T list) =
                        match ts with
                        | t :: tl -> 
                            formatter.WriteNextSequenceElement true
                            ep.Write w "elem" t
                            writeL tl

                        | [] -> formatter.WriteNextSequenceElement false

                    formatter.BeginWriteObject tag ObjectFlags.IsSequenceHeader
                    writeL list
                    formatter.EndWriteObject ()


            let reader (r : ReadState) (tag : string) =
                let formatter = r.Formatter

                if ep.TypeKind = TypeKind.Primitive && formatter.IsPrimitiveArraySerializationSupported then
                    let length = r.Formatter.ReadInt32 "length"
                    let array = Array.zeroCreate<'T> length
                    formatter.ReadPrimitiveArray "data" array
                    Array.toList array
                elif ep.IsRecursiveType || formatter.PreferLengthPrefixInSequences then

                    let length = formatter.ReadInt32 "length"
                    let array = Array.zeroCreate<'T> length
                    if formatter.BeginReadObject "list" <> ObjectFlags.IsSequenceHeader then
                        raise <| new InvalidPickleException(sprintf "Error deserializing object of type '%O': expected new array." typeof<'T []>)

                    for i = 0 to length - 1 do
                        array.[i] <- ep.Read r "elem"

                    formatter.EndReadObject ()

                    Array.toList array
                else
                    let ra = new ResizeArray<'T> ()

                    while formatter.ReadNextSequenceElement() do
                        ra.Add <| ep.Read r "elem"

                    Seq.toList ra

            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = true, skipHeaderWrite = true)

        static member Create<'T>(resolver : IPicklerResolver) =
            let ep = resolver.Resolve<'T> ()
            ListPickler.Create<'T> ep

    type SeqPickler =
        static member Create(ep : Pickler<'T>) =
            let writer (w : WriteState) (tag : string) (ts : seq<'T>) =
                let formatter = w.Formatter
                formatter.BeginWriteObject tag ObjectFlags.IsSequenceHeader
                for t in ts do
                    formatter.WriteNextSequenceElement true
                    ep.Write w "elem" t

                formatter.WriteNextSequenceElement false
                formatter.EndWriteObject()

            let reader (r : ReadState) (tag : string) =
                let formatter = r.Formatter
                let ra = new ResizeArray<'T> ()
                while formatter.ReadNextSequenceElement() do
                    ra.Add <| ep.Read r "elem"

                ra :> seq<'T>

            CompositePickler.Create<_>(reader, writer, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = true, skipHeaderWrite = true)

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
            let writer (w : WriteState) (tag : string) (set : Set<'T>) =
                let formatter = w.Formatter
                formatter.WriteInt32 "count" set.Count
                formatter.BeginWriteObject "elements" ObjectFlags.IsSequenceHeader
                for e in set do ep.Write w "elem" e
                formatter.EndWriteObject ()

            let reader (r : ReadState) (tag : string) =
                let formatter = r.Formatter
                let count = formatter.ReadInt32 "count"
                let array = Array.zeroCreate<'T> count
                let _ = formatter.BeginReadObject "elements"
                for i = 0 to array.Length - 1 do array.[i] <- ep.Read r "elem"
                formatter.EndReadObject ()
                
                Set.ofArray array

            CompositePickler.Create<_>(reader, writer, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = false)
            
        static member Create<'T when 'T : comparison> (resolver : IPicklerResolver) =
            let ep = resolver.Resolve<'T>()
            FSharpSetPickler.Create ep

    type FSharpMapPickler =
        static member Create<'K, 'V when 'K : comparison> (kp : Pickler<'K>, vp : Pickler<'V>) =

            let tp = TuplePickler.Create<'K,'V>(kp, vp)
            
            let writer (w : WriteState) (tag : string) (map : Map<'K,'V>) =
                w.Formatter.WriteInt32 "count" map.Count
                w.Formatter.BeginWriteObject "keyvals" ObjectFlags.IsSequenceHeader
                for e in Map.toSeq map do tp.Write w "entry" e
                w.Formatter.EndWriteObject ()

            let reader (r : ReadState) (tag : string) =
                let count = r.Formatter.ReadInt32 "count"
                let _ = r.Formatter.BeginReadObject "keyvals"
                let array = Array.zeroCreate<'K * 'V> count
                for i = 0 to array.Length - 1 do array.[i] <- tp.Read r "entry"
                r.Formatter.EndReadObject ()
                Map.ofArray array

            CompositePickler.Create<_>(reader, writer, PicklerInfo.Combinator, cacheByRef = true, useWithSubtypes = false)

        static member Create<'K, 'V when 'K : comparison> (resolver : IPicklerResolver) =
            let kp, vp = resolver.Resolve<'K> (), resolver.Resolve<'V> ()
            FSharpMapPickler.Create(kp, vp)


    type DictionaryPickler =
        static member Create<'K, 'V when 'K : equality> (kp : Pickler<'K>, vp : Pickler<'V>) =

            let tp = TuplePickler.Create<'K,'V>(kp, vp)

            let writer (w : WriteState) (tag : string) (d : Dictionary<'K,'V>) =
                w.Formatter.WriteInt32 "count" d.Count
                w.Formatter.BeginWriteObject "keyvals" ObjectFlags.IsSequenceHeader
                for e in Seq.map (fun (KeyValue (k,v)) -> k,v) d do tp.Write w "entry" e
                w.Formatter.EndWriteObject ()

            let reader (r : ReadState) (tag : string) =
                let d = new Dictionary<'K,'V>()
                let count = r.Formatter.ReadInt32 "count"
                let _ = r.Formatter.BeginReadObject "keyvals"
                for i = 1 to count do
                    let k,v = tp.Read r "entry"
                    d.Add(k,v)

                r.Formatter.EndReadObject()
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