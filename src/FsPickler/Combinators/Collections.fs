namespace Nessos.FsPickler

    open System
    open System.Collections.Generic

    open Nessos.FsPickler.SequenceUtils

    type internal FSharpSetPickler =
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

    type internal FSharpMapPickler =
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


    type internal DictionaryPickler =
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