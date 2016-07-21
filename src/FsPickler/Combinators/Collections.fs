namespace MBrace.FsPickler

open System
open System.Collections.Generic

open MBrace.FsPickler.SequenceUtils

type internal FSharpSetPickler =
    static member Create<'T when 'T : comparison>(ep : Pickler<'T>) =
        let writer (w : WriteState) (_ : string) (set : Set<'T>) =
            let formatter = w.Formatter
            let count = set.Count
            formatter.WriteInt32 "count" count
            writeBoundedSequence ep count w "elements" set

        let reader (r : ReadState) (_ : string) =
            let formatter = r.Formatter
            let count = formatter.ReadInt32 "count"
            let elements = readBoundedSequence ep count r "elements"
            Set.ofArray elements

        let cloner (c : CloneState) (s : Set<'T>) =
            s |> Set.map (ep.Clone c)

        let accepter (v : VisitState) (s : Set<'T>) =
            for t in s do ep.Accept v t

        CompositePickler.Create<_>(reader, writer, cloner, accepter, PicklerInfo.Combinator, useWithSubtypes = true)
            
    static member Create<'T when 'T : comparison> (resolver : IPicklerResolver) =
        let ep = resolver.Resolve<'T>()
        FSharpSetPickler.Create ep

type internal FSharpMapPickler =
    static member Create<'K, 'V when 'K : comparison> (kp : Pickler<'K>, vp : Pickler<'V>) =

        let tp = TuplePickler.Create<'K,'V>(kp, vp)
            
        let writer (w : WriteState) (_ : string) (map : Map<'K,'V>) =
            let count = map.Count
            w.Formatter.WriteInt32 "count" count
            writeBoundedSequence tp count w "items" <| Map.toSeq map

        let reader (r : ReadState) (_ : string) =
            let count = r.Formatter.ReadInt32 "count"
            let items = readBoundedSequence tp count r "items"
            Map.ofArray items

        let cloner (c : CloneState) (map : Map<'K,'V>) =
            map |> Seq.map (fun kv -> kp.Clone c kv.Key, vp.Clone c kv.Value) |> Map.ofSeq

        let accepter (v : VisitState) (map : Map<'K,'V>) =
            for kv in map do kp.Accept v kv.Key ; vp.Accept v kv.Value

        CompositePickler.Create<_>(reader, writer, cloner, accepter, PicklerInfo.Combinator, useWithSubtypes = true)

    static member Create<'K, 'V when 'K : comparison> (resolver : IPicklerResolver) =
        let kp, vp = resolver.Resolve<'K> (), resolver.Resolve<'V> ()
        FSharpMapPickler.Create(kp, vp)


type internal DictionaryPickler =
    static member Create<'K, 'V when 'K : equality> (kp : Pickler<'K>, vp : Pickler<'V>) =

        let tp = TuplePickler.Create<'K,'V>(kp, vp)

        let writer (w : WriteState) (_ : string) (d : Dictionary<'K,'V>) =
            let count = d.Count
            w.Formatter.WriteInt32 "count" d.Count
            let kvs = Seq.map (fun (KeyValue (k,v)) -> k,v) d
            writeBoundedSequence tp count w "items" kvs

        let reader (r : ReadState) (_ : string) =
            let count = r.Formatter.ReadInt32 "count"
            let dict = new Dictionary<'K,'V> (count)
            let items = readBoundedSequence tp count r "items"
            for k,v in items do dict.Add(k,v)
            dict

        let cloner (c : CloneState) (d : Dictionary<'K,'V>) =
            let d' = new Dictionary<'K,'V>(d.Count)
            for kv in d do d'.Add(kp.Clone c kv.Key, vp.Clone c kv.Value)
            d'

        let accepter (v : VisitState) (d : Dictionary<'K,'V>) =
            for kv in d do kp.Accept v kv.Key ; vp.Accept v kv.Value

        CompositePickler.Create<_>(reader, writer, cloner, accepter, PicklerInfo.Combinator, useWithSubtypes = false)

    static member Create<'K, 'V when 'K : equality> (resolver : IPicklerResolver) =
        let kp, vp = resolver.Resolve<'K>(), resolver.Resolve<'V>()
        DictionaryPickler.Create (kp, vp)