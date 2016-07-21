namespace MBrace.FsPickler

open Microsoft.FSharp.Reflection

open MBrace.FsPickler.Reflection

type internal AltPickler =
        
    static member Create(tagReader : 'T -> int, picklers : Pickler<'T> list) =
            
        let picklers = List.toArray picklers

        let writer (w : WriteState) (id : string) (t : 'T) =
            let tag = tagReader t
            do w.Formatter.WriteInt32 "Tag" tag
            match picklers.[tag] with
            | :? CompositePickler<'T> as cp -> cp.Writer w id t
            | p -> p.Write w id t

        let reader (r : ReadState) (id : string) =
            let tag = r.Formatter.ReadInt32 "Tag"
            match picklers.[tag] with
            | :? CompositePickler<'T> as cp -> cp.Reader r id
            | p -> p.Read r id

        let cloner (c : CloneState) (t : 'T) =
            let tag = tagReader t
            match picklers.[tag] with
            | :? CompositePickler<'T> as cp -> cp.Cloner c t
            | p -> p.Clone c t

        let accepter (c : VisitState) (t : 'T) =
            let tag = tagReader t
            picklers.[tag].Accept c t

        CompositePickler.Create<'T>(reader, writer, cloner, accepter, PicklerInfo.Combinator, skipVisit = true)


type internal WrapPickler =
    static member Create(origin : Pickler<'T>, recover : 'T -> 'S, convert : 'S -> 'T) =
        let writer (w : WriteState) (tag : string) (s : 'S) =
            let t = convert s
            origin.Write w tag t

        let reader (r : ReadState) (tag : string) =
            let t = origin.Read r tag
            recover t

        let cloner (c : CloneState) (s : 'S) =
            let t = convert s
            let t' = origin.Clone c t
            recover t'

        let accepter (v : VisitState) (s : 'S) =
            let t = convert s
            origin.Accept v t

        CompositePickler.Create<'S>(reader, writer, cloner, accepter, PicklerInfo.Combinator, cacheByRef = false, bypass = true, skipVisit = true)