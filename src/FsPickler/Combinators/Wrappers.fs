namespace Nessos.FsPickler

open Microsoft.FSharp.Reflection

open Nessos.FsPickler.Reflection

type internal AltPickler =
        
    static member Create(tagReader : 'T -> int, picklers : Pickler<'T> list) =
            
        let picklers = List.toArray picklers

        let writer (w : WriteState) (tag : string) (t : 'T) =
            let tag = tagReader t
            do w.Formatter.WriteInt32 "Tag" tag
            picklers.[tag].Write w "Value" t

        let reader (r : ReadState) (tag : string) =
            let tag = r.Formatter.ReadInt32 "Tag"
            picklers.[tag].Read r "Value"

        CompositePickler.Create<'T>(reader, writer, PicklerInfo.Combinator, cacheByRef = false)


type internal WrapPickler =
    static member Create(origin : Pickler<'T>, recover : 'T -> 'S, convert : 'S -> 'T) =
        let writer (w : WriteState) (tag : string) (s : 'S) =
            let t = convert s
            origin.Write w tag t

        let reader (r : ReadState) (tag : string) =
            let t = origin.Read r tag
            recover t

        CompositePickler.Create<'S>(reader, writer, PicklerInfo.Combinator, cacheByRef = false, useWithSubtypes = false, bypass = true)