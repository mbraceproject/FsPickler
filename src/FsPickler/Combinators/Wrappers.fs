namespace Nessos.FsPickler

    open Microsoft.FSharp.Reflection

    open Nessos.FsPickler.Reflection

    type internal AltPickler =
        
        static member Create(tagReader : 'T -> int, picklers : Pickler<'T> list) =
            
            let picklers = List.toArray picklers
            let cacheByRef = picklers |> Array.exists (fun p -> p.IsCacheByRef)
            let useWithSubtypes = picklers |> Array.forall (fun p -> p.UseWithSubtypes)

            let writer (w : WriteState) (tag : string) (t : 'T) =
                let tag = tagReader t
                do w.Formatter.WriteInt32 "tag" tag
                picklers.[tag].Write w "case" t

            let reader (r : ReadState) (tag : string) =
                let tag = r.Formatter.ReadInt32 "tag"
                picklers.[tag].Read r "case"

            CompositePickler.Create<_>(reader, writer, PicklerInfo.Combinator, 
                                cacheByRef = cacheByRef, useWithSubtypes = useWithSubtypes)


    type internal WrapPickler =
        static member Create(origin : Pickler<'T>, recover : 'T -> 'S, convert : 'S -> 'T) =
//#if OPTIMIZE_FSHARP
//            // disable subtype resolution if F# union or tuple
//            let useWithSubtypes = not <| Reflection.isRecursiveType true typeof<'S>
//#else
//            let useWithSubtypes = FSharpType.IsUnion(typeof<'S>, allMembers)
//#endif
            let writer (w : WriteState) (tag : string) (s : 'S) =
                let t = convert s
                origin.Write w tag t

            let reader (r : ReadState) (tag : string) =
                let t = origin.Read r tag
                recover t

            CompositePickler.Create<_>(reader, writer, PicklerInfo.Combinator, cacheByRef = false, useWithSubtypes = false)