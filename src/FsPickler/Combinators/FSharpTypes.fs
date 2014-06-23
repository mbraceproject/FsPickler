namespace Nessos.FsPickler

    open System
    open System.IO

    // Pickler implementations for common F# generic types

    type internal OptionPickler =

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


    type internal ChoicePickler private () =

        static let c2 = UnionCaseSerializationHelper.OfUnionType<Choice<_,_>> ()
        static let c3 = UnionCaseSerializationHelper.OfUnionType<Choice<_,_,_>> ()
        static let c4 = UnionCaseSerializationHelper.OfUnionType<Choice<_,_,_,_>> ()

        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>) =

            let writer (w : WriteState) (tag : string) (c : Choice<'T1, 'T2>) =
                match c with
                | Choice1Of2 t1 -> 
                    c2.WriteTag(w.Formatter, 0)
                    p1.Write w "Item" t1
                | Choice2Of2 t2 -> 
                    c2.WriteTag(w.Formatter, 1)
                    p2.Write w "Item" t2

            let reader (r : ReadState) (tag : string) =
                match c2.ReadTag r.Formatter with
                | 0 -> p1.Read r "Item" |> Choice1Of2
                | 1 -> p2.Read r "Item" |> Choice2Of2
                | _ -> raise <| new FormatException("invalid choice branch.")

            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)


        static member Create<'T1, 'T2> (resolver : IPicklerResolver) =
            let p1, p2 = resolver.Resolve<'T1> (), resolver.Resolve<'T2> ()
            ChoicePickler.Create(p1, p2)

        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>) =
            let writer (w : WriteState) (tag : string) (c : Choice<'T1, 'T2, 'T3>) =
                match c with
                | Choice1Of3 t1 -> 
                    c3.WriteTag(w.Formatter, 0)
                    p1.Write w "Item" t1
                | Choice2Of3 t2 -> 
                    c3.WriteTag(w.Formatter, 1)
                    p2.Write w "Item" t2
                | Choice3Of3 t3 -> 
                    c3.WriteTag(w.Formatter, 2)
                    p3.Write w "Item" t3

            let reader (r : ReadState) (tag : string) =
                match c3.ReadTag r.Formatter with
                | 0 -> p1.Read r "Item" |> Choice1Of3
                | 1 -> p2.Read r "Item" |> Choice2Of3
                | 2 -> p3.Read r "Item" |> Choice3Of3
                | _ -> raise <| new FormatException("invalid choice branch.")

            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)


        static member Create<'T1, 'T2, 'T3> (resolver : IPicklerResolver) =
            let p1, p2, p3 = resolver.Resolve<'T1> (), resolver.Resolve<'T2> (), resolver.Resolve<'T3> ()
            ChoicePickler.Create(p1, p2, p3)

        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, p4 : Pickler<'T4>) =
            let writer (w : WriteState) (tag : string) (c : Choice<'T1, 'T2, 'T3, 'T4>) =
                match c with
                | Choice1Of4 t1 -> 
                    c4.WriteTag(w.Formatter, 0)
                    p1.Write w "Item" t1
                | Choice2Of4 t2 -> 
                    c4.WriteTag(w.Formatter, 1)
                    p2.Write w "Item" t2
                | Choice3Of4 t3 -> 
                    c4.WriteTag(w.Formatter, 2)
                    p3.Write w "Item" t3
                | Choice4Of4 t4 -> 
                    c4.WriteTag(w.Formatter, 3)
                    p4.Write w "Item" t4

            let reader (r : ReadState) (tag : string) =
                match c3.ReadTag r.Formatter with
                | 0 -> p1.Read r "Item" |> Choice1Of4
                | 1 -> p2.Read r "Item" |> Choice2Of4
                | 2 -> p3.Read r "Item" |> Choice3Of4
                | 3 -> p4.Read r "Item" |> Choice4Of4
                | _ -> raise <| new FormatException("invalid choice branch.")

            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

        static member Create<'T1, 'T2, 'T3, 'T4> (resolver : IPicklerResolver) =
            let p1, p2 = resolver.Resolve<'T1> (), resolver.Resolve<'T2> ()
            let p3, p4 = resolver.Resolve<'T3> (), resolver.Resolve<'T4> ()
            ChoicePickler.Create(p1, p2, p3, p4)


    type internal FSharpRefPickler =
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