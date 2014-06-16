module internal Nessos.FsPickler.TuplePicklers

    open System

    open Nessos.FsPickler
//    open Nessos.FsPickler.PicklerUtils

    type TuplePickler =

        static member Create(p1 : Pickler<'T1>) =
            let writer (w : WriteState) (tag : string) (t : Tuple<'T1>) =
                p1.Write w "item1" t.Item1

            let reader (r : ReadState) (tag : string) =
                let t1 = p1.Read r "item1"
                new Tuple<'T1>(t1)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = false)
#endif

        static member Create<'T1> (resolver : IPicklerResolver) =
            let p1 = resolver.Resolve<'T1> ()
            TuplePickler.Create p1


        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>) =
            let writer (w : WriteState) (tag : string) ((t1,t2) : 'T1 * 'T2) =
                p1.Write w "item1" t1
                p2.Write w "item2" t2

            let reader (r : ReadState) (tag : string) =
                let t1 = p1.Read r "item1"
                let t2 = p2.Read r "item2"
                (t1, t2)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = false)
#endif

        static member Create<'T1, 'T2> (resolver : IPicklerResolver) =
            let p1 = resolver.Resolve<'T1> ()
            let p2 = resolver.Resolve<'T2> ()
                
            TuplePickler.Create(p1, p2)

        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>) =
            let writer (w : WriteState) (tag : string) ((t1,t2,t3) : 'T1 * 'T2 * 'T3) =
                p1.Write w "item1" t1
                p2.Write w "item2" t2
                p3.Write w "item3" t3

            let reader (r : ReadState) (tag : string) =
                let t1 = p1.Read r "item1"
                let t2 = p2.Read r "item2"
                let t3 = p3.Read r "item3"
                (t1, t2, t3)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = false)
#endif

        static member Create<'T1, 'T2, 'T3> (resolver : IPicklerResolver) =
            let p1 = resolver.Resolve<'T1> ()
            let p2 = resolver.Resolve<'T2> ()
            let p3 = resolver.Resolve<'T3> ()
                
            TuplePickler.Create(p1, p2, p3)


        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, p4 : Pickler<'T4>) =
            let writer (w : WriteState) (tag : string) ((t1,t2,t3,t4) : 'T1 * 'T2 * 'T3 * 'T4) =
                p1.Write w "item1" t1
                p2.Write w "item2" t2
                p3.Write w "item3" t3
                p4.Write w "item4" t4

            let reader (r : ReadState) (tag : string) =
                let t1 = p1.Read r "item1"
                let t2 = p2.Read r "item2"
                let t3 = p3.Read r "item3"
                let t4 = p4.Read r "item4"
                (t1, t2, t3, t4)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = false)
#endif

        static member Create<'T1, 'T2, 'T3, 'T4> (resolver : IPicklerResolver) =
            let p1 = resolver.Resolve<'T1> ()
            let p2 = resolver.Resolve<'T2> ()
            let p3 = resolver.Resolve<'T3> ()
            let p4 = resolver.Resolve<'T4> ()
                
            TuplePickler.Create(p1, p2, p3, p4)


        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, p4 : Pickler<'T4>, p5 : Pickler<'T5>) =
            let writer (w : WriteState) (tag : string) ((t1,t2,t3,t4,t5) : 'T1 * 'T2 * 'T3 * 'T4 * 'T5) =
                p1.Write w "item1" t1
                p2.Write w "item2" t2
                p3.Write w "item3" t3
                p4.Write w "item4" t4
                p5.Write w "item5" t5

            let reader (r : ReadState) (tag : string) =
                let t1 = p1.Read r "item1"
                let t2 = p2.Read r "item2"
                let t3 = p3.Read r "item3"
                let t4 = p4.Read r "item4"
                let t5 = p5.Read r "item5"
                (t1, t2, t3, t4, t5)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = false)
#endif

        static member Create<'T1, 'T2, 'T3, 'T4, 'T5> (resolver : IPicklerResolver) =
            let p1 = resolver.Resolve<'T1> ()
            let p2 = resolver.Resolve<'T2> ()
            let p3 = resolver.Resolve<'T3> ()
            let p4 = resolver.Resolve<'T4> ()
            let p5 = resolver.Resolve<'T5> ()
                
            TuplePickler.Create(p1, p2, p3, p4, p5)


        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, p4 : Pickler<'T4>, p5 : Pickler<'T5>, p6 : Pickler<'T6>) =
            let writer (w : WriteState) (tag : string) ((t1,t2,t3,t4,t5,t6) : 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6) =
                p1.Write w "item1" t1
                p2.Write w "item2" t2
                p3.Write w "item3" t3
                p4.Write w "item4" t4
                p5.Write w "item5" t5
                p6.Write w "item6" t6

            let reader (r : ReadState) (tag : string) =
                let t1 = p1.Read r "item1"
                let t2 = p2.Read r "item2"
                let t3 = p3.Read r "item3"
                let t4 = p4.Read r "item4"
                let t5 = p5.Read r "item5"
                let t6 = p6.Read r "item6"
                (t1, t2, t3, t4, t5, t6)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = false)
#endif

        static member Create<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> (resolver : IPicklerResolver) =
            let p1 = resolver.Resolve<'T1> ()
            let p2 = resolver.Resolve<'T2> ()
            let p3 = resolver.Resolve<'T3> ()
            let p4 = resolver.Resolve<'T4> ()
            let p5 = resolver.Resolve<'T5> ()
            let p6 = resolver.Resolve<'T6> ()
                
            TuplePickler.Create(p1, p2, p3, p4, p5, p6)


        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, 
                                p4 : Pickler<'T4>, p5 : Pickler<'T5>, p6 : Pickler<'T6>, 
                                p7 : Pickler<'T7>) =

            let writer (w : WriteState) (tag : string) ((t1,t2,t3,t4,t5,t6,t7) : 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7) =
                p1.Write w "item1" t1
                p2.Write w "item2" t2
                p3.Write w "item3" t3
                p4.Write w "item4" t4
                p5.Write w "item5" t5
                p6.Write w "item6" t6
                p7.Write w "item7" t7

            let reader (r : ReadState) (tag : string) =
                let t1 = p1.Read r "item1"
                let t2 = p2.Read r "item2"
                let t3 = p3.Read r "item3"
                let t4 = p4.Read r "item4"
                let t5 = p5.Read r "item5"
                let t6 = p6.Read r "item6"
                let t7 = p7.Read r "item7"
                (t1, t2, t3, t4, t5, t6, t7)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = false)
#endif

        static member Create<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> (resolver : IPicklerResolver) =
            let p1 = resolver.Resolve<'T1> ()
            let p2 = resolver.Resolve<'T2> ()
            let p3 = resolver.Resolve<'T3> ()
            let p4 = resolver.Resolve<'T4> ()
            let p5 = resolver.Resolve<'T5> ()
            let p6 = resolver.Resolve<'T6> ()
            let p7 = resolver.Resolve<'T7> ()
                
            TuplePickler.Create(p1, p2, p3, p4, p5, p6, p7)


        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, p4 : Pickler<'T4>,
                                p5 : Pickler<'T5>, p6 : Pickler<'T6>, p7 : Pickler<'T7>, pr : Pickler<'TRest>) =

            let writer (w : WriteState) (tag : string) (tuple : Tuple<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'TRest>) =
                p1.Write w "item1" tuple.Item1
                p2.Write w "item2" tuple.Item2
                p3.Write w "item3" tuple.Item3
                p4.Write w "item4" tuple.Item4
                p5.Write w "item5" tuple.Item5
                p6.Write w "item6" tuple.Item6
                p7.Write w "item7" tuple.Item7
                pr.Write w "rest" tuple.Rest

            let reader (r : ReadState) (tag : string) =
                let t1 = p1.Read r "item1"
                let t2 = p2.Read r "item2"
                let t3 = p3.Read r "item3"
                let t4 = p4.Read r "item4"
                let t5 = p5.Read r "item5"
                let t6 = p6.Read r "item6"
                let t7 = p7.Read r "item7"
                let rest = pr.Read r "rest"
                new Tuple<_,_,_,_,_,_,_,_>(t1,t2,t3,t4,t5,t6,t7,rest)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            CompositePickler.Create<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = false)
#endif

        static member Create<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest> (resolver : IPicklerResolver) =
            let p1 = resolver.Resolve<'T1> ()
            let p2 = resolver.Resolve<'T2> ()
            let p3 = resolver.Resolve<'T3> ()
            let p4 = resolver.Resolve<'T4> ()
            let p5 = resolver.Resolve<'T5> ()
            let p6 = resolver.Resolve<'T6> ()
            let p7 = resolver.Resolve<'T7> ()
            let pr = resolver.Resolve<'TRest> ()
                
            TuplePickler.Create(p1, p2, p3, p4, p5, p6, p7, pr)