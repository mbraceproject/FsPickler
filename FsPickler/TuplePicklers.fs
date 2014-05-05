module internal Nessos.FsPickler.TuplePicklers

    open System

    open Nessos.FsPickler
    open Nessos.FsPickler.PicklerUtils

    type Tuple1Pickler () =
        static member Create(p1 : Pickler<'T1>) =
            let writer (w : Writer) (t : Tuple<'T1>) =
                write (isPrimitive p1) w p1 "item1" t.Item1

            let reader (r : Reader) =
                let t1 = read (isPrimitive p1) r p1 "item1"
                new Tuple<'T1>(t1)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = false)
#endif
            
        interface IGenericPicklerFactory1 with
            member __.Create<'T1> (resolver : IPicklerResolver) =
                let p1 = resolver.Resolve<'T1> ()
                
                Tuple1Pickler.Create(p1) :> Pickler

    type Tuple2Pickler () =
        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>) =
            let writer (w : Writer) ((t1,t2) : 'T1 * 'T2) =
                write (isPrimitive p1) w p1 "item1" t1
                write (isPrimitive p2) w p2 "item2" t2

            let reader (r : Reader) =
                let t1 = read (isPrimitive p1) r p1 "item1"
                let t2 = read (isPrimitive p2) r p2 "item2"
                (t1, t2)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = false)
#endif
            
        interface IGenericPicklerFactory2 with
            member __.Create<'T1, 'T2> (resolver : IPicklerResolver) =
                let p1 = resolver.Resolve<'T1> ()
                let p2 = resolver.Resolve<'T2> ()
                
                Tuple2Pickler.Create(p1, p2) :> Pickler

    type Tuple3Pickler () =
        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>) =
            let writer (w : Writer) ((t1,t2,t3) : 'T1 * 'T2 * 'T3) =
                write (isPrimitive p1) w p1 "item1" t1
                write (isPrimitive p2) w p2 "item2" t2
                write (isPrimitive p3) w p3 "item3" t3

            let reader (r : Reader) =
                let t1 = read (isPrimitive p1) r p1 "item1"
                let t2 = read (isPrimitive p2) r p2 "item2"
                let t3 = read (isPrimitive p3) r p3 "item3"
                (t1, t2, t3)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = false)
#endif
            
        interface IGenericPicklerFactory3 with
            member __.Create<'T1, 'T2, 'T3> (resolver : IPicklerResolver) =
                let p1 = resolver.Resolve<'T1> ()
                let p2 = resolver.Resolve<'T2> ()
                let p3 = resolver.Resolve<'T3> ()
                
                Tuple3Pickler.Create(p1, p2, p3) :> Pickler

    type Tuple4Pickler () =
        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, p4 : Pickler<'T4>) =
            let writer (w : Writer) ((t1,t2,t3,t4) : 'T1 * 'T2 * 'T3 * 'T4) =
                write (isPrimitive p1) w p1 "item1" t1
                write (isPrimitive p2) w p2 "item2" t2
                write (isPrimitive p3) w p3 "item3" t3
                write (isPrimitive p4) w p4 "item4" t4

            let reader (r : Reader) =
                let t1 = read (isPrimitive p1) r p1 "item1"
                let t2 = read (isPrimitive p2) r p2 "item2"
                let t3 = read (isPrimitive p3) r p3 "item3"
                let t4 = read (isPrimitive p4) r p4 "item4"
                (t1, t2, t3, t4)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = false)
#endif
            
        interface IGenericPicklerFactory4 with
            member __.Create<'T1, 'T2, 'T3, 'T4> (resolver : IPicklerResolver) =
                let p1 = resolver.Resolve<'T1> ()
                let p2 = resolver.Resolve<'T2> ()
                let p3 = resolver.Resolve<'T3> ()
                let p4 = resolver.Resolve<'T4> ()
                
                Tuple4Pickler.Create(p1, p2, p3, p4) :> Pickler


    type Tuple5Pickler () =
        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, p4 : Pickler<'T4>, p5 : Pickler<'T5>) =
            let writer (w : Writer) ((t1,t2,t3,t4,t5) : 'T1 * 'T2 * 'T3 * 'T4 * 'T5) =
                write (isPrimitive p1) w p1 "item1" t1
                write (isPrimitive p2) w p2 "item2" t2
                write (isPrimitive p3) w p3 "item3" t3
                write (isPrimitive p4) w p4 "item4" t4
                write (isPrimitive p5) w p5 "item5" t5

            let reader (r : Reader) =
                let t1 = read (isPrimitive p1) r p1 "item1"
                let t2 = read (isPrimitive p2) r p2 "item2"
                let t3 = read (isPrimitive p3) r p3 "item3"
                let t4 = read (isPrimitive p4) r p4 "item4"
                let t5 = read (isPrimitive p5) r p5 "item5"
                (t1, t2, t3, t4, t5)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = false)
#endif
            
        interface IGenericPicklerFactory5 with
            member __.Create<'T1, 'T2, 'T3, 'T4, 'T5> (resolver : IPicklerResolver) =
                let p1 = resolver.Resolve<'T1> ()
                let p2 = resolver.Resolve<'T2> ()
                let p3 = resolver.Resolve<'T3> ()
                let p4 = resolver.Resolve<'T4> ()
                let p5 = resolver.Resolve<'T5> ()
                
                Tuple5Pickler.Create(p1, p2, p3, p4, p5) :> Pickler

    type Tuple6Pickler () =
        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, p4 : Pickler<'T4>, p5 : Pickler<'T5>, p6 : Pickler<'T6>) =
            let writer (w : Writer) ((t1,t2,t3,t4,t5,t6) : 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6) =
                write (isPrimitive p1) w p1 "item1" t1
                write (isPrimitive p2) w p2 "item2" t2
                write (isPrimitive p3) w p3 "item3" t3
                write (isPrimitive p4) w p4 "item4" t4
                write (isPrimitive p5) w p5 "item5" t5
                write (isPrimitive p6) w p6 "item6" t6

            let reader (r : Reader) =
                let t1 = read (isPrimitive p1) r p1 "item1"
                let t2 = read (isPrimitive p2) r p2 "item2"
                let t3 = read (isPrimitive p3) r p3 "item3"
                let t4 = read (isPrimitive p4) r p4 "item4"
                let t5 = read (isPrimitive p5) r p5 "item5"
                let t6 = read (isPrimitive p6) r p6 "item6"
                (t1, t2, t3, t4, t5, t6)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = false)
#endif
            
        interface IGenericPicklerFactory6 with
            member __.Create<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> (resolver : IPicklerResolver) =
                let p1 = resolver.Resolve<'T1> ()
                let p2 = resolver.Resolve<'T2> ()
                let p3 = resolver.Resolve<'T3> ()
                let p4 = resolver.Resolve<'T4> ()
                let p5 = resolver.Resolve<'T5> ()
                let p6 = resolver.Resolve<'T6> ()
                
                Tuple6Pickler.Create(p1, p2, p3, p4, p5, p6) :> Pickler

    type Tuple7Pickler () =
        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, 
                                p4 : Pickler<'T4>, p5 : Pickler<'T5>, p6 : Pickler<'T6>, p7 : Pickler<'T7>) =

            let writer (w : Writer) ((t1,t2,t3,t4,t5,t6,t7) : 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7) =
                write (isPrimitive p1) w p1 "item1" t1
                write (isPrimitive p2) w p2 "item2" t2
                write (isPrimitive p3) w p3 "item3" t3
                write (isPrimitive p4) w p4 "item4" t4
                write (isPrimitive p5) w p5 "item5" t5
                write (isPrimitive p6) w p6 "item6" t6
                write (isPrimitive p7) w p7 "item7" t7

            let reader (r : Reader) =
                let t1 = read (isPrimitive p1) r p1 "item1"
                let t2 = read (isPrimitive p2) r p2 "item2"
                let t3 = read (isPrimitive p3) r p3 "item3"
                let t4 = read (isPrimitive p4) r p4 "item4"
                let t5 = read (isPrimitive p5) r p5 "item5"
                let t6 = read (isPrimitive p6) r p6 "item6"
                let t7 = read (isPrimitive p7) r p7 "item7"
                (t1, t2, t3, t4, t5, t6, t7)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = false)
#endif
            
        interface IGenericPicklerFactory7 with
            member __.Create<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> (resolver : IPicklerResolver) =
                let p1 = resolver.Resolve<'T1> ()
                let p2 = resolver.Resolve<'T2> ()
                let p3 = resolver.Resolve<'T3> ()
                let p4 = resolver.Resolve<'T4> ()
                let p5 = resolver.Resolve<'T5> ()
                let p6 = resolver.Resolve<'T6> ()
                let p7 = resolver.Resolve<'T7> ()
                
                Tuple7Pickler.Create(p1, p2, p3, p4, p5, p6, p7) :> Pickler

    type Tuple8Pickler () =
        static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, p4 : Pickler<'T4>,
                                p5 : Pickler<'T5>, p6 : Pickler<'T6>, p7 : Pickler<'T7>, pr : Pickler<'TRest>) =

            let writer (w : Writer) (tuple : Tuple<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'TRest>) =
                write (isPrimitive p1) w p1 "item1" tuple.Item1
                write (isPrimitive p2) w p2 "item2" tuple.Item2
                write (isPrimitive p3) w p3 "item3" tuple.Item3
                write (isPrimitive p4) w p4 "item4" tuple.Item4
                write (isPrimitive p5) w p5 "item5" tuple.Item5
                write (isPrimitive p6) w p6 "item6" tuple.Item6
                write (isPrimitive p7) w p7 "item7" tuple.Item7
                write false w pr "rest" tuple.Rest

            let reader (r : Reader) =
                let t1 = read (isPrimitive p1) r p1 "item1"
                let t2 = read (isPrimitive p2) r p2 "item2"
                let t3 = read (isPrimitive p3) r p3 "item3"
                let t4 = read (isPrimitive p4) r p4 "item4"
                let t5 = read (isPrimitive p5) r p5 "item5"
                let t6 = read (isPrimitive p6) r p6 "item6"
                let t7 = read (isPrimitive p7) r p7 "item7"
                let rest = read false r pr "rest"
                new Tuple<_,_,_,_,_,_,_,_>(t1,t2,t3,t4,t5,t6,t7,rest)

#if OPTIMIZE_FSHARP
            // do not cache or apply subtype resolution for performance
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
#else
            new Pickler<_>(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = false)
#endif
            
        interface IGenericPicklerFactory8 with
            member __.Create<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest> (resolver : IPicklerResolver) =
                let p1 = resolver.Resolve<'T1> ()
                let p2 = resolver.Resolve<'T2> ()
                let p3 = resolver.Resolve<'T3> ()
                let p4 = resolver.Resolve<'T4> ()
                let p5 = resolver.Resolve<'T5> ()
                let p6 = resolver.Resolve<'T6> ()
                let p7 = resolver.Resolve<'T7> ()
                let pr = resolver.Resolve<'TRest> ()
                
                Tuple8Pickler.Create(p1, p2, p3, p4, p5, p6, p7, pr) :> Pickler


    let getTuplePicklerFactories () =
        [
            new Tuple1Pickler() :> IPicklerFactory
            new Tuple2Pickler() :> _
            new Tuple3Pickler() :> _
            new Tuple4Pickler() :> _
            new Tuple5Pickler() :> _
            new Tuple6Pickler() :> _
            new Tuple7Pickler() :> _
            new Tuple8Pickler() :> _
        ]   