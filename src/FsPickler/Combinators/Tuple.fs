namespace MBrace.FsPickler

open System
open MBrace.FsPickler
#if TUPLE_WORKAROUND
open TypeShape
#endif

type internal TuplePickler =

    static member Create(p1 : Pickler<'T1>) =
        let writer (w : WriteState) (_ : string) (t : Tuple<'T1>) =
            p1.Write w "Item1" t.Item1

        let reader (r : ReadState) (_ : string) =
            let t1 = p1.Read r "Item1"
            new Tuple<'T1>(t1)

        let cloner (c : CloneState) (t : Tuple<'T1>) =
            let t' = p1.Clone c t.Item1
            new Tuple<'T1>(t')

        let accepter (v : VisitState) (t : Tuple<'T1>) =
            p1.Accept v t.Item1

        // do not cache by reference or apply subtype resolution for performance
        CompositePickler.Create<_>(reader, writer, cloner, accepter, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

    static member Create<'T1> (resolver : IPicklerResolver) =
        let p1 = resolver.Resolve<'T1> ()
        TuplePickler.Create p1


    static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>) =
        let writer (w : WriteState) (_ : string) ((t1,t2) : 'T1 * 'T2) =
            p1.Write w "Item1" t1
            p2.Write w "Item2" t2

        let reader (r : ReadState) (_ : string) =
            let t1 = p1.Read r "Item1"
            let t2 = p2.Read r "Item2"
            (t1, t2)

        let cloner (c : CloneState) (t1,t2) =
            let t1' = p1.Clone c t1
            let t2' = p2.Clone c t2
            (t1',t2')

        let accepter (v : VisitState) (t1,t2) =
            p1.Accept v t1
            p2.Accept v t2

        // do not apply subtype resolution for performance
        CompositePickler.Create<_>(reader, writer, cloner, accepter, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

    static member Create<'T1, 'T2> (resolver : IPicklerResolver) =
        let p1 = resolver.Resolve<'T1> ()
        let p2 = resolver.Resolve<'T2> ()
                
        TuplePickler.Create(p1, p2)

    static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>) =
        let writer (w : WriteState) (_ : string) ((t1,t2,t3) : 'T1 * 'T2 * 'T3) =
            p1.Write w "Item1" t1
            p2.Write w "Item2" t2
            p3.Write w "Item3" t3

        let reader (r : ReadState) (_ : string) =
            let t1 = p1.Read r "Item1"
            let t2 = p2.Read r "Item2"
            let t3 = p3.Read r "Item3"
            (t1, t2, t3)

        let cloner (c : CloneState) (t1,t2,t3) =
            let t1' = p1.Clone c t1
            let t2' = p2.Clone c t2
            let t3' = p3.Clone c t3
            (t1',t2',t3')

        let accepter (v : VisitState) (t1,t2,t3) =
            p1.Accept v t1
            p2.Accept v t2
            p3.Accept v t3

        // do not apply subtype resolution for performance
        CompositePickler.Create<_>(reader, writer, cloner, accepter, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

    static member Create<'T1, 'T2, 'T3> (resolver : IPicklerResolver) =
        let p1 = resolver.Resolve<'T1> ()
        let p2 = resolver.Resolve<'T2> ()
        let p3 = resolver.Resolve<'T3> ()
                
        TuplePickler.Create(p1, p2, p3)


    static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, p4 : Pickler<'T4>) =
        let writer (w : WriteState) (_ : string) ((t1,t2,t3,t4) : 'T1 * 'T2 * 'T3 * 'T4) =
            p1.Write w "Item1" t1
            p2.Write w "Item2" t2
            p3.Write w "Item3" t3
            p4.Write w "Item4" t4

        let reader (r : ReadState) (_ : string) =
            let t1 = p1.Read r "Item1"
            let t2 = p2.Read r "Item2"
            let t3 = p3.Read r "Item3"
            let t4 = p4.Read r "Item4"
            (t1, t2, t3, t4)

        let cloner (c : CloneState) (t1,t2,t3,t4) =
            let t1' = p1.Clone c t1
            let t2' = p2.Clone c t2
            let t3' = p3.Clone c t3
            let t4' = p4.Clone c t4
            (t1',t2',t3',t4')

        let accepter (v : VisitState) (t1,t2,t3,t4) =
            p1.Accept v t1
            p2.Accept v t2
            p3.Accept v t3
            p4.Accept v t4

        // do not apply subtype resolution for performance
        CompositePickler.Create<_>(reader, writer, cloner, accepter, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

    static member Create<'T1, 'T2, 'T3, 'T4> (resolver : IPicklerResolver) =
        let p1 = resolver.Resolve<'T1> ()
        let p2 = resolver.Resolve<'T2> ()
        let p3 = resolver.Resolve<'T3> ()
        let p4 = resolver.Resolve<'T4> ()
                
        TuplePickler.Create(p1, p2, p3, p4)


    static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, p4 : Pickler<'T4>, p5 : Pickler<'T5>) =
        let writer (w : WriteState) (_ : string) ((t1,t2,t3,t4,t5) : 'T1 * 'T2 * 'T3 * 'T4 * 'T5) =
            p1.Write w "Item1" t1
            p2.Write w "Item2" t2
            p3.Write w "Item3" t3
            p4.Write w "Item4" t4
            p5.Write w "Item5" t5

        let reader (r : ReadState) (_ : string) =
            let t1 = p1.Read r "Item1"
            let t2 = p2.Read r "Item2"
            let t3 = p3.Read r "Item3"
            let t4 = p4.Read r "Item4"
            let t5 = p5.Read r "Item5"
            (t1, t2, t3, t4, t5)

        let cloner (c : CloneState) (t1,t2,t3,t4,t5) =
            let t1' = p1.Clone c t1
            let t2' = p2.Clone c t2
            let t3' = p3.Clone c t3
            let t4' = p4.Clone c t4
            let t5' = p5.Clone c t5
            (t1',t2',t3',t4',t5')

        let accepter (v : VisitState) (t1,t2,t3,t4,t5) =
            p1.Accept v t1
            p2.Accept v t2
            p3.Accept v t3
            p4.Accept v t4
            p5.Accept v t5

        // do not apply subtype resolution for performance
        CompositePickler.Create<_>(reader, writer, cloner, accepter, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

    static member Create<'T1, 'T2, 'T3, 'T4, 'T5> (resolver : IPicklerResolver) =
        let p1 = resolver.Resolve<'T1> ()
        let p2 = resolver.Resolve<'T2> ()
        let p3 = resolver.Resolve<'T3> ()
        let p4 = resolver.Resolve<'T4> ()
        let p5 = resolver.Resolve<'T5> ()
                
        TuplePickler.Create(p1, p2, p3, p4, p5)


    static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, p4 : Pickler<'T4>, p5 : Pickler<'T5>, p6 : Pickler<'T6>) =
        let writer (w : WriteState) (_ : string) ((t1,t2,t3,t4,t5,t6) : 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6) =
            p1.Write w "Item1" t1
            p2.Write w "Item2" t2
            p3.Write w "Item3" t3
            p4.Write w "Item4" t4
            p5.Write w "Item5" t5
            p6.Write w "Item6" t6

        let reader (r : ReadState) (_ : string) =
            let t1 = p1.Read r "Item1"
            let t2 = p2.Read r "Item2"
            let t3 = p3.Read r "Item3"
            let t4 = p4.Read r "Item4"
            let t5 = p5.Read r "Item5"
            let t6 = p6.Read r "Item6"
            (t1, t2, t3, t4, t5, t6)

        let cloner (c : CloneState) (t1,t2,t3,t4,t5,t6) =
            let t1' = p1.Clone c t1
            let t2' = p2.Clone c t2
            let t3' = p3.Clone c t3
            let t4' = p4.Clone c t4
            let t5' = p5.Clone c t5
            let t6' = p6.Clone c t6
            (t1',t2',t3',t4',t5',t6')


        let accepter (v : VisitState) (t1,t2,t3,t4,t5,t6) =
            p1.Accept v t1
            p2.Accept v t2
            p3.Accept v t3
            p4.Accept v t4
            p5.Accept v t5
            p6.Accept v t6

        // do not apply subtype resolution for performance
        CompositePickler.Create<_>(reader, writer, cloner, accepter, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

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

        let writer (w : WriteState) (_ : string) ((t1,t2,t3,t4,t5,t6,t7) : 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7) =
            p1.Write w "Item1" t1
            p2.Write w "Item2" t2
            p3.Write w "Item3" t3
            p4.Write w "Item4" t4
            p5.Write w "Item5" t5
            p6.Write w "Item6" t6
            p7.Write w "Item7" t7

        let reader (r : ReadState) (_ : string) =
            let t1 = p1.Read r "Item1"
            let t2 = p2.Read r "Item2"
            let t3 = p3.Read r "Item3"
            let t4 = p4.Read r "Item4"
            let t5 = p5.Read r "Item5"
            let t6 = p6.Read r "Item6"
            let t7 = p7.Read r "Item7"
            (t1, t2, t3, t4, t5, t6, t7)

        let cloner (c : CloneState) (t1,t2,t3,t4,t5,t6,t7) =
            let t1' = p1.Clone c t1
            let t2' = p2.Clone c t2
            let t3' = p3.Clone c t3
            let t4' = p4.Clone c t4
            let t5' = p5.Clone c t5
            let t6' = p6.Clone c t6
            let t7' = p7.Clone c t7
            (t1',t2',t3',t4',t5',t6',t7')

        let accepter (v : VisitState) (t1,t2,t3,t4,t5,t6,t7) =
            p1.Accept v t1
            p2.Accept v t2
            p3.Accept v t3
            p4.Accept v t4
            p5.Accept v t5
            p6.Accept v t6
            p7.Accept v t7

        // do not apply subtype resolution for performance
        CompositePickler.Create<_>(reader, writer, cloner, accepter, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

    static member Create<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> (resolver : IPicklerResolver) =
        let p1 = resolver.Resolve<'T1> ()
        let p2 = resolver.Resolve<'T2> ()
        let p3 = resolver.Resolve<'T3> ()
        let p4 = resolver.Resolve<'T4> ()
        let p5 = resolver.Resolve<'T5> ()
        let p6 = resolver.Resolve<'T6> ()
        let p7 = resolver.Resolve<'T7> ()
                
        TuplePickler.Create(p1, p2, p3, p4, p5, p6, p7)

#if TUPLE_WORKAROUND
    // Workaround until https://github.com/Microsoft/visualfsharp/pull/3729 is fixed
    static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, p4 : Pickler<'T4>,
                            p5 : Pickler<'T5>, p6 : Pickler<'T6>, p7 : Pickler<'T7>, pr : Pickler<'TRest>) : Pickler =

        let tuple8 = System.Type.GetType "System.Tuple`8"
        let tupleTy = tuple8.MakeGenericType [|typeof<'T1> ; typeof<'T2> ; typeof<'T3> ; typeof<'T4> ; typeof<'T5> ; typeof<'T6> ; typeof<'T7> ; typeof<'TRest> |]
        let tupleShape = TypeShape.Create tupleTy
        let ctor = tupleTy.GetConstructors().[0]
        let getters = 
            tupleTy.GetProperties() 
            |> Array.map (fun p -> p.GetGetMethod()) 
            |> Array.sortBy (fun m -> m.Name)

        tupleShape.Accept {
            new ITypeShapeVisitor<Pickler> with
                member __.Visit<'Tuple8> () = 
                    let inline mkValue (t1 : 'T1) (t2 : 'T2) (t3 : 'T3) (t4 : 'T4) (t5 : 'T5) (t6 : 'T6) (t7 : 'T7) (tRest : 'TRest) =
                        ctor.Invoke [|t1 ; t2 ; t3 ; t4 ; t5 ; t6 ; t7 ; tRest |] :?> 'Tuple8

                    let inline getValue i (t : 'Tuple8) = getters.[i].Invoke(t, [||]) :?> 'a

                    let writer (w : WriteState) (_ : string) (tuple : 'Tuple8) =
                        p1.Write w "Item1" (getValue 0 tuple)
                        p2.Write w "Item2" (getValue 1 tuple)
                        p3.Write w "Item3" (getValue 2 tuple)
                        p4.Write w "Item4" (getValue 3 tuple)
                        p5.Write w "Item5" (getValue 4 tuple)
                        p6.Write w "Item6" (getValue 5 tuple)
                        p7.Write w "Item7" (getValue 6 tuple)
                        pr.Write w "Rest" (getValue 7 tuple)

                    let reader (r : ReadState) (_ : string) =
                        let t1 = p1.Read r "Item1"
                        let t2 = p2.Read r "Item2"
                        let t3 = p3.Read r "Item3"
                        let t4 = p4.Read r "Item4"
                        let t5 = p5.Read r "Item5"
                        let t6 = p6.Read r "Item6"
                        let t7 = p7.Read r "Item7"
                        let rest = pr.Read r "Rest"
                        mkValue t1 t2 t3 t4 t5 t6 t7 rest

                    let cloner (c : CloneState) (tuple : 'Tuple8) =
                        let t1' = p1.Clone c (getValue 0 tuple)
                        let t2' = p2.Clone c (getValue 1 tuple)
                        let t3' = p3.Clone c (getValue 2 tuple)
                        let t4' = p4.Clone c (getValue 3 tuple)
                        let t5' = p5.Clone c (getValue 4 tuple)
                        let t6' = p6.Clone c (getValue 5 tuple)
                        let t7' = p7.Clone c (getValue 6 tuple)
                        let rest' = pr.Clone c (getValue 7 tuple)
                        mkValue t1' t2' t3' t4' t5' t6' t7' rest'

                    let accepter (v : VisitState) (tuple : 'Tuple8) =
                        p1.Accept v (getValue 0 tuple)
                        p2.Accept v (getValue 1 tuple)
                        p3.Accept v (getValue 2 tuple)
                        p4.Accept v (getValue 3 tuple)
                        p5.Accept v (getValue 4 tuple)
                        p6.Accept v (getValue 5 tuple)
                        p7.Accept v (getValue 6 tuple)
                        pr.Accept v (getValue 7 tuple)

                    // do not apply subtype resolution for performance
                    let pickler = CompositePickler.Create<'Tuple8>(reader, writer, cloner, accepter, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)
                    pickler :> Pickler }

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

#else
    static member Create(p1 : Pickler<'T1>, p2 : Pickler<'T2>, p3 : Pickler<'T3>, p4 : Pickler<'T4>,
                            p5 : Pickler<'T5>, p6 : Pickler<'T6>, p7 : Pickler<'T7>, pr : Pickler<'TRest>) : Pickler<System.Tuple<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'TRest>> =

        let writer (w : WriteState) (_ : string) (tuple : ('T1 *'T2*'T3*'T4*'T5*'T6*'T7*'TRest)) =
            let (Item1,
                 Item2,
                 Item3,
                 Item4,
                 Item5,
                 Item6,
                 Item7,
                 Rest)  = tuple

            p1.Write w "Item1" Item1
            p2.Write w "Item2" Item2
            p3.Write w "Item3" Item3
            p4.Write w "Item4" Item4
            p5.Write w "Item5" Item5
            p6.Write w "Item6" Item6
            p7.Write w "Item7" Item7
            pr.Write w "Rest"  Rest

        let reader (r : ReadState) (_ : string) =
            let t1 = p1.Read r "Item1"
            let t2 = p2.Read r "Item2"
            let t3 = p3.Read r "Item3"
            let t4 = p4.Read r "Item4"
            let t5 = p5.Read r "Item5"
            let t6 = p6.Read r "Item6"
            let t7 = p7.Read r "Item7"
            let rest = pr.Read r "Rest"
            new Tuple<_,_,_,_,_,_,_,_>(t1,t2,t3,t4,t5,t6,t7,rest)

        let cloner (c : CloneState) (tuple : ('T1*'T2*'T3*'T4*'T5*'T6*'T7*'TRest)) =
            let (Item1,
                 Item2,
                 Item3,
                 Item4,
                 Item5,
                 Item6,
                 Item7,
                 Rest)  = tuple
            
            let t1' = p1.Clone c   Item1
            let t2' = p2.Clone c   Item2
            let t3' = p3.Clone c   Item3
            let t4' = p4.Clone c   Item4
            let t5' = p5.Clone c   Item5
            let t6' = p6.Clone c   Item6
            let t7' = p7.Clone c   Item7
            let rest' = pr.Clone c Rest
            t1',t2',t3',t4',t5',t6',t7',rest'

        let accepter (v : VisitState) (tuple : ('T1*'T2*'T3*'T4*'T5*'T6*'T7*'TRest)) =
            let (Item1,
                 Item2,
                 Item3,
                 Item4,
                 Item5,
                 Item6,
                 Item7,
                 Rest)  = tuple
            p1.Accept v Item1
            p2.Accept v Item2
            p3.Accept v Item3
            p4.Accept v Item4
            p5.Accept v Item5
            p6.Accept v Item6
            p7.Accept v Item7
            pr.Accept v Rest

        // do not apply subtype resolution for performance
        CompositePickler.Create<_>(reader, writer, cloner, accepter, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

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
#endif