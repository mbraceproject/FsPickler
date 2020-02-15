module internal MBrace.FsPickler.TypeShapeExtensions

open System
open TypeShape

// Fixed arity shapes deprecated from the main library

type ITuple1Visitor<'R> =
    abstract Visit<'T> : unit -> 'R

type IShapeTuple1 =
    abstract Accept : ITuple1Visitor<'R> -> 'R

type private ShapeTuple1<'T> () =
    interface IShapeTuple1 with
        member __.Accept v = v.Visit<'T> ()

// System.Tuple`2	

type ITuple2Visitor<'R> =
    abstract Visit<'T1, 'T2> : unit -> 'R

type IShapeTuple2 =
    abstract Accept : ITuple2Visitor<'R> -> 'R

type private ShapeTuple2<'T1, 'T2> () =
    interface IShapeTuple2 with
        member __.Accept v = v.Visit<'T1,'T2> ()

// System.Tuple`3	

type ITuple3Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3> : unit -> 'R

type IShapeTuple3 =
    abstract Accept : ITuple3Visitor<'R> -> 'R

type private ShapeTuple3<'T1, 'T2, 'T3> () =
    interface IShapeTuple3 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3> ()

// System.Tuple`4	

type ITuple4Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3, 'T4> : unit -> 'R

type IShapeTuple4 =
    abstract Accept : ITuple4Visitor<'R> -> 'R

type private ShapeTuple4<'T1, 'T2, 'T3, 'T4> () =
    interface IShapeTuple4 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3, 'T4> ()

// System.Tuple`5	

type ITuple5Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3, 'T4, 'T5> : unit -> 'R

type IShapeTuple5 =
    abstract Accept : ITuple5Visitor<'R> -> 'R

type private ShapeTuple5<'T1, 'T2, 'T3, 'T4, 'T5> () =
    interface IShapeTuple5 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3, 'T4, 'T5> ()

// System.Tuple`6	

type ITuple6Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> : unit -> 'R

type IShapeTuple6 =
    abstract Accept : ITuple6Visitor<'R> -> 'R

type private ShapeTuple6<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> () =
    interface IShapeTuple6 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> ()

// System.Tuple`7	

type ITuple7Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> : unit -> 'R

type IShapeTuple7 =
    abstract Accept : ITuple7Visitor<'R> -> 'R

type private ShapeTuple7<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> () =
    interface IShapeTuple7 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> ()

// System.Tuple`8	

type ITuple8Visitor<'R> =
    abstract Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest> : unit -> 'R

type IShapeTuple8 =
    abstract Accept : ITuple8Visitor<'R> -> 'R

type private ShapeTuple8<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest> () =
    interface IShapeTuple8 with
        member __.Accept v = v.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest> ()


// F# Choice types

type IFSharpChoice2Visitor<'R> =
    abstract Visit<'T1,'T2> : unit -> 'R

type IShapeFSharpChoice2 =
    abstract Accept : IFSharpChoice2Visitor<'R> -> 'R

type private ShapeFSharpChoice<'T1, 'T2> () =
    interface IShapeFSharpChoice2 with
        member __.Accept v = v.Visit<'T1,'T2>()

// F# Choice`3

type IFSharpChoice3Visitor<'R> =
    abstract Visit<'T1,'T2,'T3> : unit -> 'R

type IShapeFSharpChoice3 =
    abstract Accept : IFSharpChoice3Visitor<'R> -> 'R

type private ShapeFSharpChoice<'T1, 'T2, 'T3> () =
    interface IShapeFSharpChoice3 with
        member __.Accept v = v.Visit<'T1,'T2,'T3>()

// F# Choice`4

type IFSharpChoice4Visitor<'R> =
    abstract Visit<'T1,'T2,'T3,'T4> : unit -> 'R

type IShapeFSharpChoice4 =
    abstract Accept : IFSharpChoice4Visitor<'R> -> 'R

type private ShapeFSharpChoice<'T1, 'T2, 'T3, 'T4> () =
    interface IShapeFSharpChoice4 with
        member __.Accept v = v.Visit<'T1,'T2,'T3,'T4>()

// F# Choice`5

type IFSharpChoice5Visitor<'R> =
    abstract Visit<'T1,'T2,'T3,'T4,'T5> : unit -> 'R

type IShapeFSharpChoice5 =
    abstract Accept : IFSharpChoice5Visitor<'R> -> 'R

type private ShapeFSharpChoice<'T1, 'T2, 'T3, 'T4, 'T5> () =
    interface IShapeFSharpChoice5 with
        member __.Accept v = v.Visit<'T1,'T2,'T3,'T4,'T5>()

// F# Choice`6

type IFSharpChoice6Visitor<'R> =
    abstract Visit<'T1,'T2,'T3,'T4,'T5,'T6> : unit -> 'R

type IShapeFSharpChoice6 =
    abstract Accept : IFSharpChoice6Visitor<'R> -> 'R

type private ShapeFSharpChoice<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> () =
    interface IShapeFSharpChoice6 with
        member __.Accept v = v.Visit<'T1,'T2,'T3,'T4,'T5,'T6>()

// F# Choice`7

type IFSharpChoice7Visitor<'R> =
    abstract Visit<'T1,'T2,'T3,'T4,'T5,'T6,'T7> : unit -> 'R

type IShapeFSharpChoice7 =
    abstract Accept : IFSharpChoice7Visitor<'R> -> 'R

type private ShapeFSharpChoice<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> () =
    interface IShapeFSharpChoice7 with
        member __.Accept v = v.Visit<'T1,'T2,'T3,'T4,'T5,'T6,'T7>()

module Shape =

    /// Recognizes instances of System.Tuple<_>
    let (|Tuple1|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Tuple<_>> ->
            Activator.CreateInstanceGeneric<ShapeTuple1<_>>(ta)
            :?> IShapeTuple1
            |> Some
        | _ -> None

    /// Recognizes instances of System.Tuple<_,_>
    let (|Tuple2|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<_ * _> ->
            Activator.CreateInstanceGeneric<ShapeTuple2<_,_>>(ta)
            :?> IShapeTuple2
            |> Some
        | _ -> None

    /// Recognizes instances of System.Tuple<_,_,_>
    let (|Tuple3|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<_ * _ * _> ->
            Activator.CreateInstanceGeneric<ShapeTuple3<_,_,_>>(ta)
            :?> IShapeTuple3
            |> Some
        | _ -> None
        
    /// Recognizes instances of System.Tuple<_,_,_,_>
    let (|Tuple4|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<_ * _ * _ * _> ->
            Activator.CreateInstanceGeneric<ShapeTuple4<_,_,_,_>>(ta)
            :?> IShapeTuple4
            |> Some
        | _ -> None

    /// Recognizes instances of System.Tuple<_,_,_,_,_>
    let (|Tuple5|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<_ * _ * _ * _ * _> ->
            Activator.CreateInstanceGeneric<ShapeTuple5<_,_,_,_,_>>(ta)
            :?> IShapeTuple5
            |> Some
        | _ -> None

    /// Recognizes instances of System.Tuple<_,_,_,_,_,_>
    let (|Tuple6|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<_ * _ * _ * _ * _ * _> ->
            Activator.CreateInstanceGeneric<ShapeTuple6<_,_,_,_,_,_>>(ta)
            :?> IShapeTuple6
            |> Some
        | _ -> None

    /// Recognizes instances of System.Tuple<_,_,_,_,_,_,_>
    let (|Tuple7|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<_ * _ * _ * _ * _ * _ * _> ->
            Activator.CreateInstanceGeneric<ShapeTuple7<_,_,_,_,_,_,_>>(ta)
            :?> IShapeTuple7
            |> Some
        | _ -> None

    /// Recognizes instances of System.Tuple<_,_,_,_,_,_,_,_>
    let (|Tuple8|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Tuple<_,_,_,_,_,_,_,_>> ->
            Activator.CreateInstanceGeneric<ShapeTuple8<_,_,_,_,_,_,_,_>>(ta)
            :?> IShapeTuple8
            |> Some
        | _ -> None

    /// Recognizes shapes of F# Choice<_,_> types
    let (|FSharpChoice2|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Choice<_,_>> ->
            Activator.CreateInstanceGeneric<ShapeFSharpChoice<_,_>>(ta)
            :?> IShapeFSharpChoice2
            |> Some
        | _ -> None

    /// Recognizes shapes of F# Choice<_,_,_> types
    let (|FSharpChoice3|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Choice<_,_,_>> ->
            Activator.CreateInstanceGeneric<ShapeFSharpChoice<_,_,_>>(ta)
            :?> IShapeFSharpChoice3
            |> Some
        | _ -> None

    /// Recognizes shapes of F# Choice<_,_,_,_> types
    let (|FSharpChoice4|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Choice<_,_,_,_>> ->
            Activator.CreateInstanceGeneric<ShapeFSharpChoice<_,_,_,_>>(ta)
            :?> IShapeFSharpChoice4
            |> Some
        | _ -> None

    /// Recognizes shapes of F# Choice<_,_,_,_,_> types
    let (|FSharpChoice5|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Choice<_,_,_,_,_>> ->
            Activator.CreateInstanceGeneric<ShapeFSharpChoice<_,_,_,_,_>>(ta)
            :?> IShapeFSharpChoice5
            |> Some
        | _ -> None

    /// Recognizes shapes of F# Choice<_,_,_,_,_,_> types
    let (|FSharpChoice6|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Choice<_,_,_,_,_,_>> ->
            Activator.CreateInstanceGeneric<ShapeFSharpChoice<_,_,_,_,_,_>>(ta)
            :?> IShapeFSharpChoice6
            |> Some
        | _ -> None

    /// Recognizes shapes of F# Choice<_,_,_,_,_,_,_> types
    let (|FSharpChoice7|_|) (s : TypeShape) =
        match s.ShapeInfo with
        | Generic(td,ta) when td = typedefof<Choice<_,_,_,_,_,_,_>> ->
            Activator.CreateInstanceGeneric<ShapeFSharpChoice<_,_,_,_,_,_,_>>(ta)
            :?> IShapeFSharpChoice7
            |> Some
        | _ -> None