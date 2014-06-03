namespace Nessos.FsPickler.TypeShape

    open System
    open System.Runtime.Serialization

    type Shape =
        | Abstract = 0
        | Struct = 1
        | Primitive = 2
        | Class = 3
        | ISerializable = 4
        | Delegate = 5
        | Enum = 6
        | Nullable = 7
        | Array = 8
        | Tuple = 9
        | FSharpType = 10
        | Dictionary = 11

    [<AbstractClass>]
    type TypeShape internal () =
        abstract Type : Type
        abstract Shape : Shape
        abstract Accept : ITypeVisitor<'R> -> 'R
        abstract Accept : ITypeShapeVisitor<'R> -> 'R

    and [<AbstractClass>] TypeShape<'T> internal () =
        inherit TypeShape()
        override __.Type = typeof<'T>
        override __.Accept (v : ITypeVisitor<'R>) = v.Visit<'T>()

    and ShapeAbstract<'T> internal () =
        inherit TypeShape<'T> ()
        override __.Shape = Shape.Abstract
        override __.Accept(v : ITypeShapeVisitor<'R>) = v.Abstract<'T> ()

    and ShapeStruct<'T when 'T : struct> () =
        inherit TypeShape<'T>()
        override __.Shape = Shape.Struct
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Struct<'T> ()

    and ShapePrimitive<'T> internal () =
        inherit TypeShape<'T> ()
        override __.Shape = Shape.Primitive
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Primitive<'T> ()

    and ShapeClass<'T when 'T : not struct> internal () =
        inherit TypeShape<'T>()
        override __.Shape = Shape.Class
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Class<'T> ()

    and ShapeISerializable<'T when 'T :> ISerializable> () =
        inherit TypeShape<'T>()
        override __.Shape = Shape.ISerializable
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.ISerializable<'T> ()

    and ShapeDelegate<'T when 'T :> Delegate> () =
        inherit TypeShape<'T>()
        override __.Shape = Shape.Delegate
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Delegate<'T> ()

    and ShapeEnum<'Enum, 'Underlying when 'Enum : enum<'Underlying>> () =
        inherit TypeShape<'Enum>()
        override __.Shape = Shape.Enum
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Enum<'Enum, 'Underlying> ()

    and ShapeNullable<'T when    
                    'T : (new : unit -> 'T) and 
                    'T : struct and 
                    'T :> ValueType> () =

        inherit TypeShape<Nullable<'T>>()
        override __.Shape = Shape.Nullable
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Nullable<'T> ()

    and ShapeArray<'T> () =
        inherit TypeShape<'T []>()
        override __.Shape = Shape.Array
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Array<'T> ()

    and ShapeArray2D<'T> () =
        inherit TypeShape<'T [,]>()
        override __.Shape = Shape.Array
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Array2D<'T> ()

    and ShapeArray3D<'T> () =
        inherit TypeShape<'T [,,]>()
        override __.Shape = Shape.Array
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Array3D<'T> ()

    and ShapeArray4D<'T> () =
        inherit TypeShape<'T [,,,]>()
        override __.Shape = Shape.Array
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Array4D<'T> ()

    and ShapeTuple<'T> () =
        inherit TypeShape<Tuple<'T>> ()
        override __.Shape = Shape.Array
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Tuple<'T> ()

    and ShapeTuple<'T1,'T2> () =
        inherit TypeShape<'T1 * 'T2> ()
        override __.Shape = Shape.Tuple
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Tuple<'T1,'T2> ()

    and ShapeTuple<'T1,'T2,'T3> () =
        inherit TypeShape<'T1 * 'T2 * 'T3> ()
        override __.Shape = Shape.Tuple
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Tuple<'T1,'T2,'T3> ()

    and ShapeTuple<'T1,'T2,'T3,'T4> () =
        inherit TypeShape<'T1 * 'T2 * 'T3 * 'T4> ()
        override __.Shape = Shape.Tuple
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Tuple<'T1,'T2,'T3,'T4> ()

    and ShapeTuple<'T1,'T2,'T3,'T4,'T5> () =
        inherit TypeShape<'T1 * 'T2 * 'T3 * 'T4 * 'T5> ()
        override __.Shape = Shape.Tuple
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Tuple<'T1,'T2,'T3,'T4,'T5> ()

    and ShapeTuple<'T1,'T2,'T3,'T4,'T5,'T6> () =
        inherit TypeShape<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6> ()
        override __.Shape = Shape.Tuple
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Tuple<'T1,'T2,'T3,'T4,'T5,'T6> ()

    and ShapeTuple<'T1,'T2,'T3,'T4,'T5,'T6,'T7> () =
        inherit TypeShape<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7> ()
        override __.Shape = Shape.Tuple
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Tuple<'T1,'T2,'T3,'T4,'T5,'T6,'T7> ()

    and ShapeTuple<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'TRest> () =
        inherit TypeShape<Tuple<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'TRest>> ()
        override __.Shape = Shape.Tuple
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Tuple<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'TRest> ()

    and ShapeDictionary<'K,'V when 'K : equality> () =
        inherit TypeShape<System.Collections.Generic.Dictionary<'K,'V>> ()
        override __.Shape = Shape.Dictionary
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Dictionary<'K,'V> ()

    and ShapeFSharpUnion<'Union> internal () =
        inherit TypeShape<'Union> ()
        override __.Shape = Shape.FSharpType
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.FSharpUnion<'Union> ()

    and ShapeFSharpRecord<'Record> internal () =
        inherit TypeShape<'Record> ()
        override __.Shape = Shape.FSharpType
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.FSharpRecord<'Record> ()

    and ShapeFSharpException<'Exception when 'Exception :> exn> internal () =
        inherit TypeShape<'Exception> ()
        override __.Shape = Shape.FSharpType
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.FSharpException<'Exception> ()

    and ShapeFSharpList<'T> () =
        inherit TypeShape<'T list>()
        override __.Shape = Shape.FSharpType
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.FSharpList<'T> ()

    and ShapeFSharpSet<'T when 'T : comparison> () =
        inherit TypeShape<Set<'T>> ()
        override __.Shape = Shape.FSharpType
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.FSharpSet<'T> ()

    and ShapeFSharpMap<'K,'V when 'K : comparison> () =
        inherit TypeShape<Map<'K,'V>> ()
        override __.Shape = Shape.FSharpType
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.FSharpMap<'K,'V> ()

    and ShapeFSharpOption<'T> () =
        inherit TypeShape<'T option> ()
        override __.Shape = Shape.FSharpType
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.FSharpOption<'T> ()

    and ShapeFSharpRef<'T> () =
        inherit TypeShape<'T ref> ()
        override __.Shape = Shape.FSharpType
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.FSharpRef<'T> ()

    and ShapeChoice<'T1,'T2> () =
        inherit TypeShape<Choice<'T1,'T2>> ()
        override __.Shape = Shape.FSharpType
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Choice<'T1,'T2> ()

    and ShapeChoice<'T1,'T2,'T3> () =
        inherit TypeShape<Choice<'T1,'T2, 'T3>> ()
        override __.Shape = Shape.FSharpType
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Choice<'T1,'T2,'T3> ()

    and ShapeChoice<'T1,'T2,'T3,'T4> () =
        inherit TypeShape<Choice<'T1,'T2,'T3,'T4>> ()
        override __.Shape = Shape.FSharpType
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Choice<'T1,'T2,'T3,'T4> ()

    and ShapeChoice<'T1,'T2,'T3,'T4,'T5> () =
        inherit TypeShape<Choice<'T1,'T2,'T3,'T4,'T5>> ()
        override __.Shape = Shape.FSharpType
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Choice<'T1,'T2,'T3,'T4,'T5> ()

    and ShapeChoice<'T1,'T2,'T3,'T4,'T5,'T6> () =
        inherit TypeShape<Choice<'T1,'T2,'T3,'T4,'T5,'T6>> ()
        override __.Shape = Shape.FSharpType
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Choice<'T1,'T2,'T3,'T4,'T5,'T6> ()

    and ShapeChoice<'T1,'T2,'T3,'T4,'T5,'T6,'T7> () =
        inherit TypeShape<Choice<'T1,'T2,'T3,'T4,'T5,'T6,'T7>> ()
        override __.Shape = Shape.FSharpType
        override __.Accept (v : ITypeShapeVisitor<'R>) = v.Choice<'T1,'T2,'T3,'T4,'T5,'T6,'T7> ()

    and ITypeVisitor<'R> =
        abstract Visit<'T> : unit -> 'R

    and ITypeShapeVisitor<'R> =
        abstract Primitive<'T> : unit -> 'R
        abstract Struct<'T when 'T : struct> : unit -> 'R
        abstract Abstract<'T> : unit -> 'R
        abstract Class<'T when 'T : not struct> : unit -> 'R
        abstract ISerializable<'T when 'T :> ISerializable> : unit -> 'R
        abstract Delegate<'D when 'D :> Delegate> : unit -> 'R
        abstract Enum<'Enum, 'Underlying when 'Enum : enum<'Underlying>> : unit -> 'R

        abstract Nullable<'T when   
                                'T : (new : unit -> 'T) and 
                                'T : struct and 
                                'T :> ValueType> : unit -> 'R

        abstract Array<'T> : unit -> 'R
        abstract Array2D<'T> : unit -> 'R
        abstract Array3D<'T> : unit -> 'R
        abstract Array4D<'T> : unit -> 'R

        abstract Tuple<'T> : unit -> 'R
        abstract Tuple<'T1,'T2> : unit -> 'R
        abstract Tuple<'T1,'T2,'T3> : unit -> 'R
        abstract Tuple<'T1,'T2,'T3,'T4> : unit -> 'R
        abstract Tuple<'T1,'T2,'T3,'T4,'T5> : unit -> 'R
        abstract Tuple<'T1,'T2,'T3,'T4,'T5,'T6> : unit -> 'R
        abstract Tuple<'T1,'T2,'T3,'T4,'T5,'T6,'T7> : unit -> 'R
        abstract Tuple<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'TRest> : unit -> 'R

        abstract Dictionary<'K, 'V when 'K : equality> : unit -> 'R
        
        abstract FSharpUnion<'Union> : unit -> 'R
        abstract FSharpRecord<'Record> : unit -> 'R
        abstract FSharpException<'Exception when 'Exception :> exn> : unit -> 'R
        abstract FSharpList<'T> : unit -> 'R
        abstract FSharpRef<'T> : unit -> 'R
        abstract FSharpSet<'T when 'T : comparison> : unit -> 'R
        abstract FSharpMap<'K,'V when 'K : comparison> : unit -> 'R
        abstract FSharpOption<'T> : unit -> 'R
        
        abstract Choice<'T1,'T2> : unit -> 'R
        abstract Choice<'T1,'T2,'T3> : unit -> 'R
        abstract Choice<'T1,'T2,'T3,'T4> : unit -> 'R
        abstract Choice<'T1,'T2,'T3,'T4,'T5> : unit -> 'R
        abstract Choice<'T1,'T2,'T3,'T4,'T5,'T6> : unit -> 'R
        abstract Choice<'T1,'T2,'T3,'T4,'T5,'T6,'T7> : unit -> 'R


    exception UnSupportedShape

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module TypeShape =

        open System.Reflection

        open Microsoft.FSharp.Reflection

        open Nessos.FsPickler.Reflection

        // typedefof does not work properly with 'enum' constraints
        let private getGenericEnumType () = 
            typeof<ShapeEnum<BindingFlags,int>>.GetGenericTypeDefinition()

        let private activate (gt : Type) (tp : Type []) =
            let ti = gt.MakeGenericType tp
            let ctor = ti.GetConstructor(allConstructors, null, CallingConventions.Standard, [||], [||])
            ctor.Invoke [||] :?> TypeShape

        let private activate1 (gt : Type) (tp : Type) =
            activate gt [|tp|]

        let private activate2 (gt : Type) (p1 : Type) (p2 : Type) =
            activate gt [|p1 ; p2|]

        let private (|List|Option|Choice|Union|) (t : Type) =
            if t.IsGenericType then
                let gt = t.GetGenericTypeDefinition()
                let gas = t.GetGenericArguments()
                if gt = typedefof<_ list> then List (gas.[0])
                elif gt = typedefof<_ option> then Option (gas.[0])
                elif 
                    gt.Name.StartsWith "FSharpChoice" && 
                    gt.Namespace = "Microsoft.FSharp.Core" && 
                    gt.Assembly = typeof<int option>.Assembly then

                    Choice gas

                else
                    Union
            else
                Union

        let private (|Dictionary|FSharpMap|FSharpSet|NotACollection|) (t : Type) =
            if t.IsGenericType then
                let gt = t.GetGenericTypeDefinition()
                let gas = t.GetGenericArguments()
                if gt = typedefof<System.Collections.Generic.Dictionary<_,_>> then
                    Dictionary(gas.[0], gas.[1])
                elif gt = typedefof<Map<_,_>> then
                    FSharpMap(gas.[0], gas.[1])
                elif gt = typedefof<Set<_>> then
                    FSharpSet(gas.[0])
                else
                    NotACollection
            else
                NotACollection

        let private canon = Type.GetType("System.__Canon")
        let private isIntrinsicType (t : Type) =
            t.IsPointer 
            || t = typeof<System.Reflection.Pointer>
            || t.IsByRef
            || t.IsCOMObject
            || t.IsImport
            || t.IsMarshalByRef
            || t = canon
        
        /// use reflection to bootstrap a shape instance
        let resolve (t : Type) =
            if t.IsGenericTypeDefinition then raise <| UnSupportedShape
            elif t.IsGenericParameter then raise <| UnSupportedShape
            elif isIntrinsicType t then raise <| UnSupportedShape
            elif t.IsPrimitive then activate1 typedefof<ShapePrimitive<_>> t
            elif FSharpType.IsTuple t then
                let gas = t.GetGenericArguments()
                match gas.Length with
                | 1 -> activate typedefof<ShapeTuple<_>> gas
                | 2 -> activate typedefof<ShapeTuple<_,_>> gas
                | 3 -> activate typedefof<ShapeTuple<_,_,_>> gas
                | 4 -> activate typedefof<ShapeTuple<_,_,_,_>> gas
                | 5 -> activate typedefof<ShapeTuple<_,_,_,_,_>> gas
                | 6 -> activate typedefof<ShapeTuple<_,_,_,_,_,_>> gas
                | 7 -> activate typedefof<ShapeTuple<_,_,_,_,_,_,_>> gas
                | 8 -> activate typedefof<ShapeTuple<_,_,_,_,_,_,_,_>> gas
                | _ -> invalidOp "invalid tuple type"

            elif FSharpType.IsUnion(t, allMembers) then
                match t with
                | List et -> activate1 typedefof<ShapeFSharpList<_>> et
                | Option et -> activate1 typedefof<ShapeFSharpOption<_>> et
                | Union -> activate1 typedefof<ShapeFSharpUnion<_>> t
                | Choice args -> 
                    match args.Length with
                    | 2 -> activate typedefof<ShapeChoice<_,_>> args
                    | 3 -> activate typedefof<ShapeChoice<_,_,_>> args
                    | 4 -> activate typedefof<ShapeChoice<_,_,_,_>> args
                    | 5 -> activate typedefof<ShapeChoice<_,_,_,_,_>> args
                    | 6 -> activate typedefof<ShapeChoice<_,_,_,_,_,_>> args
                    | 7 -> activate typedefof<ShapeChoice<_,_,_,_,_,_,_>> args
                    | _ -> invalidOp "invalid parameter size"

            elif FSharpType.IsRecord(t, allMembers) then
                if t.IsGenericType && t.GetGenericTypeDefinition () = typedefof<_ ref> then
                    let et = t.GetGenericArguments().[0]
                    activate1 typedefof<ShapeFSharpRef<_>> et
                else
                    activate1 typedefof<ShapeFSharpRecord<_>> t

            elif FSharpType.IsExceptionRepresentation(t, allMembers) then
                activate1 typedefof<ShapeFSharpException<_>> t

            else
                match t with
                | Dictionary(k,v) -> activate2 typedefof<ShapeDictionary<_,_>> k v
                | FSharpMap(k,v) -> activate2 typedefof<ShapeFSharpMap<_,_>> k v
                | FSharpSet k -> activate1 typedefof<ShapeFSharpSet<_>> k
                | NotACollection ->

                if t.IsAbstract then activate1 typedefof<ShapeAbstract<_>> t
                elif t.IsEnum then activate2 (getGenericEnumType()) t <| t.GetEnumUnderlyingType()
                elif isNullableType t then
                    let et = t.GetGenericArguments().[0]
                    activate1 typedefof<ShapeNullable<_>> et

                elif t.IsValueType then activate1 typedefof<ShapeStruct<_>> t
                elif t.IsArray then
                    let et = t.GetElementType()
                    match t.GetArrayRank() with
                    | 1 -> activate1 typedefof<ShapeArray<_>> et
                    | 2 -> activate1 typedefof<ShapeArray2D<_>> et
                    | 3 -> activate1 typedefof<ShapeArray3D<_>> et
                    | 4 -> activate1 typedefof<ShapeArray4D<_>> et
                    | rk -> invalidOp <| sprintf "unsupported array rank '%d'" rk

                elif typeof<Delegate>.IsAssignableFrom t then
                    activate1 typedefof<ShapeDelegate<_>> t
                elif isISerializable t then
                    activate1 typedefof<ShapeISerializable<_>> t
                else
                    activate1 typedefof<ShapeClass<_>> t