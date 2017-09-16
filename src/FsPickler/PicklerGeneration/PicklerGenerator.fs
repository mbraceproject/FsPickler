module internal MBrace.FsPickler.PicklerGenerator

//
//  Defines a type shape visitor that routes shapes into their
//  corresponding pickler combinator implementations
//

open System
open System.Reflection
open System.Runtime.Serialization

open TypeShape

open MBrace.FsPickler
open MBrace.FsPickler.Reflection
open MBrace.FsPickler.PrimitivePicklers
open MBrace.FsPickler.ReflectionPicklers

/// Implements a pickler factory type visitor

type PicklerGenerator =

    static member ExtractShape(t : Type) =
        try TypeShape.Create t
        with UnsupportedShape t -> raise <| NonSerializableTypeException(t)
        
    /// Constructs a pickler for a given shape
    static member Create (resolver : IPicklerResolver) (shape : TypeShape) : Pickler =
        let isUnsupportedType (t:Type) =
            t.IsPointer 
            || t = typeof<System.Reflection.Pointer>
            || t.IsByRef
            || t.IsCOMObject
            || t.IsImport
            || t.IsMarshalByRef

        match shape with
        | _ when isUnsupportedType shape.Type -> raise <| NonSerializableTypeException shape.Type
        | Shape.Bool -> new BooleanPickler() :> _
        | Shape.Byte -> new BytePickler() :> _
        | Shape.SByte -> new SBytePickler() :> _
        | Shape.Int16 -> new Int16Pickler() :> _
        | Shape.Int32 -> new Int32Pickler() :> _
        | Shape.Int64 -> new Int64Pickler() :> _
        | Shape.UInt16 -> new UInt16Pickler() :> _
        | Shape.UInt32 -> new UInt32Pickler() :> _
        | Shape.UInt64 -> new UInt64Pickler() :> _
        | Shape.Single -> new SinglePickler() :> _
        | Shape.Double -> new DoublePickler() :> _
        | Shape.Decimal -> new DecimalPickler() :> _
        | Shape.Char -> new CharPickler() :> _
        | Shape.String -> new StringPickler() :> _
        | Shape.Guid -> new GuidPickler() :> _
        | Shape.DateTime -> new DateTimePickler() :> _
        | Shape.DateTimeOffset -> new DateTimeOffsetPickler() :> _
        | Shape.TimeSpan -> new TimeSpanPickler() :> _
        | Shape.ByteArray -> ArrayPickler.CreateByteArrayPickler() :> _
        | Shape.Unit -> new UnitPickler() :> _
        | :? TypeShape<bigint> -> new BigIntPickler() :> _
        | :? TypeShape<System.Object> -> CompositePickler.ObjectPickler :> _
        | :? TypeShape<AssemblyInfo> -> ReflectionPicklers.CreateAssemblyInfoPickler() :> _
        | :? TypeShape<AssemblyName> -> ReflectionPicklers.CreateAssemblyNamePickler resolver :> _
        | :? TypeShape<Assembly> -> ReflectionPicklers.CreateAssemblyPickler resolver :> _
        | :? TypeShape<MemberInfo> -> ReflectionPicklers.CreateMemberInfoPickler resolver :> _
        | :? TypeShape<System.DBNull> -> new DBNullPickler() :> _
        | _ when PicklerPluginRegistry.ContainsFactory shape.Type ->
            shape.Accept {
                new ITypeShapeVisitor<Pickler> with
                    member __.Visit<'T> () = PicklerPluginRegistry.GetPicklerFactory<'T>() resolver :> Pickler 
            }

        | Shape.Nullable s ->
            s.Accept {
                new INullableVisitor<Pickler> with
                    member __.Visit<'T when 'T : struct and 'T :> ValueType and 'T : (new : unit -> 'T)> () =
                        NullablePickler.Create<'T>(resolver) :> _
            }

        | Shape.Array s ->
            s.Accept {
                new IArrayVisitor<Pickler> with
                    member __.Visit<'T> rank = 
                        match rank with
                        | 1 -> ArrayPickler.Create<'T>(resolver) :> _
                        | 2 -> ArrayPickler.Create2D<'T>(resolver) :> _
                        | 3 -> ArrayPickler.Create3D<'T>(resolver) :> _
                        | 4 -> ArrayPickler.Create4D<'T>(resolver) :> _
                        | _ -> raise <| NonSerializableTypeException(shape.Type, "Array ranks more than 4 are not supported.")
            }

        | Shape.Tuple1 s ->
            s.Accept {
                new ITuple1Visitor<Pickler> with
                    member __.Visit<'T1> () = TuplePickler.Create<'T1>(resolver) :> _
            }

        | Shape.Tuple2 s ->
            s.Accept {
                new ITuple2Visitor<Pickler> with
                    member __.Visit<'T1, 'T2> () = TuplePickler.Create<'T1, 'T2>(resolver) :> _
            }

        | Shape.Tuple3 s ->
            s.Accept {
                new ITuple3Visitor<Pickler> with
                    member __.Visit<'T1, 'T2, 'T3> () = TuplePickler.Create<'T1, 'T2, 'T3>(resolver) :> _
            }

        | Shape.Tuple4 s ->
            s.Accept {
                new ITuple4Visitor<Pickler> with
                    member __.Visit<'T1, 'T2, 'T3, 'T4> () = TuplePickler.Create<'T1, 'T2, 'T3, 'T4>(resolver) :> _
            }

        | Shape.Tuple5 s ->
            s.Accept {
                new ITuple5Visitor<Pickler> with
                    member __.Visit<'T1, 'T2, 'T3, 'T4, 'T5> () = TuplePickler.Create<'T1, 'T2, 'T3, 'T4, 'T5>(resolver) :> _
            }

        | Shape.Tuple6 s ->
            s.Accept {
                new ITuple6Visitor<Pickler> with
                    member __.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> () = TuplePickler.Create<'T1, 'T2, 'T3, 'T4, 'T5, 'T6>(resolver) :> _
            }

        | Shape.Tuple7 s ->
            s.Accept {
                new ITuple7Visitor<Pickler> with
                    member __.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> () = TuplePickler.Create<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7>(resolver) :> _
            }

        | Shape.Tuple8 s ->
            s.Accept {
                new ITuple8Visitor<Pickler> with
                    member __.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest> () = TuplePickler.Create<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest>(resolver) :> _
            }

        | Shape.Dictionary s ->
            s.Accept {
                new IDictionaryVisitor<Pickler> with
                    member __.Visit<'K,'V when 'K : equality>() = DictionaryPickler.Create<'K,'V>(resolver) :> _
            }

        | Shape.FSharpList s ->
            s.Accept {
                new IFSharpListVisitor<Pickler> with
                    member __.Visit<'T> () = ListPickler.Create<'T>(resolver) :> _
            }

        | Shape.FSharpOption s ->
            s.Accept {
                new IFSharpOptionVisitor<Pickler> with
                    member __.Visit<'T> () = OptionPickler.Create<'T>(resolver) :> _
            }

        | Shape.FSharpRef s ->
            s.Accept {
                new IFSharpRefVisitor<Pickler> with
                    member __.Visit<'T> () = FSharpRefPickler.Create<'T>(resolver) :> _
            }

        | Shape.FSharpSet s ->
            s.Accept {
                new IFSharpSetVisitor<Pickler> with
                    member __.Visit<'T when 'T : comparison> () = FSharpSetPickler.Create<'T>(resolver) :> _
            }

        | Shape.FSharpMap s ->
            s.Accept {
                new IFSharpMapVisitor<Pickler> with
                    member __.Visit<'K,'V when 'K : comparison> () = FSharpMapPickler.Create<'K,'V>(resolver) :> _
            }

        | Shape.FSharpChoice2 s ->
            s.Accept {
                new IFSharpChoice2Visitor<Pickler> with
                    member __.Visit<'T1,'T2>() = ChoicePickler.Create<'T1,'T2>(resolver) :> _
            }

        | Shape.FSharpChoice3 s ->
            s.Accept {
                new IFSharpChoice3Visitor<Pickler> with
                    member __.Visit<'T1,'T2,'T3> () = ChoicePickler.Create<'T1,'T2,'T3>(resolver) :> _
            }

        | Shape.FSharpChoice4 s ->
            s.Accept {
                new IFSharpChoice4Visitor<Pickler> with
                    member __.Visit<'T1,'T2,'T3,'T4> () = ChoicePickler.Create<'T1,'T2,'T3,'T4>(resolver) :> _
            }

        | Shape.FSharpChoice5 s->
            s.Accept {
                new IFSharpChoice5Visitor<Pickler> with
                    member __.Visit<'T1,'T2,'T3,'T4,'T5> () = ChoicePickler.Create<'T1,'T2,'T3,'T4,'T5>(resolver) :> _
            }

        | Shape.FSharpChoice6 s ->
            s.Accept {
                new IFSharpChoice6Visitor<Pickler> with
                    member __.Visit<'T1,'T2,'T3,'T4,'T5,'T6> () = ChoicePickler.Create<'T1,'T2,'T3,'T4,'T5,'T6>(resolver) :> _
            }

        | Shape.FSharpChoice7 s ->
            s.Accept {
                new IFSharpChoice7Visitor<Pickler> with
                    member __.Visit<'T1,'T2,'T3,'T4,'T5,'T6,'T7> () = ChoicePickler.Create<'T1,'T2,'T3,'T4,'T5,'T6,'T7>(resolver) :> _
            }

        //////////////////////////////////
        // User-defined types
        //////////////////////////////////

        | _ when containsAttr<CloneableOnlyAttribute> shape.Type ->
            shape.Accept {
                new ITypeShapeVisitor<Pickler> with
                    member __.Visit<'T> () = CloneableOnlyPickler.Create<'T> () :> Pickler
            }

        | _ when containsAttr<CustomPicklerAttribute> shape.Type ->
            shape.Accept {
                new ITypeShapeVisitor<Pickler> with
                    member __.Visit<'T> () = CustomPickler.Create<'T> resolver :> Pickler
            }

        | Shape.Enum s -> 
            // NB: DataContractAttribute for enum types handled by enum pickler
            s.Accept { 
                new IEnumVisitor<Pickler> with
                    member __.Visit<'E, 'U when 'E : enum<'U> 
                                            and 'E : struct 
                                            and 'E :> ValueType
                                            and 'E : (new : unit -> 'E)> () = 

                        EnumPickler.Create<'E, 'U>(resolver) :> _
            }

        | _ when containsAttr<DataContractAttribute> shape.Type ->
            shape.Accept {
                new ITypeShapeVisitor<Pickler> with
                    member __.Visit<'T> () = DataContractPickler.Create<'T> resolver :> Pickler
            }

        | Shape.Delegate s ->
            s.Accept {
                new IDelegateVisitor<Pickler> with
                    member __.Visit<'D when 'D :> Delegate> () =
                        DelegatePickler.Create<'D>(resolver) :> _
            }

        | Shape.FSharpUnion _ as s ->
            s.Accept {
                new ITypeShapeVisitor<Pickler> with
                    member __.Visit<'U> () = FsUnionPickler.Create<'U>(resolver) :> _
            }

        | Shape.FSharpRecord _ as s ->
            s.Accept {
                new ITypeShapeVisitor<Pickler> with
                    member __.Visit<'R> () = FsRecordPickler.Create<'R>(resolver) :> _
            }

        | shape when shape.Type.IsAbstract ->
            shape.Accept {
                new ITypeShapeVisitor<Pickler> with
                    member __.Visit<'T>() = AbstractPickler.Create<'T> () :> Pickler
            }

        | Shape.Exception s ->
            s.Accept {
                new IExceptionVisitor<Pickler> with
                    member __.Visit<'exn when 'exn :> exn>() = 
                        if s.IsFSharpException then FsExceptionPickler.Create<'exn>(resolver) :> _
                        else
                            match tryGetISerializableCtor typeof<'exn> with
                            | None -> ISerializablePickler.CreateNonISerializableExceptionPickler<'exn>(resolver) :> _
                            | Some _ -> ISerializablePickler.Create<'exn>() :> _
            }

        | Shape.ISerializable s ->
            s.Accept {
                new ISerializableVisitor<Pickler> with
                    member __.Visit<'T when 'T :> ISerializable> (ss:ShapeISerializable<'T>): Pickler =
                        match tryGetISerializableCtor typeof<'T> with
                        | Some _ -> ISerializablePickler.Create<'T>() :> _
                        | None -> ISerializablePickler.CreateObjectReferencePickler<'T>() :> _
            }

        | shape when shape.Type.IsValueType ->
            shape.Accept {
                new ITypeShapeVisitor<Pickler> with
                    member __.Visit<'T>() = StructFieldPickler.Create<'T>(resolver) :> Pickler
            }

        | _ ->
            shape.Accept {
                new ITypeShapeVisitor<Pickler> with
                    member __.Visit<'T>() = ClassFieldPickler.Create<'T>(resolver) :> Pickler
            }

    /// Constructs a blank, uninitialized pickler object
    static member CreateUninitialized (shape : TypeShape) =
        shape.Accept {
            new ITypeShapeVisitor<Pickler> with
                member __.Visit<'T> () = 
                    CompositePickler.CreateUninitialized<'T> () :> Pickler
        }

    static member Cast (shape : TypeShape) (pickler : Pickler) =
        shape.Accept {
            new ITypeShapeVisitor<Pickler> with
                member __.Visit<'T> () = pickler.Cast<'T>() :> Pickler
        }