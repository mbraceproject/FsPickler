module internal Nessos.FsPickler.PicklerGenerator

//
//  Defines a type shape visitor that routes shapes into their
//  corresponding pickler combinator implementations
//

open System
open System.Reflection
open System.Runtime.Serialization

open Nessos.FsPickler
open Nessos.FsPickler.TypeShape


/// Implements a pickler factory type visitor

type private PicklerGeneratorVisitor (resolver : IPicklerResolver) =
        
    interface ITypeShapeVisitor<Pickler> with

        member __.Primitive<'T> () = raise <| NonSerializableTypeException typeof<'T>

        member __.Abstract<'T> () = AbstractPickler.Create<'T> () :> Pickler
        member __.Class<'T when 'T : not struct> () = ClassFieldPickler.Create<'T> resolver :> Pickler
        member __.ISerializable<'T when 'T :> ISerializable> () = ISerializablePickler.Create<'T> () :> Pickler
        member __.IObjectReference<'T when 'T :> ISerializable> () = ISerializablePickler.CreateObjectReferencePickler<'T> () :> Pickler
        member __.NonISerializableException<'T when 'T :> exn> () = ISerializablePickler.CreateNonISerializableExceptionPickler<'T> resolver :> Pickler

        member __.DataContract<'T> () = DataContractPickler.Create<'T> resolver :> Pickler
        member __.CustomPickler<'T> () = CustomPickler.Create<'T> resolver :> Pickler
        member __.CloneableOnly<'T> () = CloneableOnlyPickler.Create<'T> () :> Pickler
        member __.Struct<'T when 'T : struct> () = StructFieldPickler.Create<'T> resolver :> Pickler
        member __.Delegate<'T when 'T :> Delegate> () = DelegatePickler.Create<'T> resolver :> Pickler
        member __.Enum<'E, 'U when 'E : enum<'U>> () = EnumPickler.Create<'E,'U> resolver :> Pickler
        member __.UserFactory<'T> () = PicklerPluginRegistry.GetPicklerFactory<'T>().Create resolver :> Pickler

        member __.Nullable<'T when  'T : (new : unit -> 'T) and 
                                    'T : struct and 
                                    'T :> ValueType> () = NullablePickler.Create<'T> resolver :> Pickler

        member __.Array<'T> () = ArrayPickler.Create<'T> resolver :> Pickler
        member __.Array2D<'T> () = ArrayPickler.Create2D<'T> resolver :> Pickler
        member __.Array3D<'T> () = ArrayPickler.Create3D<'T> resolver :> Pickler
        member __.Array4D<'T> () = ArrayPickler.Create4D<'T> resolver :> Pickler

        member __.Tuple<'T1> () = TuplePickler.Create<'T1> resolver :> Pickler
        member __.Tuple<'T1,'T2> () = TuplePickler.Create<'T1,'T2> resolver :> Pickler
        member __.Tuple<'T1,'T2,'T3> () = TuplePickler.Create<'T1,'T2,'T3> resolver :> Pickler
        member __.Tuple<'T1,'T2,'T3,'T4> () = TuplePickler.Create<'T1,'T2,'T3,'T4> resolver :> Pickler
        member __.Tuple<'T1,'T2,'T3,'T4,'T5> () = TuplePickler.Create<'T1,'T2,'T3,'T4,'T5> resolver :> Pickler
        member __.Tuple<'T1,'T2,'T3,'T4,'T5,'T6> () = TuplePickler.Create<'T1,'T2,'T3,'T4,'T5,'T6> resolver :> Pickler
        member __.Tuple<'T1,'T2,'T3,'T4,'T5,'T6,'T7> () = TuplePickler.Create<'T1,'T2,'T3,'T4,'T5,'T6,'T7> resolver :> Pickler
        member __.Tuple<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'TRest> () = TuplePickler.Create<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'TRest> resolver :> Pickler

        member __.Dictionary<'K,'V when 'K : equality> () = DictionaryPickler.Create<'K,'V> resolver :> Pickler

        member __.FSharpUnion<'Union> () = FsUnionPickler.Create<'Union> resolver :> Pickler
        member __.FSharpRecord<'Record> () = FsRecordPickler.Create<'Record> resolver :> Pickler
        member __.FSharpException<'Exception when 'Exception :> exn> () = FsExceptionPickler.Create<'Exception> resolver :> Pickler
        member __.FSharpList<'T> () = ListPickler.Create<'T> resolver :> Pickler
        member __.FSharpOption<'T> () = OptionPickler.Create<'T> resolver :> Pickler
        member __.FSharpRef<'T> () = FSharpRefPickler.Create<'T> resolver :> Pickler
        member __.FSharpSet<'T when 'T : comparison> () = FSharpSetPickler.Create<'T> resolver :> Pickler
        member __.FSharpMap<'K, 'V when 'K : comparison> () = FSharpMapPickler.Create<'K,'V> resolver :> Pickler

        member __.Choice<'T1,'T2> () = ChoicePickler.Create<'T1,'T2> resolver :> Pickler
        member __.Choice<'T1,'T2,'T3> () = ChoicePickler.Create<'T1,'T2,'T3> resolver :> Pickler
        member __.Choice<'T1,'T2,'T3,'T4> () = ChoicePickler.Create<'T1,'T2,'T3,'T4> resolver :> Pickler
        member __.Choice<'T1,'T2,'T3,'T4,'T5> () = ChoicePickler.Create<'T1,'T2,'T3,'T4,'T5> resolver :> Pickler
        member __.Choice<'T1,'T2,'T3,'T4,'T5,'T6> () = ChoicePickler.Create<'T1,'T2,'T3,'T4,'T5,'T6> resolver :> Pickler
        member __.Choice<'T1,'T2,'T3,'T4,'T5,'T6,'T7> () = ChoicePickler.Create<'T1,'T2,'T3,'T4,'T5,'T6,'T7> resolver :> Pickler


type PicklerGenerator =
        
    /// Constructs a pickler for a given shape
    static member Create (resolver : IPicklerResolver) (shape : TypeShape) =
        let factory = new PicklerGeneratorVisitor(resolver)
        shape.Accept factory

    /// Constructs a blank, uninitialized pickler object
    static member CreateUninitialized (shape : TypeShape) =
        shape.Accept {
            new ITypeVisitor<Pickler> with
                member __.Visit<'T> () = CompositePickler.CreateUninitialized<'T> () :> Pickler
        }