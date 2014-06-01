namespace Nessos.FsPickler

    open System
    open System.Runtime.Serialization
    
    /// A factory pattern for defining pluggable picklers.
    /// Types implementing this interface must declare a method of type:
    ///
    ///     Create<'T1, ... , 'Tn | constraints> : IPicklerResolver -> Pickler
    ///
    /// The 'Create' method may or may not contain generic parameters.
    type IPicklerFactory = interface end

    // a few guideline templates that inherit the above interface

    and IConstantPicklerFactory =
        inherit IPicklerFactory
        abstract Create : IPicklerResolver -> Pickler

    and IGenericPicklerFactory1 =
        inherit IPicklerFactory
        abstract Create<'T1> : IPicklerResolver -> Pickler

    and IGenericPicklerFactory2 =
        inherit IPicklerFactory
        abstract Create<'T1, 'T2> : IPicklerResolver -> Pickler

    and IGenericPicklerFactory3 =
        inherit IPicklerFactory
        abstract Create<'T1, 'T2, 'T3> : IPicklerResolver -> Pickler

    and IGenericPicklerFactory4 =
        inherit IPicklerFactory
        abstract Create<'T1, 'T2, 'T3, 'T4> : IPicklerResolver -> Pickler

    and IGenericPicklerFactory5 =
        inherit IPicklerFactory
        abstract Create<'T1, 'T2, 'T3, 'T4, 'T5> : IPicklerResolver -> Pickler

    and IGenericPicklerFactory6 =
        inherit IPicklerFactory
        abstract Create<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> : IPicklerResolver -> Pickler

    and IGenericPicklerFactory7 =
        inherit IPicklerFactory
        abstract Create<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> : IPicklerResolver -> Pickler

    and IGenericPicklerFactory8 =
        inherit IPicklerFactory
        abstract Create<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'T8> : IPicklerResolver -> Pickler

    /// raised by pickler generator whenever an unexpected error is encountered while calling pickler factories
    type PicklerFactoryException =
        inherit SerializationException

        val private factoryType : Type

        new (factory : IPicklerFactory, ?message : string, ?inner : exn) =
            let ft = factory.GetType()
            let message =
                match message with
                | None -> sprintf "Error calling pluggable pickler factory '%O'." ft
                | Some msg -> sprintf "Error calling pluggable pickler factory '%O': %s" ft msg

            let inner = defaultArg inner null

            { inherit SerializationException(message, inner) ; factoryType = ft }

        new (sI : SerializationInfo, sc : StreamingContext) =
            {
                inherit SerializationException(sI, sc)
                factoryType = sI.Read<Type> "factoryType"
            }

        member __.FactoryType = __.factoryType

        interface ISerializable with
            member __.GetObjectData(sI : SerializationInfo, sc : StreamingContext) =
                base.GetObjectData(sI, sc)
                sI.Write<Type> ("factoryType", __.factoryType)