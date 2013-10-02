namespace FsPickler

    open System
    open System.Runtime.Serialization

    /// Marks a type that uses a pickler generated from a static factory method.
    type CustomPicklerAttribute () = inherit System.Attribute()

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
        abstract Create<'T> : IPicklerResolver -> Pickler

    and IGenericPicklerFactory2 =
        inherit IPicklerFactory
        abstract Create<'T, 'S> : IPicklerResolver -> Pickler

    and IGenericPicklerFactory3 =
        inherit IPicklerFactory
        abstract Create<'T, 'S, 'U> : IPicklerResolver -> Pickler

    and IGenericPicklerFactory4 =
        inherit IPicklerFactory
        abstract Create<'T, 'S, 'U, 'V> : IPicklerResolver -> Pickler


    /// raised by pickler generator whenever an unexpected error is encountered.
    type PicklerGenerationException internal (t : Type, ?message : string) =
        inherit SerializationException(
            match message with
            | None -> sprintf "Error while generating pickler for type '%O'." t
            | Some msg -> sprintf "Error while generating pickler for type '%O': %s" t msg)

        member __.GeneratedType = t

    /// raised by pickler generator whenever an unsupported type is encountered in the type graph.
    type NonSerializableTypeException internal (unsupportedType : Type, ?message : string) =
        inherit SerializationException(
            match message with
            | None -> sprintf "Serialization of type '%O' is not supported." unsupportedType
            | Some msg -> sprintf "Serialization of type '%O' is not supported: %s" unsupportedType msg)

        member __.UnsupportedType = unsupportedType

    /// raised by pickler generator whenever an unexpected error is encountered while calling pickler factories
    type PicklerFactoryException internal (picklerFactory : IPicklerFactory, ?message) =
        inherit SerializationException(
            match message with
            | None -> sprintf "Error calling pluggable pickler factory '%O'." (picklerFactory.GetType())
            | Some msg -> sprintf "Error calling pluggable pickler factory '%O': %s" (picklerFactory.GetType()) msg)

        member __.PicklerFactory = picklerFactory

