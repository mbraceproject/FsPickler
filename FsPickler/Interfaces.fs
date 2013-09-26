namespace FsPickler

    open System
    open System.Runtime.Serialization

    /// A factory pattern for generating formatters for generic types
    /// any type implementing this interface must declare a method
    ///
    ///     Create<'T1, ... , 'Tn | constraints> : IPicklerResolver -> Pickler
    ///
    /// Generic formatters are registered and resolved at runtime. 
    type IGenericPicklerFactory = interface end

    // a few guideline templates that inherit the above interface

    and IGenericPicklerFactory1 =
        inherit IGenericPicklerFactory
        abstract Create<'T> : IPicklerResolver -> Pickler

    and IGenericPicklerFactory2 =
        inherit IGenericPicklerFactory
        abstract Create<'T, 'S> : IPicklerResolver -> Pickler

    and IGenericPicklerFactory3 =
        inherit IGenericPicklerFactory
        abstract Create<'T, 'S, 'U> : IPicklerResolver -> Pickler

    and IGenericPicklerFactory4 =
        inherit IGenericPicklerFactory
        abstract Create<'T, 'S, 'U, 'V> : IPicklerResolver -> Pickler

    /// Raised by FsPickler whenever an unsupported type is encountered in the object graph.
    type NonSerializableTypeException(unsupportedType : Type, ?message : string) =
        inherit SerializationException(
            match message with
            | None -> sprintf "Serialization of type '%O' is not supported." unsupportedType
            | Some msg -> sprintf "Serialization of type '%O' is not supported: %s" unsupportedType msg)

        member __.UnsupportedType = unsupportedType