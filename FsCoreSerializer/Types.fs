namespace FsCoreSerializer

    open System

//    type IFormatterResolver =
//        abstract Resolve<'T> : unit -> Formatter<'T>
    
    /// an ISerializable-like pattern for describing serialization rules in new types
    /// serialization is performed by providing direct access to the underlying stream
    /// through the Reader/Writer types.
    ///
    /// Types implementing this interface require a constructor 'new (reader : Reader)'
    type IFsCoreSerializable =
        abstract GetObjectData : Writer -> unit

    /// A factory pattern for precomputing formatters
    and IFormatterFactory =
        abstract Type : Type
        abstract Create : IFormatterResolver -> Formatter

    /// A factory pattern for generating formatters for generic types
    /// any type implementing this interface must declare a method
    ///
    ///     Create<'T1, ... , 'Tn | constraints> : IFormatterResolver -> Formatter
    ///
    /// Generic formatters are registered and resolved at runtime. 
    and IGenericFormatterFactory = interface end

    // a few guideline templates that inherit the above interface

    and IGenericFormatterFactory1 =
        inherit IGenericFormatterFactory
        abstract Create<'T> : IFormatterResolver -> Formatter

    and IGenericFormatterFactory2 =
        inherit IGenericFormatterFactory
        abstract Create<'T, 'S> : IFormatterResolver -> Formatter

    and IGenericFormatterFactory3 =
        inherit IGenericFormatterFactory
        abstract Create<'T, 'S, 'U> : IFormatterResolver -> Formatter

    and IGenericFormatterFactory4 =
        inherit IGenericFormatterFactory
        abstract Create<'T, 'S, 'U, 'V> : IFormatterResolver -> Formatter


    open System.Runtime.Serialization

    /// Raised by FsCoreSerializer whenever an unsupported type is encountered in the object graph.
    type NonSerializableTypeException(unsupportedType : Type, ?message : string) =
        inherit SerializationException(
            match message with
            | None -> sprintf "Serialization of type '%O' is not supported." unsupportedType
            | Some msg -> sprintf "Serialization of type '%O' is not supported: %s" unsupportedType msg)

        member __.UnsupportedType = unsupportedType