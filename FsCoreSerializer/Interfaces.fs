namespace FsCoreSerializer

    open System
    
    /// an ISerializable-like pattern for describing serialization rules in new types
    /// serialization is performed by providing direct access to the underlying stream
    /// through the Reader/Writer types.
    ///
    /// Types implementing this interface require a constructor 'new (reader : Reader)'
    type IFsCoreSerializable =
        abstract GetObjectData : Writer -> unit

    /// A factory pattern for generating formatters for generic types
    and internal IFormatterFactory =
        abstract Create : (Type -> Lazy<Formatter>) -> Formatter

    /// A factory pattern for generating formatters for generic types
    /// any type implementing this interface must declare a method
    ///
    ///     Create<'T1, ... , 'Tn | constraints> : (Type -> Lazy<Formatter>) -> Formatter
    ///
    /// Generic formatters are registered and resolved at runtime. 
    and IGenericFormatterFactory = interface end

    // a few guideline templates that inherit the above interface

    and IGenericFormatterFactory1 =
        inherit IGenericFormatterFactory
        abstract Create<'T> : (Type -> Lazy<Formatter>) -> Formatter

    and IGenericFormatterFactory2 =
        inherit IGenericFormatterFactory
        abstract Create<'T, 'S> : (Type -> Lazy<Formatter>) -> Formatter

    and IGenericFormatterFactory3 =
        inherit IGenericFormatterFactory
        abstract Create<'T, 'S, 'U> : (Type -> Lazy<Formatter>) -> Formatter

    and IGenericFormatterFactory4 =
        inherit IGenericFormatterFactory
        abstract Create<'T, 'S, 'U, 'V> : (Type -> Lazy<Formatter>) -> Formatter