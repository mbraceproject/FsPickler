namespace FsPickler

    open System
    open System.Globalization
    open System.IO
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



    // reflection - related types

    /// <summary>Provides facility for implementing a custom type serialization scheme.
    /// This is particularly useful in cases where bridging mono/.NET runtimes or
    /// dynamic/static assemblies is required.</summary>
    type ITypeNameConverter =
        abstract member OfSerializedType : TypeInfo -> TypeInfo
        abstract member ToDeserializedType : TypeInfo -> TypeInfo

    and TypeInfo =
        {
            Name : string

            AssemblyName : string
            Version : string
            Culture : string
            PublicKeyToken : byte []
        }
    with
        member internal tI.Assembly : AssemblyInfo =
            {
                Name = tI.AssemblyName
                Version = tI.Version
                Culture = tI.Culture
                PublicKeyToken = tI.PublicKeyToken
            }

    and internal AssemblyInfo =
        {
            Name : string
            Version : string
            Culture : string
            PublicKeyToken : byte []
        }
    with
        member aI.GetType(typeName : string) : TypeInfo =
            {
                Name = typeName
                AssemblyName = aI.Name
                Version = aI.Version
                Culture = aI.Culture
                PublicKeyToken = aI.PublicKeyToken
            }