namespace FsPickler

    open System
    open System.Globalization
    open System.IO
    open System.Runtime.Serialization

    open FsPickler.Utils

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
    type PicklerGenerationException =
        inherit SerializationException

        val private ty : Type
        
        new (t : Type, ?message : string, ?inner : exn) =
            let message =
                match message with
                | None -> sprintf "Error generating pickler for type '%O'." t
                | Some msg -> sprintf "Error generating pickler for type '%O': %s." t msg

            match inner with
            | None -> { inherit SerializationException(message) ; ty = t }
            | Some e -> { inherit SerializationException(message, e) ; ty = t }

        new (sI : SerializationInfo, sc : StreamingContext) =
            {
                inherit SerializationException(sI, sc)
                ty = SerializationInfo.read sI "picklerType"
            }

        member __.GeneratedType = __.ty

        interface ISerializable with
            member __.GetObjectData(sI : SerializationInfo, sc : StreamingContext) =
                base.GetObjectData(sI, sc)
                SerializationInfo.write sI "picklerType" __.ty


    /// raised by pickler generator whenever an unsupported type is encountered in the type graph.
    type NonSerializableTypeException =
        inherit SerializationException

        val private ty : Type

        new (t : Type, ?message : string, ?inner : exn) =
            let message =
                match message with
                | None -> sprintf "Serialization of type '%O' is not supported." t
                | Some msg -> sprintf "Serialization of type '%O' is not supported: %s" t msg

            match inner with
            | None -> { inherit SerializationException(message) ; ty = t }
            | Some e -> { inherit SerializationException(message, e) ; ty = t }

        new (sI : SerializationInfo, sc : StreamingContext) =
            {
                inherit SerializationException(sI, sc)
                ty = SerializationInfo.read sI "picklerType"
            }

        member __.UnsupportedType = __.ty

        interface ISerializable with
            member __.GetObjectData(sI : SerializationInfo, sc : StreamingContext) =
                base.GetObjectData(sI, sc)
                SerializationInfo.write sI "picklerType" __.ty

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

            match inner with
            | None -> { inherit SerializationException(message) ; factoryType = ft }
            | Some e -> { inherit SerializationException(message, e) ; factoryType = ft }

        member __.FactoryType = __.factoryType

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
        member t.AssemblyQualifiedName = 
            let sb = new System.Text.StringBuilder()
            let inline add (x:string) = sb.Append x |> ignore
            add t.Name
            add ", Version="
            add (if String.IsNullOrEmpty t.Version then "0.0.0.0" else t.Version)
            add ", Culture="
            add (if String.IsNullOrEmpty t.Culture then "neutral" else t.Culture)
            add ", PublicKeyToken="
            if t.PublicKeyToken.Length = 0 then add "null"
            else
                for b in t.PublicKeyToken do
                    add <| sprintf "%02x" b

            sb.ToString()

        member t.FullName = sprintf "%s, %s" t.Name t.AssemblyQualifiedName


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