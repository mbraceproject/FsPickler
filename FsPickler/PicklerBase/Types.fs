namespace Nessos.FsPickler

    open System
    open System.Reflection
    open System.IO
    open System.Runtime.Serialization

    open Nessos.FsPickler.Utils

    /// Specifies that the pickler for this type is to be generated using
    /// the static method 'TypeDef.CreatePickler : IPicklerResolver -> Pickler<TypeDef>'
    [<Sealed>]
    [<System.AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
    type CustomPicklerAttribute () = 
        inherit System.Attribute()


    /// <summary>Provides facility for implementing a custom type serialization scheme.
    /// This is particularly useful in cases where bridging mono/.NET runtimes or
    /// dynamic/static assemblies is required.</summary>
    type ITypeNameConverter =
        abstract member OfSerializedType : TypeInfo -> TypeInfo
        abstract member ToDeserializedType : TypeInfo -> TypeInfo

    and TypeInfo =
        {
            Name : string
            AssemblyInfo : AssemblyInfo
        }

    // An immutable, structurally equatable version of AssemblyName
    and AssemblyInfo =
        {
            Name : string
            Version : string
            Culture : string
            PublicKeyToken : byte []
        }
    with
        static member OfAssemblyName(an : AssemblyName) =
            {
                Name = an.Name
                Version = an.Version.ToString()
                Culture = an.CultureInfo.Name
                PublicKeyToken = an.GetPublicKeyToken()
            }

        static member OfAssembly(a : Assembly) =
            a.GetName() |> AssemblyInfo.OfAssemblyName


    //
    //  Exception Definitions
    //

    /// the base Pickler exception
    type FsPicklerException =
        inherit Exception

        internal new (message : string, ?inner : exn) =
            { inherit Exception(message, defaultArg inner null) }

        internal new (si : SerializationInfo, sc : StreamingContext) =
            { inherit Exception(si, sc) }


    /// raised by pickler generator whenever an unexpected error is encountered.
    type PicklerGenerationException =
        inherit FsPicklerException

        val public Type : Type
        
        internal new (t : Type, ?message : string, ?inner : exn) =
            let message =
                match message with
                | None -> sprintf "Error generating pickler for type '%O'." t
                | Some msg -> sprintf "Error generating pickler for type '%O': %s." t msg

            { 
                inherit FsPicklerException(message, defaultArg inner null)
                Type = t
            }

        internal new (si : SerializationInfo, sc : StreamingContext) =
            {
                inherit FsPicklerException(si, sc)
                Type = si.Read<Type> "picklerType"
            }

        interface ISerializable with
            member e.GetObjectData(si : SerializationInfo, sc : StreamingContext) =
                base.GetObjectData(si, sc)
                si.Write<Type> ("picklerType", e.Type)

    /// raised by pickler generator whenever an unsupported type is encountered in the type graph.
    type NonSerializableTypeException =
        inherit FsPicklerException

        val public Type : Type
        val containedNonSerializableField : Type option

        /// The nested field that caused this type to be non-serializable
        member e.NonSerializableType =
            match e.containedNonSerializableField with
            | None -> e.Type
            | Some ft -> ft

        internal new (t : Type, ?message : string, ?inner : exn) =
            let message =
                match message with
                | None -> sprintf "Type '%O' is not serializable." t
                | Some msg -> sprintf "Type '%O' is not serializable: %s" t msg

            { 
                inherit FsPicklerException(message, defaultArg inner null)
                Type = t 
                containedNonSerializableField = None
            }

        internal new (t : Type, containedNonSerializableField : Type) =
            let message = sprintf "Type '%O' contains non-serializable field of type '%O'." t containedNonSerializableField
            {
                inherit FsPicklerException(message, null)
                Type = t
                containedNonSerializableField = Some containedNonSerializableField
            }

        new (si : SerializationInfo, sc : StreamingContext) =
            {
                inherit FsPicklerException(si, sc)
                Type = si.Read<Type> "picklerType"
                containedNonSerializableField = si.Read<Type option> "containedNonSerializableField"
            }

        interface ISerializable with
            member e.GetObjectData(si : SerializationInfo, sc : StreamingContext) =
                base.GetObjectData(si, sc)
                si.Write<Type> ("picklerType", e.Type)
                si.Write<Type option> ("containedNonSerializableField", e.containedNonSerializableField)