namespace Nessos.FsPickler

    open System
    open System.Reflection
    open System.IO
    open System.Runtime.Serialization

    open Nessos.FsPickler.Utils
    open Nessos.FsPickler.Reflection

    /// Type stratification

    type TypeKind =
        | Primitive             = 0uy
        | String                = 1uy
        | Enum                  = 2uy
        | Value                 = 3uy
        | Array                 = 4uy
        | Sealed                = 5uy
        | NonSealed             = 6uy
        | Abstract              = 7uy
        | ArrayCompatible       = 8uy // interfaces assignable from arrays
        | Delegate              = 9uy

    /// Pickler generation information

    type PicklerInfo =
        | Primitive             = 0uy
        | Object                = 1uy
        | ReflectionType        = 2uy
        | FieldSerialization    = 3uy
        | ISerializable         = 4uy
        | Array                 = 5uy
        | FSharpValue           = 6uy // tuples, records and DUs
        | Delegate              = 7uy
        | Combinator            = 8uy
        | UserDefined           = 9uy

    /// flags that specify runtime properties of instances

    type ObjectFlags = 
        | None                  = 0uy
        | IsNull                = 1uy
        | IsProperSubtype       = 2uy
        | IsCachedInstance      = 4uy
        | IsCyclicInstance      = 8uy
        | IsSequenceHeader      = 16uy
        

        
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module internal ObjectFlags =
        let inline hasFlag (flags : ObjectFlags) (flag : ObjectFlags) = flags &&& flag = flag

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module internal TypeKind =
        /// builds type info enumeration out of reflection info
        let compute (t : Type) =
            if t.IsPrimitive then TypeKind.Primitive
            elif t = typeof<string> then TypeKind.String
            elif t.IsEnum then TypeKind.Enum
            elif t.IsValueType then TypeKind.Value
            elif t.IsArray then TypeKind.Array
            elif isAssignableFromArray t then TypeKind.ArrayCompatible
            elif t.IsSealed then TypeKind.Sealed
            elif t.IsAbstract then TypeKind.Abstract
            else TypeKind.NonSealed


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


    type InvalidPickleTypeException =
        inherit FsPicklerException

        internal new (expectedType : string, actualType : string) =
            let message = sprintf "Expected pickle of type '%s' but was '%s'." expectedType actualType
            { inherit FsPicklerException(message) }

        internal new (si : SerializationInfo, sc : StreamingContext) =
            { inherit FsPicklerException(si, sc) }


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