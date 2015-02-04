namespace Nessos.FsPickler

open System
open System.Globalization
open System.Reflection
open System.IO
open System.Runtime.Serialization

open Nessos.FsPickler.Utils
open Nessos.FsPickler.Reflection

/// Defines a type stratification.

type TypeKind =
    | Primitive             = 0uy
    | Char                  = 1uy // char is a special primitive that should be serialized w.r.t. encoding
    | String                = 2uy
    | Enum                  = 3uy
    | Value                 = 4uy
    | Nullable              = 5uy
    | Array                 = 6uy
    | Sealed                = 7uy
    | NonSealed             = 8uy
    | Abstract              = 9uy
    | Delegate              = 10uy

/// Pickler generation metadata.

type PicklerInfo =
    | Primitive             = 0uy
    | Object                = 1uy
    | ReflectionType        = 2uy
    | FieldSerialization    = 3uy
    | DataContract          = 4uy
    | ISerializable         = 5uy
    | Array                 = 6uy
    | FSharpValue           = 7uy // tuples, records and DUs
    | Delegate              = 8uy
    | Combinator            = 9uy
    | UserDefined           = 10uy

/// Specifies runtime properties of serialized objects.

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
        if t = typeof<char> then TypeKind.Char
        elif t.IsPrimitive then TypeKind.Primitive
        elif t = typeof<string> then TypeKind.String
        elif t.IsEnum then TypeKind.Enum
        elif isNullableType t then TypeKind.Nullable
        elif t.IsValueType then TypeKind.Value
        elif t.IsArray then TypeKind.Array
        elif t.IsSealed then TypeKind.Sealed
        elif t.IsAbstract then TypeKind.Abstract
        else TypeKind.NonSealed


/// Specifies that the pickler for this type is to be generated using
/// the static method 'TypeDef.CreatePickler : IPicklerResolver -> Pickler<TypeDef>'.
[<Sealed>]
[<System.AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type CustomPicklerAttribute () = 
    inherit System.Attribute()


/// <summary>
///     Serialization information for named types.
/// </summary>
type TypeInfo =
    {
        Name : string
        AssemblyInfo : AssemblyInfo
    }


/// <summary>
///     An immutable, structurally equatable version of System.Reflection.AssemblyName.
/// </summary>
and AssemblyInfo =
    {
        Name : string
        Version : string
        Culture : string
        PublicKeyToken : string
    }
with
    /// <summary>
    ///     Initializes a new record out of a given assembly name.
    /// </summary>
    /// <param name="name"></param>
    static member OfAssemblyName(name : AssemblyName) =
        {
            Name = name.Name
            Version = 
                match name.Version with
                | null -> null
                | v -> v.ToString()

            Culture =
                match name.CultureInfo with
                | null -> null
                | cI ->
                    if String.IsNullOrEmpty cI.Name then "neutral"
                    else cI.Name

            PublicKeyToken =
                match name.GetPublicKeyToken () with
                | null -> null
                | [||] -> ""
                | pkt -> Bytes.toBase16String pkt
        }

    /// <summary>
    ///     Initializes a new record out of a given assembly.
    /// </summary>
    /// <param name="assembly">input assembly.</param>
    static member OfAssembly(assembly : Assembly) =
        assembly.GetName() |> AssemblyInfo.OfAssemblyName

    /// <summary>
    ///     Defines a new System.Reflection.AssemblyName from given record.
    /// </summary>
    member aI.ToAssemblyName () =
        let an = new AssemblyName()

        an.Name <- aI.Name

        match aI.Version with
        | null | "" -> ()
        | version -> an.Version <- new Version(version)
                
        match aI.Culture with
        | null -> ()
        | "neutral" -> an.CultureInfo <- new CultureInfo("")
        | culture -> an.CultureInfo <- new CultureInfo(culture)

        match aI.PublicKeyToken with
        | null -> ()
        | "" -> an.SetPublicKeyToken [||]
        | pkt -> an.SetPublicKeyToken(Bytes.ofBase16String pkt)

        an

    /// <summary>
    ///     Returns assembly qualified name string from given assembly info.
    /// </summary>
    member __.AssemblyQualifiedName = __.ToAssemblyName().ToString()

/// <summary>
///     Provides facility for user-defined type conversion at 
///     serialization and deserialization.
/// </summary>
type ITypeNameConverter =
    /// TypeInfo to be recorded to serialization
    abstract member OfSerializedType : TypeInfo -> TypeInfo
    /// TypeInfo to be converted at deserialization
    abstract member ToDeserializedType : TypeInfo -> TypeInfo

/// <summary>
///     Defines a type conversion scheme in which strong assembly info is dropped 
///     at deserialization.
/// </summary>
type IgnoreStrongNamesConverter (?ignoreVersion) =
    let ignoreVersion = defaultArg ignoreVersion true
    interface ITypeNameConverter with
        member __.OfSerializedType (tI : TypeInfo) = tI
        member __.ToDeserializedType (tI : TypeInfo) =
            let aI = 
                { tI.AssemblyInfo with 
                    Version = if ignoreVersion then null else tI.AssemblyInfo.Version
                    Culture = null ; 
                    PublicKeyToken = null 
                }
            { tI with AssemblyInfo = aI }


//
//  Exception Definitions
//

/// Base exception raised by the FsPickler library.
type FsPicklerException =
    inherit Exception

    new (message : string, ?inner : exn) =
        { inherit Exception(message, defaultArg inner null) }

    internal new (si : SerializationInfo, sc : StreamingContext) =
        { inherit Exception(si, sc) }
            
/// Raised when pickle is of invalid type.
type InvalidPickleTypeException =
    inherit FsPicklerException

    new (expectedType : string, actualType : string) =
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
            | Some msg -> sprintf "Error generating pickler for type '%O': %s" t msg

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