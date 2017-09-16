namespace MBrace.FsPickler

open System
open System.Globalization
open System.Reflection
open System.IO
open System.Runtime.Serialization

open MBrace.FsPickler.Utils
open MBrace.FsPickler.Reflection

/// Defines a stratification of .NET types from simplest to more complex.
type Kind =
    | Primitive             = 0uy
    | Char                  = 1uy // char is a special primitive that should be serialized w.r.t. encoding
    | Enum                  = 2uy
    | Value                 = 3uy
    | Nullable              = 4uy // Nullable is a value type which can be null
    | String                = 5uy
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
[<Flags>]
type ObjectFlags = 
    | None                  = 0uy
    | IsNull                = 1uy
    | IsProperSubtype       = 2uy
    | IsCachedInstance      = 4uy
    | IsCyclicInstance      = 8uy
    | IsSequenceHeader      = 16uy
    | IsSiftedValue         = 32uy

type VisitOrder =
    | PreOrder = 1uy
    | PostOrder = 2uy

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Kind =
    /// builds type info enumeration out of reflection info
    let compute (t : Type) =
        if t = typeof<char> then Kind.Char
        elif t.IsPrimitive then Kind.Primitive
        elif t = typeof<string> then Kind.String
        elif t.IsEnum then Kind.Enum
        elif isNullableType t then Kind.Nullable
        elif t.IsValueType then Kind.Value
        elif t.IsArray then Kind.Array
        elif t.IsSealed then Kind.Sealed
        elif t.IsAbstract then Kind.Abstract
        else Kind.NonSealed


/// Specifies that the pickler for this type is to be generated using
/// the static method 'TypeDef.CreatePickler : IPicklerResolver -> Pickler<TypeDef>'.
[<Sealed>]
[<System.AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type CustomPicklerAttribute () = 
    inherit System.Attribute()

/// Specifies that the type is not serializable but can be freely cloned/hashed
/// by maintaining identical references when found inside object graphs.
[<Sealed>]
[<System.AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type CloneableOnlyAttribute () = 
    inherit System.Attribute()

/// <summary>
///     Serialization information for named types.
/// </summary>
type TypeInfo =
    {
        /// Type name
        Name : string
        /// Assembly Information
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

/// TypeNameConverter utilities
[<RequireQualifiedAccess>]
module TypeNameConverter =
    /// Composes a collection of ITypeNameConverters into one
    let compose (converters : seq<ITypeNameConverter>) =
        let converters = Seq.toArray converters
        { new ITypeNameConverter with
            member x.OfSerializedType(tI: TypeInfo): TypeInfo = 
                let mutable tI = tI
                for i = 0 to converters.Length - 1 do 
                    tI <- converters.[i].OfSerializedType tI
                tI

            member __.ToDeserializedType (tI : TypeInfo) : TypeInfo =
                let mutable tI = tI
                for i = converters.Length - 1 downto 0 do 
                    tI <- converters.[i].ToDeserializedType tI
                tI }

/// <summary>
///     Defines a type conversion scheme in which strong assembly info is dropped 
///     at deserialization.
/// </summary>
[<AutoSerializable(false)>]
type IgnoreStrongNamesConverter (?ignoreVersion : bool) =
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

/// A type name converter that forces deserialization uses the default
/// FSharp.Core version that is loaded in the current AppDomain
[<AutoSerializable(false)>]
type LocalFSharpCoreConverter () =
    let localCore = typeof<int option>.Assembly |> AssemblyInfo.OfAssembly

    interface ITypeNameConverter with
        member x.OfSerializedType (tI: TypeInfo): TypeInfo = tI
        member x.ToDeserializedType (tI : TypeInfo): TypeInfo = 
            if tI.AssemblyInfo.Name = localCore.Name then { tI with AssemblyInfo = localCore }
            else
                tI


/// Declares a sifted version of a version of type 'T
/// Is generated by the sifting implementation of FsPickler.
[<Sealed; DataContract>]
type Sifted<'T> internal (value : 'T, siftedIndices : (int64 * int64 [])[]) =
    [<DataMember(Name = "Value")>]
    let value = value
    // int64 * int64 [] defines an object reference id 
    // together with its matching graph node ids
    [<DataMember(Name = "Indices")>]
    let siftedIndices = siftedIndices
    /// Number of objects that have been sifted from parent graph.
    member __.SiftCount = siftedIndices.Length
    member internal __.Value = value
    member internal __.SiftedIndices = siftedIndices
    override __.ToString() = sprintf "Sift: %A" value

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

    internal new (t : Type, containedNonSerializableField : Type, ?inner : exn) =
        let message = sprintf "Type '%O' contains non-serializable field of type '%O'." t containedNonSerializableField
        {
            inherit FsPicklerException(message, ?inner = inner)
            Type = t
            containedNonSerializableField = Some containedNonSerializableField
        }

    internal new (si : SerializationInfo, sc : StreamingContext) =
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