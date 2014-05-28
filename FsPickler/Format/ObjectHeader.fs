namespace Nessos.FsPickler

    open System
    
    open Nessos.FsPickler.Reflection

    /// Type information

    type TypeInfo =
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
        | Atomic                = 1uy
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
    module ObjectFlags =
        let inline hasFlag (flags : ObjectFlags) (flag : ObjectFlags) = flags &&& flag = flag

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TypeInfo =
        /// builds type info enumeration out of reflection info
        let compute (t : Type) =
            if t.IsPrimitive then TypeInfo.Primitive
            elif t = typeof<string> then TypeInfo.String
            elif t.IsEnum then TypeInfo.Enum
            elif t.IsValueType then TypeInfo.Value
            elif t.IsArray then TypeInfo.Array
            elif isAssignableFromArray t then TypeInfo.ArrayCompatible
            elif t.IsSealed then TypeInfo.Sealed
            elif t.IsAbstract then TypeInfo.Abstract
            else TypeInfo.NonSealed