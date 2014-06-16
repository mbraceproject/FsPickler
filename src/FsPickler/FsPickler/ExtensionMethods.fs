namespace Nessos.FsPickler

//    open Nessos.FsPickler.PicklerUtils

    [<AutoOpen>]
    module ExtensionMethods =

        type Pickler with
            /// <summary>Initializes a pickler out of a pair of read/write lambdas. Unsafe pickler generation method.</summary>
            /// <param name="reader">Deserialization logic for the pickler.</param>
            /// <param name="writer">Serialization logic for the pickler.</param>
            /// <param name="cacheByRef">Specifies whether objects serialized by this pickler should be cached by reference.</param>
            /// <param name="useWithSubtypes">Specifies whether pickler should also apply for all subtypes.</param>
            static member FromPrimitives<'T>(reader : ReadState -> string -> 'T, writer : WriteState -> string -> 'T -> unit, ?cacheByRef, ?useWithSubtypes) =
                if typeof<'T>.IsPrimitive then
                    invalidArg typeof<'T>.FullName "defining custom picklers for primitives not supported."

                let cacheByRef = defaultArg cacheByRef (not typeof<'T>.IsValueType)
                let useWithSubtypes = defaultArg useWithSubtypes false
                CompositePickler.Create(reader, writer, PicklerInfo.UserDefined, cacheByRef = cacheByRef, useWithSubtypes = useWithSubtypes)