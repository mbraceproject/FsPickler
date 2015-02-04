namespace Nessos.FsPickler

/// F# Extension methods for FsPickler

[<AutoOpen>]
module ExtensionMethods =

    /// Object pickle with type annotation
    type Pickle<'T> internal (bytes : byte []) =
        /// Byte array pickle
        member __.Bytes = bytes

    type FsPicklerSerializer with

        /// <summary>
        ///     Creates a type annotated pickle for given value.
        /// </summary>
        /// <param name="value">Value to be pickled.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        member fsp.PickleTyped(value : 'T, ?streamingContext, ?encoding) : Pickle<'T> = 
            let bytes = fsp.Pickle(value, ?streamingContext = streamingContext, ?encoding = encoding)
            new Pickle<'T>(bytes)

        /// <summary>
        ///     Deserializes a type annotated pickle.
        /// </summary>
        /// <param name="pickle">Type annotated pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        member fsp.UnPickleTyped(pickle : Pickle<'T>, ?streamingContext, ?encoding) : 'T =
            fsp.UnPickle<'T>(pickle.Bytes, ?streamingContext = streamingContext, ?encoding = encoding)

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