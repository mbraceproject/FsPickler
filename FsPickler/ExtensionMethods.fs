namespace Nessos.FsPickler

    open Nessos.FsPickler.PicklerUtils
    open Nessos.FsPickler.CombinatorImpls

    [<AutoOpen>]
    module ExtensionMethods =

        type Pickler with
            /// <summary>Initializes a pickler out of a pair of read/write lambdas. Unsafe pickler generation method.</summary>
            /// <param name="reader">Deserialization logic for the pickler.</param>
            /// <param name="writer">Serialization logic for the pickler.</param>
            /// <param name="cache">Specifies whether the serializer should cache by reference when serializing.</param>
            /// <param name="useWithSubtypes">Specifies whether this specific pickler should apply to all subtypes.</param>
            static member FromPrimitives<'T>(reader : ReadState -> 'T, writer : WriteState -> 'T -> unit, ?cache, ?useWithSubtypes) =
                if typeof<'T>.IsPrimitive then
                    invalidArg "T" "defining custom picklers for primitives not supported."

                let cache = defaultArg cache (not typeof<'T>.IsValueType)
                let useWithSubtypes = defaultArg useWithSubtypes false
                mkPickler PicklerInfo.UserDefined useWithSubtypes cache reader writer

        type WriteState with
            /// evaluates given sequence, serializing all elements to the underlying stream
            member w.WriteSequence (tag : string) (xs : seq<'T>) =
                let p = w.PicklerResolver.Resolve<'T> ()
                writeSequence p tag w xs

            /// evaluates given sequence of pairs, serializing all elements to the underlying stream
            member w.WriteKeyValueSequence (tag : string) (xs : seq<'K * 'V>) =
                let kp = w.PicklerResolver.Resolve<'K> ()
                let vp = w.PicklerResolver.Resolve<'V> ()
                writePairSequence kp vp tag w xs

        type ReadState with
            /// reads the underlying stream for a sequence of items of given type.
            member r.readSequence<'T> (tag : string) =
                let p = r.PicklerResolver.Resolve<'T> ()
                readSequence p tag r

            /// reads the underlying stream for a sequence of pairs of given types.
            member r.ReadKeyValueSequence<'K, 'V> (tag : string) =
                let kp = r.PicklerResolver.Resolve<'K> ()
                let vp = r.PicklerResolver.Resolve<'V> ()
                readPairSequence kp vp tag r