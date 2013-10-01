namespace FsPickler

    open FsPickler.PicklerUtils
    open FsPickler.CombinatorImpls

    [<AutoOpen>]
    module ExtensionMethods =

        type Pickler with
            /// <summary>Initializes a formatter out of a pair of read/write lambdas.</summary>
            /// <param name="reader">Deserialization logic for the pickler.</param>
            /// <param name="writer">Serialization logic for the pickler.</param>
            /// <param name="cache">Specifies whether the serializer should cache by reference when serializing.</param>
            /// <param name="useWithSubtypes">Specifies whether this specific formatter should apply to all subtypes.</param>
            static member FromPrimitives(reader : Reader -> 'T, writer : Writer -> 'T -> unit, ?cache, ?useWithSubtypes) =
                let cache = defaultArg cache (not typeof<'T>.IsValueType)
                let useWithSubtypes = defaultArg useWithSubtypes false
                mkPickler PicklerInfo.UserDefined useWithSubtypes cache reader writer

        type Writer with
            /// evaluates given sequence, serializing all elements to the underlying stream
            member w.WriteSequence(xs : seq<'T>) =
                let fmt = w.Resolver.Resolve<'T> ()
                writeSeq' fmt w xs

            /// evaluates given sequence of pairs, serializing all elements to the underlying stream
            member w.WriteKeyValueSequence(xs : seq<'K * 'V>) =
                let kf = w.Resolver.Resolve<'K> ()
                let vf = w.Resolver.Resolve<'V> ()
                writeKVPairs' kf vf w xs

        type Reader with
            /// reads the underlying stream for a sequence of items of given type.
            member r.ReadSequence<'T> () =
                let fmt = r.Resolver.Resolve<'T> ()
                readSeq' fmt r

            /// reads the underlying stream for a sequence of pairs of given types.
            member r.ReadKeyValueSequence<'K, 'V> () =
                let kf = r.Resolver.Resolve<'K> ()
                let vf = r.Resolver.Resolve<'V> ()
                readKVPairs' kf vf r