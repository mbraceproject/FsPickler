namespace FsCoreSerializer

    open FsCoreSerializer.FormatterUtils
    open FsCoreSerializer.FSharpCombinators


    [<AutoOpen>]
    module ExtensionMethods =

        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        [<RequireQualifiedAccess>]
        module Formatter =

            let inline auto<'T> (resolver : IFormatterResolver) = resolver.Resolve<'T> ()
            
            let pair f g = PairFormatter.Create(f,g)
            let triple f g h = TripleFormatter.Create(f,g,h)
            let quad f g h i = QuadFormatter.Create(f,g,h,i)

            let option f = OptionFormatter.Create f
            let choice2 f g = Choice2Formatter.Create(f,g)
            let choice3 f g h = Choice3Formatter.Create(f,g,h)
            let choice4 f g h i = Choice4Formatter.Create(f,g,h,i)

            let ref f = FSharpRefFormatter.Create f
            let list f = ListFormatter.Create f
            let seq f = SeqFormatter.Create f
            let pairSeq kf vf = KeyValueSeqFormatter.Create(kf, vf)
            let map f = FSharpMapFormatter.Create f
            let set f = FSharpSetFormatter.Create f

            let wrap fmt recover convert = WrapFormatter.Create(fmt, convert, recover)
            let alt tagReader fmts = AltFormatter.Create(tagReader, fmts)


        type Formatter with
            /// <summary>Initializes a formatter out of a pair of read/write lambdas.</summary>
            /// <param name="cache">Specifies whether the serializer should cache by reference when serializing.</param>
            /// <param name="useWithSubtypes">Specifies whether this specific formatter should apply to all subtypes.</param>
            static member FromPrimitives(reader : Reader -> 'T, writer : Writer -> 'T -> unit, ?cache, ?useWithSubtypes) =
                let cache = defaultArg cache (not typeof<'T>.IsValueType)
                let useWithSubtypes = defaultArg useWithSubtypes false
                mkFormatter FormatterInfo.Custom useWithSubtypes cache reader writer

        type Writer with
            member w.WriteSequence(xs : seq<'T>) =
                let fmt = w.ResolveFormatter<'T> ()
                writeSeq' fmt w xs

            member w.WriteKeyValueSequence(xs : seq<'K * 'V>) =
                let kf = w.ResolveFormatter<'K> ()
                let vf = w.ResolveFormatter<'V> ()
                writeKVPairs' kf vf w xs

        type Reader with
            member r.ReadSequence<'T> () =
                let fmt = r.ResolveFormatter<'T> ()
                readSeq' fmt r

            member r.ReadKeyValueSequence<'K, 'V> () =
                let kf = r.ResolveFormatter<'K> ()
                let vf = r.ResolveFormatter<'V> ()
                readKVPairs' kf vf r