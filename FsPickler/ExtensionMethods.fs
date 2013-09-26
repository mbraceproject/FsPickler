namespace FsPickler

    open FsPickler.PicklerUtils
    open FsPickler.FSharpCombinators


    [<AutoOpen>]
    module ExtensionMethods =

        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        [<RequireQualifiedAccess>]
        module Pickler =

            let inline auto<'T> (resolver : IPicklerResolver) = resolver.Resolve<'T> ()
            
            let pair f g = PairPickler.Create(f,g)
            let triple f g h = TriplePickler.Create(f,g,h)
            let quad f g h i = QuadPickler.Create(f,g,h,i)

            let option f = OptionPickler.Create f
            let choice2 f g = Choice2Pickler.Create(f,g)
            let choice3 f g h = Choice3Pickler.Create(f,g,h)
            let choice4 f g h i = Choice4Pickler.Create(f,g,h,i)

            let ref f = FSharpRefPickler.Create f
            let list f = ListPickler.Create f
            let seq f = SeqPickler.Create f
            let pairSeq kf vf = KeyValueSeqPickler.Create(kf, vf)
            let map f = FSharpMapPickler.Create f
            let set f = FSharpSetPickler.Create f

            let wrap fmt recover convert = WrapPickler.Create(fmt, convert, recover)
            let alt tagReader fmts = AltPickler.Create(tagReader, fmts)


        type Pickler with
            /// <summary>Initializes a formatter out of a pair of read/write lambdas.</summary>
            /// <param name="cache">Specifies whether the serializer should cache by reference when serializing.</param>
            /// <param name="useWithSubtypes">Specifies whether this specific formatter should apply to all subtypes.</param>
            static member FromPrimitives(reader : Reader -> 'T, writer : Writer -> 'T -> unit, ?cache, ?useWithSubtypes) =
                let cache = defaultArg cache (not typeof<'T>.IsValueType)
                let useWithSubtypes = defaultArg useWithSubtypes false
                mkPickler PicklerInfo.UserDefined useWithSubtypes cache reader writer

        type Writer with
            member w.WriteSequence(xs : seq<'T>) =
                let fmt = w.ResolvePickler<'T> ()
                writeSeq' fmt w xs

            member w.WriteKeyValueSequence(xs : seq<'K * 'V>) =
                let kf = w.ResolvePickler<'K> ()
                let vf = w.ResolvePickler<'V> ()
                writeKVPairs' kf vf w xs

        type Reader with
            member r.ReadSequence<'T> () =
                let fmt = r.ResolvePickler<'T> ()
                readSeq' fmt r

            member r.ReadKeyValueSequence<'K, 'V> () =
                let kf = r.ResolvePickler<'K> ()
                let vf = r.ResolvePickler<'V> ()
                readKVPairs' kf vf r