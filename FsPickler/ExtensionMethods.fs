namespace FsPickler

    open FsPickler.PicklerUtils
    open FsPickler.CombinatorImpls


    [<AutoOpen>]
    module ExtensionMethods =

        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        [<RequireQualifiedAccess>]
        module Pickler =

            let private resolver = lazy(PicklerCache.GetDefault() :> IPicklerResolver)

            let auto<'T> = resolver.Value.Resolve<'T> ()
            
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

            let wrap recover convert fmt = WrapPickler.Create(fmt, recover, convert)
            let alt tagReader fmts = AltPickler.Create(tagReader, fmts)

            let fix (F : Pickler<'T> -> Pickler<'T>) =
                let f = new Pickler<'T>()
                let f' = F f
                f.InitializeFrom f' ; f

            let fix2 (F : Pickler<'T> -> Pickler<'S> -> Pickler<'T> * Pickler<'S>) =
                let f = new Pickler<'T>()
                let g = new Pickler<'S>()
                let f',g' = F f g
                f.InitializeFrom f' ; g.InitializeFrom g'
                f',g'


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
                let fmt = w.Resolver.Resolve<'T> ()
                writeSeq' fmt w xs

            member w.WriteKeyValueSequence(xs : seq<'K * 'V>) =
                let kf = w.Resolver.Resolve<'K> ()
                let vf = w.Resolver.Resolve<'V> ()
                writeKVPairs' kf vf w xs

        type Reader with
            member r.ReadSequence<'T> () =
                let fmt = r.Resolver.Resolve<'T> ()
                readSeq' fmt r

            member r.ReadKeyValueSequence<'K, 'V> () =
                let kf = r.Resolver.Resolve<'K> ()
                let vf = r.Resolver.Resolve<'V> ()
                readKVPairs' kf vf r