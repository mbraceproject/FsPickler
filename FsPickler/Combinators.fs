namespace FsPickler

    open System.IO

    open FsPickler.PicklerUtils
    open FsPickler.BasePicklers
    open FsPickler.ReflectionPicklers
    open FsPickler.CombinatorImpls

    module Combinators =

        let private defaultSerializer = lazy(new FsPickler())

        /// pickles a value
        let pickle (pickler : Pickler<'T>) (value : 'T) : byte [] =
            defaultSerializer.Value.Pickle pickler value

        /// upickles a value
        let unpickle (pickler : Pickler<'T>) (data : byte []) =
            defaultSerializer.Value.UnPickle pickler data

        [<RequireQualifiedAccess>]
        module Pickler =

            // .NET primitive picklers

            let unit = mkUnitP ()
            let bool = mkBoolP ()
            let byte = mkByteP ()
            let sbyte = mkSByteP ()
            let char = mkCharP ()
            let decimal = mkDecimalP ()
            let single = mkSingleP ()
            let float = mkFloatP ()
            let int16 = mkInt16P ()
            let int = mkInt32P ()
            let int64 = mkInt64P()
            let uint16 = mkUInt16P ()
            let uint32 = mkUInt32P ()
            let uint64 = mkUInt64P ()

            // misc atomic picklers
            let string = mkStringPickler ()
            let guid = mkGuidPickler ()
            let bytes = mkByteArrayPickler ()
            let bigint = mkBigIntPickler () : Pickler<bigint>

            /// the default System.Object pickler
            let obj = mkObjPickler () : Pickler<obj>

            /// auto generate a pickler
            let auto<'T> = defaultSerializer.Value.GeneratePickler<'T> ()

            let inline private uc (p : Pickler<'T>) = p :> Pickler

            /// pair pickler combinator
            let pair f g = PairPickler.Create(f,g) |> setPicklerId [uc f; uc g]
            /// triple pickler combinator
            let triple f g h = TriplePickler.Create(f,g,h) |> setPicklerId [uc f; uc g; uc h]
            /// quad pickler combinator
            let quad f g h i = QuadPickler.Create(f,g,h,i) |> setPicklerId [uc f; uc g; uc h; uc i]
            /// option pickler combinator
            let option f = OptionPickler.Create f |> setPicklerId [uc f]
            /// Choice<_,_> pickler combinator
            let choice2 f g = Choice2Pickler.Create(f,g) |> setPicklerId [uc f; uc g]
            /// Choice<_,_,_> pickler combinator
            let choice3 f g h = Choice3Pickler.Create(f,g,h) |> setPicklerId [uc f; uc g; uc h]
            /// Choice<_,_,_,_> pickler combinator
            let choice4 f g h i = Choice4Pickler.Create(f,g,h,i) |> setPicklerId [uc f; uc g; uc h; uc i]

            /// FSharp ref pickler combinator
            let ref f = FSharpRefPickler.Create f |> setPicklerId [uc f]
            /// FSharp list pickler combinator
            let list f = ListPickler.Create f |> setPicklerId [uc f]
            /// FSharp map pickler combinator
            let map kp vp = FSharpMapPickler.Create(kp,vp) |> setPicklerId [uc kp; uc kp]
            /// FSharp set pickler combinator
            let set f = FSharpSetPickler.Create f |> setPicklerId [uc f]
            /// array pickler combinator
            let array f = DotNetPicklers.ArrayPickler.Create<'T, 'T []> f |> setPicklerId [uc f]
            /// array2D pickler combinator
            let array2D f = DotNetPicklers.ArrayPickler.Create<'T, 'T [,]> f |> setPicklerId [uc f]
            /// array3D pickler combinator
            let array3D f = DotNetPicklers.ArrayPickler.Create<'T, 'T [,,]> f |> setPicklerId [uc f]
            /// array4D pickler combinator
            let array4D f = DotNetPicklers.ArrayPickler.Create<'T, 'T [,,,]> f |> setPicklerId [uc f]
            /// sequence pickler combinator ; uses eager evaluation
            let seq f = SeqPickler.Create f |> setPicklerId [uc f]
            /// sequence of pairs pickler combinator ; uses eager evaluation
            let pairSeq kp vp = KeyValueSeqPickler.Create(kp, vp) |> setPicklerId [uc kp; uc vp]

            /// wrap combinator: defines picklers up to isomorphism
            let wrap recover convert p = WrapPickler.Create(p, recover, convert) |> setPicklerId [p]
            /// alt combinator: choose from list of pickler combinators using tag reader
            let alt tagReader ps = AltPickler.Create(tagReader, ps) |> setPicklerId (ps |> Seq.map uc)

            /// F# function combinator
            let func<'T, 'U> = AbstractPickler.Create<'T -> 'U> ()

            /// pickler fixpoint combinator
            let fix (F : Pickler<'T> -> Pickler<'T>) =
                let f = new Pickler<'T>()
                let f' = F f
                f.InitializeFrom f' ; f

            /// pickler fixpoint combinator
            let fix2 (F : Pickler<'T> -> Pickler<'S> -> Pickler<'T> * Pickler<'S>) =
                let f = new Pickler<'T>()
                let g = new Pickler<'S>()
                let f',g' = F f g
                f.InitializeFrom f' ; g.InitializeFrom g'
                f,g

            /// pickler fixpoint combinator
            let fix3 (F : Pickler<'T> -> Pickler<'S> -> Pickler<'U> -> Pickler<'T> * Pickler<'S> * Pickler<'U>) =
                let f = new Pickler<'T>()
                let g = new Pickler<'S>()
                let h = new Pickler<'U>()
                let f',g',h' = F f g h
                f.InitializeFrom f' ; g.InitializeFrom g' ; h.InitializeFrom h'
                f,g,h