namespace FsPickler

    open System.IO

    open FsPickler.BasePicklers
    open FsPickler.CombinatorImpls

    module Combinators =

        let private defaultSerializer = lazy(new FsPickler())

        /// pickles a value
        let pickle (pickler : Pickler<'T>) (value : 'T) : byte [] =
            use mem = new MemoryStream()
            defaultSerializer.Value.Serialize(pickler, mem, value)
            mem.ToArray()

        /// upickles a value
        let unpickle (pickler : Pickler<'T>) (data : byte []) =
            use mem = new MemoryStream(data)
            defaultSerializer.Value.Deserialize(pickler, mem)

        [<RequireQualifiedAccess>]
        module Pickler =

            // F# primitives

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

            let any = mkObjPickler () : Pickler<obj>
            let string = mkStringPickler ()
            let guid = mkGuidPickler ()
            let bytes = mkByteArrayPickler ()
            let bigint = mkBigIntPickler () : Pickler<bigint>

            /// pair pickler combinator
            let pair f g = PairPickler.Create(f,g)
            /// triple pickler combinator
            let triple f g h = TriplePickler.Create(f,g,h)
            /// quad pickler combinator
            let quad f g h i = QuadPickler.Create(f,g,h,i)
            /// option pickler combinator
            let option f = OptionPickler.Create f
            /// Choice<_,_> pickler combinator
            let choice2 f g = Choice2Pickler.Create(f,g)
            /// Choice<_,_,_> pickler combinator
            let choice3 f g h = Choice3Pickler.Create(f,g,h)
            /// Choice<_,_,_,_> pickler combinator
            let choice4 f g h i = Choice4Pickler.Create(f,g,h,i)

            /// FSharp ref pickler combinator
            let ref f = FSharpRefPickler.Create f
            /// FSharp list pickler combinator
            let list f = ListPickler.Create f
            /// FSharp map pickler combinator
            let map f = FSharpMapPickler.Create f
            /// FSharp set pickler combinator
            let set f = FSharpSetPickler.Create f
            /// array pickler combinator
            let array f = DotNetPicklers.ArrayPickler.Create<'T, 'T []> f
            /// array2D pickler combinator
            let array2D f = DotNetPicklers.ArrayPickler.Create<'T, 'T [,]> f
            /// array3D pickler combinator
            let array3D f = DotNetPicklers.ArrayPickler.Create<'T, 'T [,,]> f
            /// array4D pickler combinator
            let array4D f = DotNetPicklers.ArrayPickler.Create<'T, 'T [,,,]> f
            /// sequence pickler combinator ; uses eager evaluation
            let seq f = SeqPickler.Create f
            /// sequence of pairs pickler combinator ; uses eager evaluation
            let pairSeq kf vf = KeyValueSeqPickler.Create(kf, vf)

            /// wrap combinator: defines picklers up to isomorphism
            let wrap recover convert fmt = WrapPickler.Create(fmt, recover, convert)
            /// alt combinator: choose pickler combinators using tag reader
            let alt tagReader fmts = AltPickler.Create(tagReader, fmts)

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
                f',g'

            /// pickler fixpoint combinator
            let fix3 (F : Pickler<'T> -> Pickler<'S> -> Pickler<'U> -> Pickler<'T> * Pickler<'S> * Pickler<'U>) =
                let f = new Pickler<'T>()
                let g = new Pickler<'S>()
                let h = new Pickler<'U>()
                let f',g',h' = F f g h
                f.InitializeFrom f' ; g.InitializeFrom g' ; h.InitializeFrom h'
                f',g',h'