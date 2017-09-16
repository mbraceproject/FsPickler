namespace MBrace.FsPickler.Combinators

open System
open System.IO
open System.Runtime.Serialization

open MBrace.FsPickler
open MBrace.FsPickler.PrimitivePicklers

/// Binary pickling methods

[<RequireQualifiedAccess>]
module Binary =

    let private binarySerializer = lazy(new BinarySerializer())

    /// <summary>
    ///     Pickles a value to binary.
    /// </summary>
    /// <param name="pickler">utilized pickler.</param>
    /// <param name="value">input value.</param>
    let pickle (pickler : Pickler<'T>) (value : 'T) : byte [] =
        binarySerializer.Value.Pickle (value, pickler = pickler)

    /// <summary>
    ///     Unpickles a values from binary.
    /// </summary>
    /// <param name="pickler">utilized pickler.</param>
    /// <param name="pickle">input pickle.</param>
    let unpickle (pickler : Pickler<'T>) (pickle : byte []) =
        binarySerializer.Value.UnPickle (pickle, pickler = pickler)

/// Xml pickling methods

[<RequireQualifiedAccess>]
module Xml =
    let private xmlSerializer = lazy(FsPickler.CreateXmlSerializer())

    /// <summary>
    ///     Pickles a value to XML.
    /// </summary>
    /// <param name="pickler">utilized pickler.</param>
    /// <param name="value">input value.</param>
    let pickle (pickler : Pickler<'T>) (value : 'T) : string =
        xmlSerializer.Value.PickleToString (value, pickler = pickler) : string

    /// <summary>
    ///     Unpickles a values from XML.
    /// </summary>
    /// <param name="pickler">utilized pickler.</param>
    /// <param name="pickle">input pickle.</param>
    let unpickle (pickler : Pickler<'T>) (pickle : string) =
        xmlSerializer.Value.UnPickleOfString (pickle, pickler = pickler)


/// Pickler combinator definitions

[<RequireQualifiedAccess>]
module Pickler =

    // .NET primitive picklers

    let unit = new UnitPickler() :> Pickler<unit>
    let bool = new BooleanPickler() :> Pickler<bool>
    let byte = new BytePickler() :> Pickler<byte>
    let sbyte = new SBytePickler() :> Pickler<sbyte>
    let char = new CharPickler() :> Pickler<char>
    let decimal = new DecimalPickler() :> Pickler<decimal>
    let single = new SinglePickler() :> Pickler<single>
    let double = new DoublePickler() :> Pickler<double>
    let float = double : Pickler<float>
    let int16 = new Int16Pickler() :> Pickler<int16>
    let int = new Int32Pickler() :> Pickler<int>
    let int64 = new Int64Pickler() :> Pickler<int64>
    let uint16 = new UInt16Pickler() :> Pickler<uint16>
    let uint32 = new UInt32Pickler() :> Pickler<uint32>
    let uint64 = new UInt64Pickler() :> Pickler<uint64>

    // misc atomic picklers
    let string = new StringPickler() :> Pickler<string>
    let guid = new GuidPickler() :> Pickler<Guid>
    let dateTime = new DateTimePickler() :> Pickler<DateTime>
    let dateTimeOffset = new DateTimeOffsetPickler() :> Pickler<DateTimeOffset>
    let timeSpan = new TimeSpanPickler() :> Pickler<TimeSpan>
    let bytes = ArrayPickler.CreateByteArrayPickler()
    let bigint = new BigIntPickler() :> Pickler<bigint>

    /// the default System.Object pickler
    let obj = CompositePickler.ObjectPickler

    /// auto generate a pickler
    let auto<'T> = FsPickler.GeneratePickler<'T> ()

    let inline private uc (p : Pickler<'T>) = p :> Pickler

    /// pair pickler combinator
    let pair f g = TuplePickler.Create(f,g)
    /// triple pickler combinator
    let triple f g h = TuplePickler.Create(f,g,h)
    /// quad pickler combinator
    let quad f g h i = TuplePickler.Create(f,g,h,i)
    /// option pickler combinator
    let option (f : Pickler<'T>) = OptionPickler.Create f
    /// nullable pickler combinator
    let nullable (f : Pickler<'T>) = NullablePickler.Create f
    /// Choice<_,_> pickler combinator
    let choice2 f g = ChoicePickler.Create(f,g)
    /// Choice<_,_,_> pickler combinator
    let choice3 f g h = ChoicePickler.Create(f,g,h)
    /// Choice<_,_,_,_> pickler combinator
    let choice4 f g h i = ChoicePickler.Create(f,g,h,i)

    /// FSharp ref pickler combinator
    let ref (f : Pickler<'T>) = FSharpRefPickler.Create f
    /// FSharp list pickler combinator
    let list (f : Pickler<'T>) = ListPickler.Create f
    /// FSharp map pickler combinator
    let map kp vp = FSharpMapPickler.Create(kp,vp)
    /// FSharp set pickler combinator
    let set (f : Pickler<'T>) = FSharpSetPickler.Create f
    /// array pickler combinator
    let array (f : Pickler<'T>) = ArrayPickler.Create<'T> f
    /// array2D pickler combinator
    let array2D (f : Pickler<'T>) = ArrayPickler.Create2D<'T> f
    /// array3D pickler combinator
    let array3D (f : Pickler<'T>) = ArrayPickler.Create3D<'T> f
    /// array4D pickler combinator
    let array4D (f : Pickler<'T>) = ArrayPickler.Create4D<'T> f
    /// sequence pickler combinator ; uses eager evaluation
    let seq f = SeqPickler.Create f

    /// wrap combinator: defines picklers up to isomorphism
    let wrap recover convert p = WrapPickler.Create(p, recover, convert)
    /// alt combinator: choose from list of pickler combinators using tag reader
    let alt tagReader ps = AltPickler.Create(tagReader, ps)

    /// Pickler combinator based on SerializationInfo
    let fromSerializationInfo (ctor : SerializationInfo -> 'T) (proj : SerializationInfo -> 'T -> unit) =
        ISerializablePickler.FromSerializationInfo(ctor, proj)

    /// F# function combinator
    let func<'T, 'U> = AbstractPickler.Create<'T -> 'U> ()

    /// pickler fixpoint combinator
    let fix (F : Pickler<'T> -> Pickler<'T>) =
        let f = CompositePickler.CreateUninitialized<'T>()
        let f' = F f
        f.InitializeFrom f' ; f :> Pickler<'T>

    /// pickler fixpoint combinator
    let fix2 (F : Pickler<'T> -> Pickler<'S> -> Pickler<'T> * Pickler<'S>) =
        let f = CompositePickler.CreateUninitialized<'T>()
        let g = CompositePickler.CreateUninitialized<'S>()
        let f',g' = F f g
        f.InitializeFrom f' ; g.InitializeFrom g'
        f :> Pickler<'T>, g :> Pickler<'S>

    /// pickler fixpoint combinator
    let fix3 (F : Pickler<'T> -> Pickler<'S> -> Pickler<'U> -> Pickler<'T> * Pickler<'S> * Pickler<'U>) =
        let f = CompositePickler.CreateUninitialized<'T>()
        let g = CompositePickler.CreateUninitialized<'S>()
        let h = CompositePickler.CreateUninitialized<'U>()
        let f',g',h' = F f g h
        f.InitializeFrom f' ; g.InitializeFrom g' ; h.InitializeFrom h'
        f :> Pickler<'T>, g :> Pickler<'S>, h :> Pickler<'U>

    /// Experimental support for n-way product types such as records.
    /// See `product` and `field` combinators.
    module ProductInternals =

        /// Internal type for type-checking intermediate values.
        type Part<'R,'X,'Z> =
            private
            | P of ('R -> 'Z) * ('X -> 'Z -> 'R) * Pickler<'Z>

        let private pp f g t =
            P (f, g, t)

        let private finish () =
            pp ignore (fun r () -> r) unit

        /// Internal type for type-checking intermediate values.
        type Wrap<'T> =
            internal
            | W of 'T

            /// Defines an extra field.
            static member ( ^+ ) (W f, x) =
                f x

            /// Defines the last field.
            static member ( ^. ) (W f, W x) =
                f (x (finish ()))

        let internal defProduct e p =
            match p with
            | P (f, g, t) ->
                wrap (g e) f t

        let internal defField proj tf p =
            match p with
            | P (g, h, tr) ->
                pp
                    (fun rr -> (proj rr, g rr))
                    (fun c fx -> h (c (fst fx)) (snd fx))
                    (pair tf tr)

    /// Starts defining a pickler for an n-ary product, such as
    /// record. Example:
    ///
    ///    type Person =
    ///        {
    ///            Address : string
    ///            Age : int
    ///            Name : string
    ///        }
    ///
    ///    let makePerson name age address =
    ///        {
    ///            Address = address
    ///            Age = age
    ///            Name = name
    ///        }
    ///
    ///    let personPickler =
    ///        Pickler.product makePerson
    ///        ^+ Pickler.field (fun p -> p.Name) Pickler.string
    ///        ^+ Pickler.field (fun p -> p.Age) Pickler.int
    ///        ^. Pickler.field (fun p -> p.Address) Pickler.string
    ///
    /// The implementation is not currently efficient, though it
    /// may improve in the future.
    let product f =
        ProductInternals.W (ProductInternals.defProduct f)

    /// See `product`.
    let field f p =
        ProductInternals.W (ProductInternals.defField f p)

    /// Experimental support for n-way sum types such as unions.
    /// See `sum`.
    module SumInternals =

        /// Internal type for type-checking intermediate values.
        type Part<'U,'T,'X,'Y> =
            private
            | P of Pickler<'X> * ('X -> 'U) * (('X -> 'Y) -> ('T -> 'Y))

        let private defP p f g =
            P (p, f, g)

        let private defLastCase inj p =
            defP p inj (fun h t -> t h)

        let private defNextCase inj p (P (tr, xu, f)) =
            defP (choice2 p tr)
                (function
                    | Choice1Of2 x -> inj x
                    | Choice2Of2 x -> xu x)
                (fun g h ->
                    f (fun x -> g (Choice2Of2 x))
                        (h (fun x -> g (Choice1Of2 x))))

        let private defSum ev (P (tr, xu, f)) =
            wrap xu (fun u -> f (fun x -> x) (ev u)) tr

        /// Internal type for type-checking intermediate values.
        type Case<'T1,'T2> =
            internal
            | C of 'T1 * 'T2

            /// Adds a case.
            static member ( ^+ ) (C (i1, p1), W x) =
                W (defNextCase i1 p1 x)

            /// Adds the last case.
            static member ( ^. ) (C (i1, p1), C (i2, p2)) =
                W (defNextCase i1 p1 (defLastCase i2 p2))

        /// Internal type for type-checking intermediate values.
        and Wrap<'T> =
            internal
            | W of 'T

            /// Adds a case.
            static member ( ^+ ) (W f, W x) =
                f x

            /// Adds the last case.
            static member ( ^. ) (W f, C (inj, p)) =
                f (defLastCase inj p)

        let internal makeCase inj p =
            C (inj, p)

        let internal makeSum f =
            W (defSum f)

    /// Starts defining a pickler for an n-ary sum type, such as
    /// a union type. For example:
    ///
    ///    type UnionT =
    ///        | Case1
    ///        | Case2 of int
    ///        | Case3 of string * int
    ///
    ///    let unionTPickler =
    ///        Pickler.sum (fun x k1 k2 k3 ->
    ///            match x with
    ///            | Case1 -> k1 ()
    ///            | Case2 x -> k2 x
    ///            | Case3 (x, y) -> k3 (x, y))
    ///        ^+ Pickler.variant Case1
    ///        ^+ Pickler.case Case2 Pickler.int
    ///        ^. Pickler.case Case3 (Pickler.pair Pickler.string Pickler.int)
    ///
    /// Note that the implementation is not currently efficient,
    /// though it may improve in the future.
    let sum f =
        SumInternals.makeSum f

    /// See `sum`.
    let case inj p =
        SumInternals.makeCase inj p

    /// Useful for union cases without arguments.
    let variant v =
        case (fun () -> v) unit