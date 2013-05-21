module FsUnit

    open NUnit.Framework
    open NUnit.Framework.Constraints

    let should (f : 'a -> #Constraint) x (y : obj) =
        let c = f x
        let y =
            match y with
            | :? (unit -> unit) -> box (new TestDelegate(y :?> unit -> unit))
            | _                 -> y
        Assert.That(y, c)

    let shouldMatch (f : 'a -> bool) (x : 'a) =
        if f x then () else raise <| new AssertionException(sprintf "Unexpected result: %A." x)

    let between x y = new AndConstraint(new GreaterThanOrEqualConstraint(x), new LessThanOrEqualConstraint(y))
    let betweenExclusive x y = new AndConstraint(new GreaterThanConstraint(x), new LessThanConstraint(y))

    let equal x = new EqualConstraint(x)

    let not x = new NotConstraint(x)

    let contain x = new ContainsConstraint(x)

    let haveLength n = Has.Length.EqualTo(n)

    let haveCount n = Has.Count.EqualTo(n)

    let be = id

    let haveExactType t = new ExactTypeConstraint(t)

    let Null = new NullConstraint()

    let Empty = new EmptyConstraint()

    let EmptyString = new EmptyStringConstraint()

    let NullOrEmptyString = new NullOrEmptyStringConstraint()

    let True = new TrueConstraint()

    let False = new FalseConstraint()

    let sameAs x = new SameAsConstraint(x)

    let throw = Throws.TypeOf

    let shouldFailWith<'Exn when 'Exn :> exn> (f : unit -> unit) =
        try
            f() ; raise <| new AssertionException(sprintf "Expected exception of type %s" (typeof<'Exn>.ToString()))
        with :? 'Exn -> ()

