module internal Nessos.FsPickler.PicklerResolution

    open System
    open System.Collections.Generic
    open System.Runtime.CompilerServices

    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection
    open Nessos.FsPickler.TypeShape
    open Nessos.FsPickler.PicklerFactory

    /// <summary>
    ///     Y combinator for parametric recursion.
    /// </summary>
    /// <param name="external">External cache for memoization.</param>
    /// <param name="F">Recursive operation functional; first parameter is an early registration operation.</param>
    /// <param name="t">Initial input.</param>
    let YParametric (external : ICache<'T,'S>) (F : ('T -> 'S -> unit) -> ('T -> 'S) -> ('T -> 'S)) (t : 'T) =

        // a temporary local cache is used to store early, unitialized instances
        // this keeps the global cache from being contaminated with partial state
        let local = new Dictionary<'T, 'S> ()

        let rec recurse (t : 'T) =
            match external.Lookup t with
            | Some s -> s
            | None ->
                match local.TryFind t with
                | Some s -> s
                | None ->
                    let s = F (fun t s -> local.Add(t,s)) recurse t
                    external.Commit t s

        recurse t


    /// reflection - based pickler resolution
    let resolvePickler (earlyRegister : Type -> Exn<Pickler> -> unit) (self : Type -> Exn<Pickler>) (t : Type) =
        
        // the inner IPicklerResolver simply encapsulates recursive calls to this method
        let resolver =
            {
                new IPicklerResolver with
                    member __.IsSerializable t = (self t).IsValue
                    member __.IsSerializable<'T> () = (self typeof<'T>).IsValue

                    member __.Resolve t = (self t).Value
                    member __.Resolve<'T> () = (self typeof<'T>).Value :?> Pickler<'T>
            }

        try
            // while stack overflows are unlikely here (this is type-level traversal)
            // it can be useful in catching a certain class of user errors when declaring custom picklers.
            try RuntimeHelpers.EnsureSufficientExecutionStack ()
            with :? InsufficientExecutionStackException ->
                raise <| new PicklerGenerationException(t, "insufficient execution stack.")

            // step 1: resolve shape of given type
            let shape = 
                try TypeShape.resolve t
                with UnSupportedShape -> raise <| NonSerializableTypeException(t)

            // step 2: create an uninitialized pickler instance and register to the local cache
            let p0 = PicklerFactory.CreateUninitialized shape
            do earlyRegister t (Success p0)

            // step 3: subtype pickler resolution
            let result =
                if t.BaseType <> null then
                    match resolver.Resolve t.BaseType with
                    | p when p.UseWithSubtypes -> Some p
                    | _ -> None
                else
                    None

            // step 4: consult the pickler factory.
            let p =
                match result with
                | Some p -> p
                | None -> PicklerFactory.Create resolver shape

            // step 5; pickler generation complete, copy data to uninitialized binding
            CompositePickler.Copy(p, p0)

            Success p0

        with 
        // Store all NonSerializableTypeException's in cache
        | :? NonSerializableTypeException as e when e.Type = t -> Exn.Error e
        | :? NonSerializableTypeException as e ->
            Exn.error <| NonSerializableTypeException(t, e.NonSerializableType)

        // wrap/reraise everything else as PicklerGenerationExceptions
        | :? PicklerGenerationException -> reraise ()
        | e -> raise <| new PicklerGenerationException(t, inner = e) 