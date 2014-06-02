module internal Nessos.FsPickler.PicklerResolution

    open System
    open System.Reflection
    open System.Collections.Generic
    open System.Collections.Concurrent
    open System.Runtime.CompilerServices
    open System.Runtime.Serialization

    open Microsoft.FSharp.Reflection

    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection
    open Nessos.FsPickler.TypeShape
    open Nessos.FsPickler.DotNetPicklers
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
    let resolvePickler (registerUninitializedPickler : Type -> Exn<Pickler> -> unit) (self : Type -> Exn<Pickler>) (t : Type) =

        let mkResolver () =
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

            // resolve shape of given type
            let shape = 
                try TypeShape.resolve t
                with UnSupportedShape -> raise <| NonSerializableTypeException(t)

            // create an uninitialized pickler instance and register to the local cache
            let p0 = UninitializedPickler.Create shape
            do registerUninitializedPickler t (Success p0)

            // initialize a local pickler resolver instance
            let resolver = mkResolver ()

            // step 1: subtype pickler resolution
            let result =
                if t.BaseType <> null then
                    match resolver.Resolve t.BaseType with
                    | p when p.UseWithSubtypes -> Some p
                    | _ -> None
                else
                    None

            // step 2: check for [<CustomPickler>] attributes
            let result =
                match result with
                | Some _ -> result
                | None ->
                    if containsAttr<CustomPicklerAttribute> t then
                        Some <| CustomPickler.Create(t, resolver)
                    else
                        None

            // step 3: consult the pickler factory.
            let pickler =
                match result with
                | Some r -> r
                | None ->
                    let factory = new PicklerFactory(resolver)
                    shape.Accept factory

            // pickler generation complete, copy data to uninitialized binding and return it
            p0.InitializeFrom pickler
            Success p0

        with 
        // Store all NonSerializableTypeException's in cache
        | :? NonSerializableTypeException as e when e.Type = t -> Exn.Error e
        | :? NonSerializableTypeException as e ->
            Exn.error <| NonSerializableTypeException(t, e.NonSerializableType)

        // wrap/reraise everything else as PicklerGenerationExceptions
        | :? PicklerGenerationException -> reraise ()
        | e -> raise <| new PicklerGenerationException(t, inner = e) 