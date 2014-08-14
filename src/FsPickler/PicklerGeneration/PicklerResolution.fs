module internal Nessos.FsPickler.PicklerResolution

    open System
    open System.Collections.Generic
    open System.Runtime.CompilerServices

    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection
    open Nessos.FsPickler.TypeShape
    open Nessos.FsPickler.PicklerFactory

    /// reflection - based pickler resolution
    let resolvePickler (resolver : IPicklerResolver) (mkEarlyBinding : Pickler -> unit) (t : Type) =

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
            do mkEarlyBinding p0

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

    /// recursively generates picklers required for given type, 
    /// storing results in global cache when completed.
    let generatePickler (globalCache : ICache<Type, Exn<Pickler>>) (t : Type) =
        // a temporary local cache is used to store generated picklers.
        // this serves the purpose of providing recursive bindings rectypes.
        // a local cache keeps the global cache from being contaminated with partial state.
        // the boolean flag indicates whether generation is completed for given pickler
        let localCache = new Dictionary<Type, bool * Exn<Pickler>> ()

        let getCompletedPicklers() =
            localCache |> Seq.choose(function (KeyValue(t,(true,p))) -> Some(t,p) | _ -> None)

        let rec resolver =
            {
                new IPicklerResolver with
                    member __.IsSerializable t = (generate t).IsValue
                    member __.IsSerializable<'T> () = (generate typeof<'T>).IsValue

                    member __.Resolve t = (generate t).Value
                    member __.Resolve<'T> () = (generate typeof<'T>).Value :?> Pickler<'T>
            }

        and generate (t : Type) : Exn<Pickler> =
            match globalCache.Lookup t with
            | Some p -> p
            | None ->
                match localCache.TryFind t with
                | Some (_,p) -> p
                | None ->
                    let p = resolvePickler resolver (fun p -> localCache.Add(p.Type, (false, Success p))) t
                    localCache.[t] <- (true, p)
                    p

        let p = generate t

        // pickler generation complete
        // now commit to global cache

        for t',p' in getCompletedPicklers() do
            // only cache completed picklers other than the current
            // if p is exception, only cache results that are exceptions
            if t' <> t && (p.IsValue || p'.IsException) then
                globalCache.Commit t' p' |> ignore

        globalCache.Commit t p