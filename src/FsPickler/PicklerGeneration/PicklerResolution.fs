module internal Nessos.FsPickler.PicklerResolution

    open System
    open System.Collections.Generic
    open System.Runtime.CompilerServices

    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection
    open Nessos.FsPickler.TypeShape
    open Nessos.FsPickler.PicklerFactory

    /// resolves a suitable pickler implementation for given type
    let resolvePickler (resolver : IPicklerResolver) (factory : PicklerFactory) 
                        (registerUninitializedPickler : Pickler -> unit) (t : Type) =

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
            let p0 = UninitializedPickler.Create shape
            do registerUninitializedPickler p0

            // step 3: subtype pickler resolution
            let result =
                if t.BaseType <> null then
                    match resolver.Resolve t.BaseType with
                    | p when p.UseWithSubtypes -> Some p
                    | _ -> None
                else
                    None

            // step 4: generate pickler using the pickler factory
            let pickler =
                match result with
                | Some r -> r
                | None -> shape.Accept factory

            // step 5: pickler generation complete, copy data to uninitialized binding and return it
            p0.Unpack
                {
                    // should be IPicklerUnpacker<unit> but F# does not allow this
                    new IPicklerUnpacker<bool> with
                        member __.Apply<'T> (p : Pickler<'T>) =
                            match p with
                            | :? CompositePickler<'T> as p -> p.InitializeFrom pickler ; true
                            | _ -> 
                                let msg = sprintf "Unexpected pickler implementation '%O'" <| p.GetType()
                                raise <| new PicklerGenerationException(p.Type, msg)
                } |> ignore

            Success p0

        with 
        // Store all NonSerializableTypeException's in cache
        | :? NonSerializableTypeException as e when e.Type = t -> Exn.Error e
        | :? NonSerializableTypeException as e ->
            Exn.error <| NonSerializableTypeException(t, e.NonSerializableType)

        // wrap/reraise everything else as PicklerGenerationExceptions
        | :? PicklerGenerationException -> reraise ()
        | e -> raise <| new PicklerGenerationException(t, inner = e) 


    /// recursively builds a pickler implementation for given type
    let generatePickler (globalCache : ICache<Type, Exn<Pickler>>) (t : Type) =

        // a temporary local cache is used to store early, unitialized instances
        // this keeps the global cache from being contaminated with partial state
        let localCache = new Dictionary<Type, Exn<Pickler>> ()

        let rec resolver = 
            {
                new IPicklerResolver with
                    member __.IsSerializable t = (resolve t).IsValue
                    member __.IsSerializable<'T> () = (resolve typeof<'T>).IsValue

                    member __.Resolve t = (resolve t).Value
                    member __.Resolve<'T> () = (resolve typeof<'T>).Value :?> Pickler<'T>
            }

        and factory = new PicklerFactory(resolver)

        and resolve (t : Type) : Exn<Pickler> =
            match globalCache.Lookup t with
            | Some r -> r
            | None ->
                match localCache.TryFind t with
                | Some r -> r
                | None ->
                    let p = resolvePickler resolver factory (fun p -> localCache.Add(p.Type, Success p)) t
                    globalCache.Commit t p

        resolve t