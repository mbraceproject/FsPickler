module internal MBrace.FsPickler.PicklerResolution

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

open MBrace.FsPickler
open MBrace.FsPickler.Reflection
open MBrace.FsPickler.PicklerGenerator

let isSerializable (result : Exn<Pickler>) =
    match result with
    | Success p -> not p.IsCloneableOnly
    | Error _ -> false

/// reflection - based pickler resolution
let resolvePickler (registry : ICustomPicklerRegistry) (resolver : IPicklerResolver) 
                    (mkEarlyBinding : Pickler -> unit) 
                    (isPicklerReferenced : Pickler -> bool) (t : Type) =

    try
        // while stack overflows are unlikely here (this is type-level traversal)
        // it can be useful in catching a certain class of user errors when declaring custom picklers.
#if PROTECT_STACK_OVERFLOWS
        try RuntimeHelpers.EnsureSufficientExecutionStack ()
        with :? InsufficientExecutionStackException ->
            raise <| new PicklerGenerationException(t, "insufficient execution stack.")
#endif

        // step 1: resolve shape of given type
        let shape = PicklerGenerator.ExtractShape t

        // step 2: create an uninitialized pickler instance and register to the local cache
        let p0 = PicklerGenerator.CreateUninitialized shape
        mkEarlyBinding p0

        // step 3: subtype pickler resolution
        let result =
            if isNotNull t.BaseType then 
                try 
                    let baseP = resolver.Resolve t.BaseType
                    if baseP.UseWithSubtypes then
                        let pickler = PicklerGenerator.Cast shape baseP
                        Some pickler
                    else
                        None

                with :? NonSerializableTypeException -> None
            else
                None

        // step 4: consult the pickler factory.
        let p =
            match result with
            | Some p -> p
            | None -> PicklerGenerator.Create registry resolver shape

        // step 5: pickler generation complete, copy data to uninitialized binding
        if isPicklerReferenced p0 then CompositePickler.Copy(p, p0) ; Success p0
        else Success p

    with 
    // Store all NonSerializableTypeException's in cache
    | :? NonSerializableTypeException as e when e.Type = t -> Exn.Error e
    | :? NonSerializableTypeException as e ->
        Exn.error <| NonSerializableTypeException(t, e.Type, e)

    // wrap/reraise everything else as PicklerGenerationExceptions
    | :? PicklerGenerationException -> reraise ()
    | e -> raise <| new PicklerGenerationException(t, inner = e)

type private GenerationState =
    | Fresh       = 0
    | Referenced  = 1
    | Completed   = 2

/// recursively generates picklers required for given type, 
/// storing results in global cache when completed.
let generatePickler (registry : ICustomPicklerRegistry) (globalCache : ICache<Type, Exn<Pickler>>) (t : Type) =
    // a temporary local cache is used to store generated picklers.
    // this serves the purpose of providing recursive bindings rectypes.
    // a local cache keeps the global cache from being contaminated with partial state.
    // the boolean flag indicates whether generation is completed for given pickler
    let localCache = new Dictionary<Type, GenerationState * Exn<Pickler>> ()

    let rec resolver =
        {
            new IPicklerResolver with
                member __.IsSerializable t = isSerializable(generate t)
                member __.IsSerializable<'T> () = isSerializable(generate typeof<'T>)

                member __.Resolve t = (generate t).Value
                member __.Resolve<'T> () = (generate typeof<'T>).Value :?> Pickler<'T>
        }

    and generate (t : Type) : Exn<Pickler> =
        match globalCache.Lookup t with
        | Some p -> p
        | None ->
            match localCache.TryFind t with
            | Some (GenerationState.Fresh,p) -> 
                localCache.[t] <- (GenerationState.Referenced, p) 
                p
            | Some (_,p) -> p
            | None ->
                let p = resolvePickler registry resolver 
                                (fun p -> localCache.Add(p.Type, (GenerationState.Fresh, Success p))) 
                                (fun p -> let s,_ = localCache.[p.Type] in s = GenerationState.Referenced) t

                localCache.[t] <- (GenerationState.Completed, p)
                p

    let p = generate t

    // pickler generation complete
    // now commit to global cache

    for (KeyValue(t',(genState, p'))) in localCache do
        // only cache completed picklers other than the current
        // if p is exception, only cache results that are exceptions
        if genState = GenerationState.Completed && t' <> t && (p.IsValue || p'.IsException) then
            globalCache.Commit t' p' |> ignore

    globalCache.Commit t p