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
    open Nessos.FsPickler.PicklerUtils
    open Nessos.FsPickler.ReflectionPicklers
    open Nessos.FsPickler.DotNetPicklers
    open Nessos.FsPickler.ArrayPickler
    open Nessos.FsPickler.FSharpPicklers
    open Nessos.FsPickler.CombinatorImpls

    /// Y combinator with parametric recursion support
    let YParametric (globalCache : ICache<Type, Exn<Pickler>>)
                    (resolverF : IPicklerResolver -> Type -> Pickler) (t : Type) =

        // use internal cache to avoid corruption in event of exceptions being raised
        let localCache = new Dictionary<Type, Pickler> ()

        let rec lookup (t : Type) =
            match globalCache.Lookup t with
            | Some f -> f.Value
            | None ->
                match localCache.TryFind t with
                | Some f -> f
                | None ->
                    // while stack overflows are unlikely here (this is a type-level traversal)
                    // it can be useful in catching a certain class of user errors when declaring custom picklers.
                    try RuntimeHelpers.EnsureSufficientExecutionStack()
                    with :? InsufficientExecutionStackException -> 
                        raise <| PicklerGenerationException(t, "insufficient execution stack.")

                    // start pickler construction
                    let pickler =
                        try
                            let f = UninitializedPickler.CreateUntyped t
                            localCache.Add(t, f)

                            // perform recursive resolution
                            let f' = resolverF resolver t
                    
                            // complete the recursive binding
                            f.InitializeFrom f'

                            Success f

                        // all pickler generations that fail due to the type being deemed
                        // non-serializable, are to be stored in the cache as exceptions
                        with :? NonSerializableTypeException as e -> Error e

                    // pickler generation complete, commit to cache
                    let commited = globalCache.Commit t pickler
                    
                    commited.Value

        and resolver =
            {
                new IPicklerResolver with
                    member __.Resolve<'T> () = lookup typeof<'T> :?> Pickler<'T>
                    member __.Resolve (t : Type) = lookup t
            }

        lookup t


    // reflection - based pickler resolution

    let resolvePickler (picklerFactoryIndex : PicklerFactoryIndex) (resolver : IPicklerResolver) (t : Type) =

        // check if type is supported
        if isUnSupportedType t then raise <| NonSerializableTypeException t

        // subtype resolution
        let result =
            if t.BaseType <> null then
                match resolver.Resolve t.BaseType with
                | fmt when fmt.UseWithSubtypes -> Some fmt
                | _ -> None
            else
                None

        // pickler factories
        let result =
            match result with
            | Some _ -> result
            | None ->
                if containsAttr<CustomPicklerAttribute> t then
                    Some <| CustomPickler.Create(t, resolver)
                else
                    None

        // pluggable pickler factories, resolved by type shape
        let result =
            match result with
            | Some _ -> result
            | None -> picklerFactoryIndex.TryResolvePicklerFactory(t, resolver)

        // FSharp Values
        let result =
            match result with
            | Some _ -> result
            | None ->
                if FSharpType.IsUnion(t, allMembers) then
                    Some <| FsUnionPickler.CreateUntyped(t, resolver)
                elif FSharpType.IsRecord(t, allMembers) then
                    Some <| FsRecordPickler.CreateUntyped(t, resolver)
                elif FSharpType.IsExceptionRepresentation(t, allMembers) then
                    Some <| FsExceptionPickler.CreateUntyped(t, resolver)
                else None

        // .NET serialization resolution
        let result =
            match result with
            | None ->
                if t.IsAbstract then 
                    AbstractPickler.CreateUntyped t
                elif t.IsEnum then 
                    EnumPickler.CreateUntyped(t, resolver)
                elif isNullableType t then
                    NullablePickler.CreateUntyped(t, resolver) 
                elif t.IsValueType then 
                    StructPickler.CreateUntyped(t, resolver)
                elif t.IsArray then 
                    ArrayPickler.CreateUntyped(t, resolver)
                elif typeof<System.Delegate>.IsAssignableFrom t then
                    DelegatePickler.CreateUntyped(t, resolver)
                elif isISerializable t then
                    ISerializablePickler.CreateUntyped(t, resolver)
                elif not t.IsSerializable then 
                    raise <| NonSerializableTypeException t
                else
                    ClassPickler.CreateUntyped(t, resolver)
            | Some r -> r

        result