module internal FsPickler.PicklerResolution

    open System
    open System.Reflection
    open System.Collections.Generic
    open System.Collections.Concurrent
    open System.Runtime.CompilerServices
    open System.Runtime.Serialization

    open Microsoft.FSharp.Reflection

    open FsPickler
    open FsPickler.Utils
    open FsPickler.TypeShape
    open FsPickler.PicklerUtils
    open FsPickler.ReflectionPicklers
    open FsPickler.DotNetPicklers
    open FsPickler.ArrayPickler
    open FsPickler.FSharpPicklers
    open FsPickler.CombinatorImpls

    /// Y combinator with parametric recursion support
    let YParametric (externalCache : ICache<Type,Pickler>)
                    (resolverF : IPicklerResolver -> Type -> Pickler) (t : Type) =

        // use internal cache to avoid corruption in event of exceptions being raised
        let internalCache = new Dictionary<Type, Pickler> ()

        let rec lookup (t : Type) =
            match externalCache.Lookup t with
            | Some f -> f
            | None ->
                match internalCache.TryFind t with
                | Some f -> f
                | None ->
                    // while stack overflows are unlikely here (this is a type-level traversal)
                    // it can be useful in catching a certain class of user errors when declaring custom picklers.
                    try RuntimeHelpers.EnsureSufficientExecutionStack()
                    with :? InsufficientExecutionStackException -> 
                        raise <| PicklerGenerationException(t, "insufficient execution stack.")

                    // start pickler construction
                    let f = UninitializedPickler.CreateUntyped t
                    internalCache.Add(t, f)

                    // perform recursive resolution
                    let f' = resolverF resolver t
                    
                    // complete the recursive binding
                    f.InitializeFrom f'

                    // pickler construction successful, commit to external cache
                    externalCache.Commit t f

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