module internal FsPickler.PicklerResolution

    open System
    open System.Reflection
    open System.Collections.Generic
    open System.Collections.Concurrent
    open System.Runtime.Serialization

    open Microsoft.FSharp.Reflection

    open FsPickler
    open FsPickler.Utils
    open FsPickler.TypeShape
    open FsPickler.PicklerUtils
    open FsPickler.ReflectionPicklers
    open FsPickler.DotNetPicklers
    open FsPickler.FSharpPicklers

    /// Y combinator with parametric recursion support
    let YParametric (resolverName : string)
                    (externalCacheLookup : Type -> Pickler option)
                    (externalCacheCommit : Type -> Pickler -> unit)
                    (resolverF : IPicklerResolver -> Type -> Pickler) (t : Type) =

        // use internal cache to avoid corruption in event of exceptions being raised
        let internalCache = new Dictionary<Type, Pickler> ()

        let rec lookup (t : Type) =
            match externalCacheLookup t with
            | Some f -> f
            | None ->
                match internalCache.TryFind t with
                | Some f -> f
                | None ->
                    // start pickler construction
                    let f = UninitializedPickler.CreateUntyped t
                    internalCache.Add(t, f)

                    let f' = resolverF resolver t
                    do 
                        f.InitializeFrom f'
                        f.ResolverName <- resolverName

                    // pickler construction successful, commit to external cache
                    do externalCacheCommit t f
                    f

        and resolver =
            {
                new IPicklerResolver with
                    member __.Id = resolverName
                    member __.Resolve<'T> () = lookup typeof<'T> :?> Pickler<'T>
                    member __.Resolve (t : Type) = lookup t
            }

        lookup t


    // reflection - based pickler resolution

    let resolvePickler (customPicklers : Map<string, Pickler>)
                        (picklerFactoryIndex : PicklerFactoryIndex)
                        (resolver : IPicklerResolver) (t : Type) =

        // check if type is supported
        if isUnSupportedType t then raise <| new NonSerializableTypeException(t)

        // subtype resolution
        let result =
            if t.BaseType <> null then
                match resolver.Resolve t.BaseType with
                | fmt when fmt.UseWithSubtypes -> Some fmt
                | _ -> None
            else
                None

        // custom pickler lookup
        let result =
            match result with
            | Some _ -> result
            | None ->
                customPicklers.TryFind t.AssemblyQualifiedName

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
                elif FSharpType.IsTuple t then
                    Some <| TuplePickler.CreateUntyped(t, resolver)
                elif FSharpType.IsRecord(t, allMembers) then
                    Some <| FsRecordPickler.CreateUntyped(t, resolver, isExceptionType = false)
                elif FSharpType.IsExceptionRepresentation(t, allMembers) then
                    Some <| FsRecordPickler.CreateUntyped(t, resolver, isExceptionType = true)
                else None

        // .NET serialization interfaces
        let result =
            match result with
            | Some _ -> result
            | None ->
                if t.IsAbstract then 
                    Some <| AbstractPickler.CreateUntyped t
                elif typeof<ISerializable>.IsAssignableFrom t then
                    ISerializablePickler.TryCreateUntyped(t, resolver)
                else None

        // .NET reflection serialization
        let result =
            match result with
            | None ->
                if t.IsEnum then 
                    EnumPickler.CreateUntyped(t, resolver)
                elif t.IsValueType then 
                    StructPickler.CreateUntyped(t, resolver)
                elif t.IsArray then 
                    ArrayPickler.CreateUntyped(t, resolver)
                elif typeof<System.Delegate>.IsAssignableFrom t then
                    DelegatePickler.CreateUntyped(t, resolver)
                elif not t.IsSerializable then 
                    raise <| new NonSerializableTypeException(t)
                else
                    ClassPickler.CreateUntyped(t, resolver)
            | Some r -> r

        result