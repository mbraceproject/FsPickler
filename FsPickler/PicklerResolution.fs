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
    let YParametric (externalCache : ConcurrentDictionary<Type, Pickler>)
                    (resolverF : IPicklerResolver -> Type -> Pickler) 
                    (t : Type) =

        // use internal cache to avoid corruption in event of exceptions being raised
        let internalCache = new Dictionary<Type, Pickler> ()

        let rec recurse (t : Type) =
            match externalCache.TryFind t with
            | None ->
                match internalCache.TryFind t with
                | None ->
                    let fmt = UninitializedPickler.CreateUntyped t
                    internalCache.Add(t, fmt)
                    let fmt' = resolverF resolver t
                    do fmt.InitializeFrom(fmt')
                    // recursive operation successful, commit to external cache
                    externalCache.TryAdd(t, fmt) |> ignore
                    fmt
                | Some l -> l
            | Some f -> f

        and resolver =
            {
                new IPicklerResolver with
                    member __.Resolve<'T> () = recurse typeof<'T> :?> Pickler<'T>
                    member __.Resolve (t : Type) = recurse t
            }

        recurse t

    // recursive formatter resolution

    let resolvePickler (typeNameConverter : ITypeNameConverter) 
                            (genericIdx : GenericPicklerIndex) 
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

        // resolve factory method
        let result =
            match result with
            | Some _ -> result
            | None -> PicklerFactory.TryCall(t, resolver)

        // lookup generic shapes
        let result =
            match result with
            | Some _ -> result
            | None ->
                if t.IsGenericType || t.IsArray then
                    genericIdx.TryResolveGenericPickler(t, resolver)
                else
                    None

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