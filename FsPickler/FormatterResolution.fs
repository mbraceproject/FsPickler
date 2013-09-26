module internal FsPickler.FormatterResolution

    open System
    open System.Reflection
    open System.Collections.Generic
    open System.Collections.Concurrent
    open System.Runtime.Serialization

    open Microsoft.FSharp.Reflection

    open FsPickler
    open FsPickler.Utils
    open FsPickler.TypeShape
    open FsPickler.FormatterUtils
    open FsPickler.ReflectionFormatters
    open FsPickler.DotNetFormatters
    open FsPickler.FSharpFormatters

    /// Y combinator with parametric recursion support
    let YParametric (externalCache : ConcurrentDictionary<Type, Formatter>)
                    (resolverF : IFormatterResolver -> Type -> Formatter) 
                    (t : Type) =

        // use internal cache to avoid corruption in event of exceptions being raised
        let internalCache = new Dictionary<Type, Formatter> ()

        let rec recurse (t : Type) =
            match externalCache.TryFind t with
            | None ->
                match internalCache.TryFind t with
                | None ->
                    let fmt = UninitializedFormatter.CreateUntyped t
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
                new IFormatterResolver with
                    member __.Resolve<'T> () = recurse typeof<'T> :?> Formatter<'T>
                    member __.Resolve (t : Type) = recurse t
            }

        recurse t

    // recursive formatter resolution

    let resolveFormatter (typeNameConverter : ITypeNameConverter) 
                            (genericIdx : GenericFormatterIndex) 
                            (resolver : IFormatterResolver) (t : Type) =

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
            | None -> FormatterFactory.TryCall(t, resolver)

        // lookup generic shapes
        let result =
            match result with
            | Some _ -> result
            | None ->
                if t.IsGenericType || t.IsArray then
                    genericIdx.TryResolveGenericFormatter(t, resolver)
                else
                    None

        // FSharp Values
        let result =
            match result with
            | Some _ -> result
            | None ->
                if FSharpType.IsUnion(t, allMembers) then
                    Some <| FsUnionFormatter.CreateUntyped(t, resolver)
                elif FSharpType.IsTuple t then
                    Some <| TupleFormatter.CreateUntyped(t, resolver)
                elif FSharpType.IsRecord(t, allMembers) then
                    Some <| FsRecordFormatter.CreateUntyped(t, resolver, isExceptionType = false)
                elif FSharpType.IsExceptionRepresentation(t, allMembers) then
                    Some <| FsRecordFormatter.CreateUntyped(t, resolver, isExceptionType = true)
                else None

        // .NET serialization interfaces
        let result =
            match result with
            | Some _ -> result
            | None ->
                if t.IsAbstract then 
                    Some <| AbstractFormatter.CreateUntyped t
                elif typeof<ISerializable>.IsAssignableFrom t then
                    ISerializableFormatter.TryCreateUntyped(t, resolver)
                else None

        // .NET reflection serialization
        match result with
        | None ->
            if t.IsEnum then 
                EnumFormatter.CreateUntyped(t, resolver)
            elif t.IsValueType then 
                StructFormatter.CreateUntyped(t, resolver)
            elif t.IsArray then 
                ArrayFormatter.CreateUntyped(t, resolver)
            elif typeof<System.Delegate>.IsAssignableFrom t then
                DelegateFormatter.CreateUntyped(t, resolver)
            elif not t.IsSerializable then 
                raise <| new NonSerializableTypeException(t)
            else
                ClassFormatter.CreateUntyped(t, resolver)
        | Some r -> r