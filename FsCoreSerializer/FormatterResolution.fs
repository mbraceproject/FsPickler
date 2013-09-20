module internal FsCoreSerializer.FormatterResolution

    open System
    open System.Reflection
    open System.Collections.Generic
    open System.Collections.Concurrent
    open System.Runtime.Serialization

    open Microsoft.FSharp.Reflection

    open FsCoreSerializer
    open FsCoreSerializer.Utils
    open FsCoreSerializer.TypeShape
    open FsCoreSerializer.FormatterUtils
    open FsCoreSerializer.BaseFormatters
    open FsCoreSerializer.DotNetFormatters
    open FsCoreSerializer.ArrayFormatter
    open FsCoreSerializer.FSharpTypeFormatters

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
                            (factoryIdx : Map<string, IFormatterFactory>) 
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

        // lookup factory index
        let result =
            match result with
            | Some _ -> result
            | None ->
                match factoryIdx.TryFind t.AssemblyQualifiedName with
                | Some ff -> 
                    let f = ff.Create resolver
                    if f.Type <> t then
                        new SerializationException(sprintf "Invalid formatter factory: expected type '%s' but got '%s'." t.Name f.Type.Name)
                        |> raise
                    else
                        Some f
                | None -> None

        // lookup generic shapes
        let result =
            match result with
            | Some _ -> result
            | None ->
                if t.IsGenericType || t.IsArray then
                    genericIdx.TryResolveGenericFormatter(t, resolver)
                elif FSharpType.IsUnion(t, memberBindings) then
                    Some <| FsUnionFormatter.CreateUntyped(t, resolver)
                elif typeof<IFsCoreSerializable>.IsAssignableFrom t then
                    Some <| FsCoreSerialibleFormatter.CreateUntyped(t, resolver)
                elif typeof<ISerializable>.IsAssignableFrom t then
                    SerializableFormatter.TryCreateUntyped(t, resolver)
                else None

//        // lookup F# types
//        let result =
//            match result with
//            | Some _ -> result
//            | None ->
//                if FSharpType.IsTuple t then
//                    mkTupleFormatter self t |> Some
//                elif FSharpType.IsUnion(t, memberBindings) then
//                    mkUnionFormatter self t |> Some
//                elif FSharpType.IsRecord(t, memberBindings) then
//                    mkRecordFormatter self t |> Some
//#if EMIT_IL
//                elif FSharpType.IsExceptionRepresentation(t, memberBindings) then
//                    mkExceptionFormatter self t |> Some
//#endif
//                else None

        // .NET reflection serialization
        match result with
        | None ->
            if t.IsEnum then 
                EnumFormatter.CreateUntyped(t, resolver)
            elif t.IsValueType then 
                StructFormatter.CreateUntyped(t, resolver)
            elif t.IsAbstract then 
                AbstractFormatter.CreateUntyped t
            elif t.IsArray then 
                ArrayFormatter.CreateUntyped(t, resolver)
            elif typeof<System.Delegate>.IsAssignableFrom t then
                DelegateFormatter.CreateUntyped(t, resolver)
            elif not t.IsSerializable then 
                raise <| new NonSerializableTypeException(t)
            else
                ClassFormatter.CreateUntyped(t, resolver)
        | Some r -> r