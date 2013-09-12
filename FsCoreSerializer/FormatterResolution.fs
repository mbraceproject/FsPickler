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
    open FsCoreSerializer.BaseFormatters
    open FsCoreSerializer.FormatterUtils
    open FsCoreSerializer.ArrayFormatter
    open FsCoreSerializer.FSharpTypeFormatters

    /// Y combinator with parametric recursion support
    let YParametric (externalCache : ConcurrentDictionary<'a, 'b>) (F : ('a -> Lazy<'b>) -> 'a -> 'b) (x : 'a) =
        // use internal cache to avoid corruption in event of exceptions being raised
        let internalCache = new Dictionary<'a , Lazy<'b>> ()

        let rec recurse (x : 'a) =
            match externalCache.TryFind x with
            | None ->
                match internalCache.TryFind x with
                | None ->
                    let r = ref None
                    let l = lazy (
                        match r.Value with
                        | None -> failwith "attemping to consume at construction time!"
                        | Some v -> v)

                    internalCache.Add(x, l)
                    r := Some (F recurse x)
                    // recursive operation successful, commit to external cache
                    externalCache.TryAdd(x, l.Value) |> ignore
                    l
                | Some l -> l
            | Some b -> lazy b

        (recurse x).Value


    // recursive formatter resolution

    let resolveFormatter (factoryIdx : ConcurrentDictionary<Type, IFormatterFactory>) 
                            (genericIdx : GenericFormatterIndex) 
                            (self : Type -> Lazy<Formatter>) (t : Type) =

        // subtype resolution, must happen before everything else
        let result =
            if t.BaseType <> null then
                match (self t.BaseType).Value with
                | fmt when fmt.UseWithSubtypes -> Some fmt
                | _ -> None
            else
                None

        // lookup factory index
        let result =
            match result with
            | Some _ -> result
            | None ->
                match factoryIdx.TryFind t with
                | Some ff -> 
                    let f = ff.Create self
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
                if t.IsArray then Some <| mkArrayFormatter self t
                elif typeof<System.Delegate>.IsAssignableFrom t then
                    Some <| mkDelegateFormatter t
                elif t.IsGenericType || t.IsArray then
                    genericIdx.TryResolveGenericFormatter(t, self)
                elif t.IsPointer then
                    raise <| new SerializationException("Serializing pointers not supported.")
                elif t.IsByRef then
                    raise <| new SerializationException("Serializing refs not supported.")
                elif t.IsEnum then
                    Some <| mkEnumFormatter self t
                else None

        // lookup F# types
        let result =
            match result with
            | Some _ -> result
            | None ->
                if FSharpType.IsTuple t then
                    mkTupleFormatter self t |> Some
                elif FSharpType.IsUnion(t, memberBindings) then
                    mkUnionFormatter self t |> Some
                elif FSharpType.IsRecord(t, memberBindings) then
                    mkRecordFormatter self t |> Some
#if EMIT_IL
                elif FSharpType.IsExceptionRepresentation(t, memberBindings) then
                    mkExceptionFormatter self t |> Some
#endif
                else None

        // IFsCoreSerializable resolution
        let result =
            match result with
            | None -> tryMkIFsCoreSerializable t
            | Some _ -> result

        // .NET ISerializable resolution
        let result =
            match result with
            | None -> tryMkISerializableFormatter t
            | Some _ -> result

        // .NET reflection serialization
        let result =
            match result with
            | None ->
                if t.IsValueType then mkStructFormatter self t
                elif t.IsAbstract then mkAbstractFormatter t
                elif not t.IsSerializable then 
                    raise <| new SerializationException(sprintf "type '%s' is not serializable." t.Name)
                else
                    mkClassFormatter self t
            | Some r -> r

        result