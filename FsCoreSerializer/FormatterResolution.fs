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
    open FsCoreSerializer.BaseFormatters.Utils
    open FsCoreSerializer.ArrayFormatter
    open FsCoreSerializer.FSharpFormatters

    // Y combinator with parametric recursion

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
                    // recursive operation successful, commit to external state
                    externalCache.TryAdd(x, l.Value) |> ignore
                    l
                | Some l -> l
            | Some b -> lazy b

        (recurse x).Value


    let resolveFormatter (genericIdx : GenericFormatterIndex) (self : Type -> Lazy<Formatter>) (t : Type) =

        // lookup generic shapes
        let result =
            if t.IsArray then Some <| mkArrayFormatter self t
            elif t.IsGenericType || t.IsArray then
                genericIdx.TryResolveGenericFormatter(t, self)
            elif t.IsPointer then
                raise <| new SerializationException("Serializing pointers not supported.")
            elif t.IsByRef then
                raise <| new SerializationException("Serializing ref types not supported.")
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
                elif FSharpType.IsUnion(t, allFlags) then
                    mkUnionFormatter self t |> Some
                elif FSharpType.IsRecord(t, allFlags) then
                    mkRecordFormatter self t |> Some
                elif FSharpType.IsExceptionRepresentation(t, allFlags) then
                    mkExceptionFormatter self t |> Some
                else None

        // subtype resolution
        let result =
            match result with
            | None when t.BaseType <> null ->
                match (self t.BaseType).Value with
                | fmt when fmt.UseWithSubtypes -> Some fmt
                | _ -> None
            | r -> r

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
            | None -> mkReflectionFormatter self t
            | Some r -> r

        result