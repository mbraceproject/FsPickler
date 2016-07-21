namespace MBrace.FsPickler

open System
open System.Collections.Concurrent

// Defines a registry for providing user-supplied runtime pickler declarations

type private PicklerPlugin =
    /// Treat type as if carrying SerializableAttribute
    | DeclareSerializable
    /// Registered pickler factory instance
    | Factory of obj

type internal PicklerPluginRegistry private () =
    static let registry = new ConcurrentDictionary<Type, PicklerPlugin> ()
    static let serializablePredicates = ref []

    static member RegisterFactory<'T>(factory : IPicklerResolver -> Pickler<'T>) =
        registry.TryAdd(typeof<'T>, Factory (factory :> obj))

    static member DeclareSerializable (t : Type) =
        registry.TryAdd(t, DeclareSerializable)

    static member DeclareSerializable (predicate : Type -> bool) =
        lock serializablePredicates 
            (fun () -> serializablePredicates := predicate :: !serializablePredicates)

    static member ContainsFactory(t : Type) = 
        match registry.TryFind t with
        | Some(Factory _) -> true
        | _ -> false

    static member IsDeclaredSerializable(t : Type) =
        match registry.TryFind t with
        | Some DeclareSerializable -> true
        | _ -> !serializablePredicates |> List.exists (fun pred -> pred t)

    static member GetPicklerFactory<'T> () : IPicklerResolver -> Pickler<'T> =
        match registry.[typeof<'T>] with
        | Factory o -> o :?> IPicklerResolver -> Pickler<'T>
        | _ -> invalidOp "internal error: not a pickler factory."