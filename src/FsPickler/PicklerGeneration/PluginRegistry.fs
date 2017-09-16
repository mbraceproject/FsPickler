namespace MBrace.FsPickler

open System
open System.Collections.Concurrent

// Defines a registry for providing user-supplied runtime pickler declarations

type private PicklerPlugin =
    /// Treat type as if carrying SerializableAttribute
    | DeclareSerializable
    /// Registered pickler factory instance
    | Factory of obj

// PicklerPluginRegistry is internal, so we need a public type to pass it through from PicklerContext to FsPicklerSerializer
type IPicklerPluginRegistry = interface end

type internal PicklerPluginRegistry () =
    
    static let defaultInstance = PicklerPluginRegistry()

    let registry = new ConcurrentDictionary<Type, PicklerPlugin> ()
    let serializablePredicates = ref []

    interface IPicklerPluginRegistry

    member self.RegisterFactory<'T>(factory : IPicklerResolver -> Pickler<'T>) =
        registry.TryAdd(typeof<'T>, Factory (factory :> obj))

    member self.DeclareSerializable (t : Type) =
        registry.TryAdd(t, DeclareSerializable)

    member self.DeclareSerializable (predicate : Type -> bool) =
        lock serializablePredicates 
            (fun () -> serializablePredicates := predicate :: !serializablePredicates)

    member self.ContainsFactory(t : Type) = 
        match registry.TryFind t with
        | Some(Factory _) -> true
        | _ -> false

    member self.IsDeclaredSerializable(t : Type) =
        match registry.TryFind t with
        | Some DeclareSerializable -> true
        | _ -> !serializablePredicates |> List.exists (fun pred -> pred t)

    member self.GetPicklerFactory<'T> () : IPicklerResolver -> Pickler<'T> =
        match registry.[typeof<'T>] with
        | Factory o -> o :?> IPicklerResolver -> Pickler<'T>
        | _ -> invalidOp "internal error: not a pickler factory."

    static member Default = defaultInstance