namespace MBrace.FsPickler

open System
open System.Collections.Generic

/// Abstraction for specifying user-supplied custom picklers
type ICustomPicklerRegistry =
    /// Look up pickler registration for particular type
    abstract GetRegistration : Type -> CustomPicklerRegistration

/// Pickler registration for particular type
and CustomPicklerRegistration =
    /// Pickler not registered for particular type
    | UnRegistered
    /// Declares a type serializable, but uses default pickler generators for it
    | DeclaredSerializable
    /// Declares a custom pickler factor for the given type
    | CustomPickler of factory:(IPicklerResolver -> Pickler)

[<AutoOpen>]
module internal PicklerRegistryExtensions =

    type ICustomPicklerRegistry with
        member __.IsDeclaredSerializable(t : Type) = 
            match __.GetRegistration t with
            | DeclaredSerializable -> true
            | _ -> false

        member __.TryGetPicklerFactory(t : Type) =
            match __.GetRegistration t with
            | CustomPickler f -> Some f
            | _ -> None

/// A pickler registry with no items
[<Sealed; AutoSerializable(false)>]
type EmptyPicklerRegistry() =
    interface ICustomPicklerRegistry with
        member __.GetRegistration _ = UnRegistered

/// Type for appending user-supplied pickler registrations
[<Sealed; AutoSerializable(false)>]
type CustomPicklerRegistry () =
    let syncRoot = obj()
    let mutable isGenerationStarted = false

    let picklerFactories = new Dictionary<Type, IPicklerResolver -> Pickler>()
    let typesDeclaredSerializable = new HashSet<Type>()
    let serializationPredicates = new ResizeArray<Type -> bool>()

    let registerPickler f =
        lock syncRoot (fun () ->
            if not isGenerationStarted then f ()
            else
                invalidOp "this pickler registry is already in use by a serializer, cannot register new instances.")

    let getPicklerRegistration (t : Type) =
        if not isGenerationStarted then lock syncRoot (fun () -> isGenerationStarted <- true)

        let ok, f = picklerFactories.TryGetValue t
        if ok then CustomPickler f 
        elif typesDeclaredSerializable.Contains t then DeclaredSerializable
        elif serializationPredicates |> Seq.exists (fun p -> p t) then DeclaredSerializable
        else UnRegistered

    /// Gets whether isntance is alread being used for pickler generation.
    /// In that case, any attempt to register new types will result in an InvalidOperationException
    member __.IsGenerationStarted = isGenerationStarted
    /// List of all individual types declared serializable
    member __.TypesDeclaredSerializable = typesDeclaredSerializable |> Seq.toArray
    /// List of all individual pickler factory types
    member __.PicklerFactories = picklerFactories.Keys |> Seq.toArray
    /// List of all user-specified custom serialization predicates
    member __.SerializationPredicates = serializationPredicates.ToArray()

    /// Registers a supplied custom pickler
    member __.RegisterPickler(pickler : Pickler) : unit =
        registerPickler (fun () -> picklerFactories.[pickler.Type] <- fun _ -> pickler)

    /// Registers a collections of supplied custom pickelrs
    member __.RegisterPicklers([<ParamArray>] picklers : Pickler[]) : unit =
        for p in picklers do __.RegisterPickler p

    /// Registers a user-specified pickler factory
    member __.RegisterFactory<'T>(factory : IPicklerResolver -> Pickler<'T>) : unit =
        registerPickler (fun () -> picklerFactories.[typeof<'T>] <- fun r -> factory r :> Pickler)

    /// Appends a predicate used to determine whether a specific type should be treated as if carrying the .IsSerializable flag
    member __.DeclareSerializable (isSerializable : Type -> bool) : unit =
        registerPickler (fun () -> serializationPredicates.Add isSerializable)

    /// Appends a list of types that will be treated by the pickler generator as if carrying the .IsSerializable flag
    member __.DeclareSerializable ([<ParamArray>] typesToSerialize : Type[]) : unit =
        registerPickler (fun () -> for t in typesToSerialize do typesDeclaredSerializable.Add t |> ignore)

    /// Registers the specifed type as if carrying the .IsSerializable flag
    member __.DeclareSerializable<'T> () : unit =
        __.DeclareSerializable(typeof<'T>)

    interface ICustomPicklerRegistry with
        member __.GetRegistration t = getPicklerRegistration t