namespace Nessos.FsPickler

    open System
    open System.Collections.Generic
    open System.Collections.Concurrent

    open Nessos.FsPickler.Utils
    open Nessos.FsPickler.TypeShape
    open Nessos.FsPickler.PicklerUtils
    open Nessos.FsPickler.PrimitivePicklers
    open Nessos.FsPickler.ReflectionPicklers
    open Nessos.FsPickler.CombinatorImpls
    open Nessos.FsPickler.TuplePicklers
    open Nessos.FsPickler.PicklerResolution

    [<Sealed>]
    type CustomPicklerRegistry (name : string) =

        let typeNameConverter = ref None : ITypeNameConverter option ref
        let customPicklers = Atom.atom Map.empty<string, Pickler>
        let customPicklerFactories = Atom.atom PicklerFactoryIndex.Empty

        /// register custom type serialization rules; useful for FSI type serializations.
        member __.SetTypeNameConverter tc = typeNameConverter := Some tc

        /// register pickler for a specific type
        member __.RegisterPickler(pickler : Pickler) =
            if pickler.TypeInfo = TypeInfo.Primitive then 
                invalidArg "pickler" "defining custom picklers for primitives not supported."

            customPicklers.Swap(fun fmts -> fmts.AddNoOverwrite(pickler.Type.AssemblyQualifiedName, pickler))

        /// register pluggable pickler factories
        member __.RegisterPicklerFactory(pf : IPicklerFactory) =
            customPicklerFactories.Swap(fun factories -> factories.AddPicklerFactory(pf, Fail))

        member internal __.CustomPicklerFactories = customPicklerFactories.Value

        /// Identifier for the custom registry
        member __.Name = name
        /// registered type name converter, if exists.
        member __.TypeNameConverter = typeNameConverter.Value
        /// list of currently registered custom picklers
        member __.Picklers = customPicklers.Value |> Map.toSeq |> Seq.map snd |> List.ofSeq
        /// list of currently registered custom pickler factories
        member __.PicklerFactories = customPicklerFactories.Value.GetEntries()



    type private PicklerDictionary(cacheId : string) =
        let dict = new ConcurrentDictionary<Type, Exn<Pickler>> ()

        interface ICache<Type, Exn<Pickler>> with
            member __.Lookup(t : Type) =
                let found, p = dict.TryGetValue t
                if found then Some p
                else None

            member __.Commit (t : Type) (p : Exn<Pickler>) =
                let clone (p : Pickler) =
                    // first, create a shallow copy of pickler to contain mutation effects
                    let p = p.Clone()

                    // check cache id for compatibility
                    match p.CacheId with
                    | null -> ()
                    | id when id <> cacheId ->
                        raise <| new PicklerGenerationException(p.ImplementationType, "pickler generated using an incompatible cache.")
                    | _ -> ()

                    // label pickler with current cache id
                    p.CacheId <- cacheId

                    p

                let p = Exn.map clone p

                // commit
                if dict.TryAdd(t, p) then p
                else
                    dict.[t]


    type internal PicklerCache private (uuid : string, name : string,
                                            tyConv : ITypeNameConverter option, 
                                            customPicklers : seq<Pickler>, 
                                            customPicklerFactories : PicklerFactoryIndex) =

        // keep a record of all PicklerCache instances.
        static let caches = Atom.atom Set.empty<string>
        do
            caches.Swap(fun s ->
                if caches.Value.Contains name then
                    invalidOp <| sprintf "A pickler cache with id '%s' has already been initialized." name
                else
                    s.Add name)


        // include default pickler factories
        let customPicklerFactories =
            let defaultFactories = getDefaultPicklerFactories ()
            let tupleFactories = getTuplePicklerFactories ()
            customPicklerFactories.AddPicklerFactories(defaultFactories @ tupleFactories, Discard)

        let reflection = 
#if SERIALIZE_STRONG_NAMES
            new ReflectionManager(true, tyConv)
#else
            new ReflectionManager(false, tyConv)
#endif
        let cache = new PicklerDictionary(uuid) :> ICache<Type, Exn<Pickler>>
        
        // populate the cache
        do
            // add atomic/default picklers
            [|  
                [| CompositePickler.ObjectPickler :> Pickler |]
                PrimitivePicklers.mkAll ()
                reflection.ReflectionPicklers
            |]
            |> Seq.concat
            |> Seq.iter (fun p -> cache.Commit p.Type (Success p) |> ignore)

            // add custom picklers
            for p in customPicklers do
                cache.Commit p.Type (Success p) |> ignore

        let resolver (t : Type) = 
            try YParametric cache (resolvePickler customPicklerFactories) t
            with :? NonSerializableTypeException as e ->
                if e.UnsupportedType = t then reraise ()
                else
                    let msg = sprintf "graph contains nonserializable field '%O'." e.UnsupportedType
                    raise <| new NonSerializableTypeException(t, msg)

        // default cache instance
        static let singleton =
            lazy(new PicklerCache(string Guid.Empty, "default cache instance", None, [], PicklerFactoryIndex.Empty))

        member __.Name = name
        member __.UUId = uuid

        member __.GetQualifiedName(t : Type) = reflection.GetQualifiedName t

        interface IPicklerResolver with
            member r.Resolve<'T> () = resolver typeof<'T> :?> Pickler<'T>
            member r.Resolve (t : Type) = resolver t
        
        static member FromPicklerRegistry(pr : CustomPicklerRegistry) =
            let uuid = string <| Guid.NewGuid()
            new PicklerCache(uuid, pr.Name, pr.TypeNameConverter, pr.Picklers, pr.CustomPicklerFactories)

        static member GetDefaultInstance () = singleton.Value