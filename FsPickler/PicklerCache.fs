namespace FsPickler

    open System
    open System.Collections.Generic
    open System.Collections.Concurrent

    open FsPickler.Utils
    open FsPickler.TypeShape
    open FsPickler.BasePicklers
    open FsPickler.CombinatorImpls
    open FsPickler.PicklerResolution

    [<Sealed>]
    type CustomPicklerRegistry (?id : string) =
        let id = match id with None -> string <| Guid.NewGuid() | Some id -> id

        let typeNameConverter = ref None : ITypeNameConverter option ref
        let customPicklers = Atom.atom Map.empty<string, Pickler>
        let customPicklerFactories = Atom.atom PicklerFactoryIndex.Empty

        /// registered type name converted, if present.
        member __.TypeNameConverter = typeNameConverter.Value
        /// register custom type serialization rules; useful for FSI type serializations.
        member __.SetTypeNameConverter tc = typeNameConverter := Some tc

        /// register pickler for a specific type
        member __.RegisterPickler(f : Pickler) =
            customPicklers.Swap(fun fmts -> fmts.AddNoOverwrite(f.Type.AssemblyQualifiedName, f))

        /// register pluggable pickler factories
        member __.RegisterPicklerFactory(pf : IPicklerFactory) =
            customPicklerFactories.Swap(fun factories -> factories.AddPicklerFactory(pf, Fail))

        member internal __.CustomPicklerFactories = customPicklerFactories.Value
        member internal __.CustomPicklers = customPicklers.Value

        /// Identifier for the current registry
        member __.Id = id
        /// list of currently registered custom picklers
        member __.RegisteredPicklers = customPicklers.Value |> Map.toSeq |> Seq.map snd |> List.ofSeq
        /// list of currently registered custom pickler factories
        member __.RegisteredPicklerFactories = customPicklerFactories.Value.GetEntries()


    type internal PicklerCache private (id : string, 
                                            tyConv : ITypeNameConverter option, 
                                            customPicklers : Map<string, Pickler>, 
                                            customPicklerFactories : PicklerFactoryIndex) =

        static let singleton = lazy(
            let id = "default pickler cache"
            new PicklerCache(id, None, Map.empty, PicklerFactoryIndex.Empty))

        // resolve the default type name converter
        let tyConv =
            match tyConv with 
            | Some tc -> tc 
            | None when runsOnMono -> new MonoTypeNameConverter() :> _
            | None -> new DefaultTypeNameConverter() :> _

        // include default pickler factories
        let customPicklerFactories =
            let defaultFactories = getDefaultPicklerFactories ()
            customPicklerFactories.AddPicklerFactories(defaultFactories, Discard)
        
        // populate initial cache with primitives
        let cache =
            [|
                mkAtomicPicklers ()
                mkReflectionPicklers tyConv
            |]
            |> Seq.concat
            |> Seq.map (fun f -> f.ResolverName <- id ; f)
            |> Seq.map (fun f -> KeyValuePair(f.Type, f)) 
            |> fun fs -> new ConcurrentDictionary<_,_>(fs)

        let resolver (t : Type) = 
            YParametric id 
                        cache.TryFind (fun t f -> cache.TryAdd(t,f) |> ignore) 
                        (resolvePickler customPicklers customPicklerFactories) t

        interface IPicklerResolver with
            member r.Id = id
            member r.Resolve<'T> () = resolver typeof<'T> :?> Pickler<'T>
            member r.Resolve (t : Type) = resolver t
        
        static member FromPicklerRegistry(pr : CustomPicklerRegistry) =
            let uuid = string <| Guid.NewGuid ()
            new PicklerCache(uuid, pr.TypeNameConverter, pr.CustomPicklers, pr.CustomPicklerFactories)

        static member GetDefaultInstance () = singleton.Value