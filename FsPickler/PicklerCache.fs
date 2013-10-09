namespace FsPickler

    open System
    open System.Collections.Generic
    open System.Collections.Concurrent

    open FsPickler.Utils
    open FsPickler.TypeShape
    open FsPickler.BasePicklers
    open FsPickler.ReflectionPicklers
    open FsPickler.CombinatorImpls
    open FsPickler.PicklerResolution

    [<Sealed>]
    type CustomPicklerRegistry (name : string) =

        let typeNameConverter = ref None : ITypeNameConverter option ref
        let customPicklers = Atom.atom Map.empty<string, Pickler>
        let customPicklerFactories = Atom.atom PicklerFactoryIndex.Empty

        /// registered type name converted, if present.
        member __.TypeNameConverter = typeNameConverter.Value
        /// register custom type serialization rules; useful for FSI type serializations.
        member __.SetTypeNameConverter tc = typeNameConverter := Some tc

        /// register pickler for a specific type
        member __.RegisterPickler(pickler : Pickler) =
            if pickler.TypeKind = TypeKind.Primitive then 
                invalidArg "pickler" "defining custom picklers for primitives not supported."

            customPicklers.Swap(fun fmts -> fmts.AddNoOverwrite(pickler.Type.AssemblyQualifiedName, pickler))

        /// register pluggable pickler factories
        member __.RegisterPicklerFactory(pf : IPicklerFactory) =
            customPicklerFactories.Swap(fun factories -> factories.AddPicklerFactory(pf, Fail))

        member internal __.CustomPicklerFactories = customPicklerFactories.Value

        /// Identifier for the custom registry
        member __.Name = name
        /// list of currently registered custom picklers
        member __.RegisteredPicklers = customPicklers.Value |> Map.toSeq |> Seq.map snd |> List.ofSeq
        /// list of currently registered custom pickler factories
        member __.RegisteredPicklerFactories = customPicklerFactories.Value.GetEntries()


    type internal PicklerCache private (uuid : string, name : string,
                                            tyConv : ITypeNameConverter option, 
                                            customPicklers : seq<Pickler>, 
                                            customPicklerFactories : PicklerFactoryIndex) =

        static let singleton =
            lazy(new PicklerCache(string Guid.Empty, "default cache instance", None, [], PicklerFactoryIndex.Empty))

        // resolve the default type name converter
        let tyConv =
            match tyConv with 
            | Some tc -> tc 
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
            |> Seq.map (fun f -> f.CacheId <- uuid ; f)
            |> Seq.map (fun f -> KeyValuePair(f.Type, f)) 
            |> fun fs -> new ConcurrentDictionary<_,_>(fs)

        do
            // populate cache with custom picklers
            for cp in customPicklers do
                // clone custom pickler to contain sourceId mutation
                let cp' = cp.ClonePickler()
                cp'.CacheId <- uuid
                cache.AddOrUpdate(cp'.Type, cp', fun _ _ -> cp') |> ignore

        let resolver (t : Type) = 
            YParametric uuid 
                        cache.TryFind (fun t f -> cache.TryAdd(t,f) |> ignore) 
                        (resolvePickler customPicklerFactories) t

        member __.Name = name

        interface IPicklerResolver with
            member r.UUId = uuid
            member r.Resolve<'T> () = resolver typeof<'T> :?> Pickler<'T>
            member r.Resolve (t : Type) = resolver t
        
        static member FromPicklerRegistry(pr : CustomPicklerRegistry) =
            let uuid = string <| Guid.NewGuid()
            new PicklerCache(uuid, pr.Name, pr.TypeNameConverter, pr.RegisteredPicklers, pr.CustomPicklerFactories)

        static member GetDefaultInstance () = singleton.Value