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
    type CustomPicklerRegistry () =

        let typeNameConverter = ref None : ITypeNameConverter option ref
        let formatters = Atom.atom Map.empty<string, Pickler>
        let genericFactories = Atom.atom GenericPicklerIndex.Empty

        /// register custom type serialization rules; useful for FSI type serializations
        member __.TypeNameConverter = typeNameConverter.Value
        member __.SetTypeNameConverter tc = typeNameConverter := Some tc

        /// register formatter for a specific type
        member __.RegisterPickler(f : Pickler) =
            formatters.Swap(fun fmts -> fmts.AddNoOverwrite(f.Type.AssemblyQualifiedName, f))

        /// register generic formatter rules
        member __.RegisterGenericPickler(gf : IGenericPicklerFactory) =
            genericFactories.Swap(fun genericFactories -> genericFactories.AddGenericPickler(gf, Fail))

        member internal __.GenericFactories = genericFactories.Value
        member __.RegisteredPicklers = formatters.Value |> Map.toSeq |> Seq.map snd |> List.ofSeq
        member __.RegisteredGenericPicklerFactories = genericFactories.Value.GetEntries()


    type internal PicklerCache (tyConv : ITypeNameConverter option, formatters : seq<Pickler>, gfi : GenericPicklerIndex) =

        static let singleton = lazy(new PicklerCache(None, [], GenericPicklerIndex.Empty))

        let tyConv =
            match tyConv with 
            | Some tc -> tc 
            | None -> new DefaultTypeNameConverter() :> _
        
        let cache =
            [|
                mkPrimitivePicklers ()
                mkAtomicPicklers ()
                mkReflectionPicklers tyConv
            |]
            |> Seq.concat
            |> Seq.map (fun f -> KeyValuePair(f.Type, f)) 
            |> fun fs -> new ConcurrentDictionary<_,_>(fs)

        let gfi =
            let fsharpGenericPicklers = mkGenericPicklers ()
            gfi.AddGenericPicklers(fsharpGenericPicklers, Discard)

        do 
            for f in formatters do 
                cache.AddOrUpdate(f.Type, f, fun _ _ -> f) |> ignore

        let resolver (t : Type) = 
            YParametric cache.TryFind (fun t f -> cache.TryAdd(t,f) |> ignore) (resolvePickler gfi) t

        interface IPicklerResolver with
            member s.Resolve<'T> () = resolver typeof<'T> :?> Pickler<'T>
            member s.Resolve (t : Type) = resolver t
        
        static member FromPicklerRegistry(fr : CustomPicklerRegistry) =
            new PicklerCache(fr.TypeNameConverter, fr.RegisteredPicklers, fr.GenericFactories)

        static member GetDefault () = singleton.Value