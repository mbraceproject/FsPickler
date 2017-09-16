namespace MBrace.FsPickler

open System
open System.Reflection
open System.Collections.Generic
open System.Runtime.Serialization
open System.Text
    
open MBrace.FsPickler.Hashing


type PicklerContext internal (defaultSerializer:FsPicklerSerializer, resolver:IPicklerResolver, cache:PicklerCache, registry:PicklerPluginRegistry) =

    new (?defaultSerializer:FsPicklerSerializer) =
            let registry = PicklerPluginRegistry()
            let defaultSerializer =
                match defaultSerializer with
                | Some s -> s
                | None -> BinarySerializer(registry=(registry :> IPicklerPluginRegistry)) :> FsPicklerSerializer
            let cache = PicklerCache(registry)
            PicklerContext(defaultSerializer, cache :> IPicklerResolver, cache, registry)

    member self.Registry : IPicklerPluginRegistry = upcast registry

    /// <summary>
    ///     Create a new FsPickler serializer instance that uses the built-in binary format.
    /// </summary>
    /// <param name="forceLittleEndian">Force little-endian encoding in primitive arrays but is slower. Defaults to false.</param>
    /// <param name="tyConv">optional type name converter implementation.</param>
    member self.CreateBinarySerializer([<O;D(null)>] ?forceLittleEndian : bool, [<O;D(null)>] ?typeConverter : ITypeNameConverter) = 
        new BinarySerializer(self.Registry, ?forceLittleEndian = forceLittleEndian, ?typeConverter = typeConverter)

    /// <summary>
    ///     Create a new FsPickler serializer instance that uses the XML format.
    /// </summary>
    /// <param name="tyConv">optional type name converter implementation.</param>
    member self.CreateXmlSerializer([<O;D(null)>]?typeConverter : ITypeNameConverter, [<O;D(null)>]?indent : bool) = 
        new XmlSerializer(self.Registry, ?typeConverter = typeConverter, ?indent = indent)

    /// Decides if given type is serializable by FsPickler
    member self.IsSerializableType<'T> () : bool = 
        resolver.IsSerializable<'T> ()

    /// Decides if given type is serializable by FsPickler
    member self.IsSerializableType (t : Type) : bool = 
        resolver.IsSerializable t

    /// <summary>
    ///     Decides if given value is serializable object graph without performing an actual serialization.
    /// </summary>
    /// <param name="graph">Graph to be checked.</param>
    /// <param name="failOnCloneableOnlyTypes">Fail on types that are declared cloneable only. Defaults to true.</param>
    member self.IsSerializableValue (graph : 'T, [<O;D(null)>] ?failOnCloneableOnlyTypes : bool) : bool =
        try self.EnsureSerializable(graph, ?failOnCloneableOnlyTypes = failOnCloneableOnlyTypes) ; true
        with :? FsPicklerException -> false

    /// Auto generates a pickler for given type variable
    member self.GeneratePickler<'T> () : Pickler<'T> = 
        resolver.Resolve<'T> ()
        
    /// Auto generates a pickler for given type
    member self.GeneratePickler (t : Type) : Pickler = 
        resolver.Resolve t

    /// <summary>
    ///     Registers a pickler factory for use by the pickler generation mechanism.
    ///     Factories can only be registered before any serializations take place.
    /// </summary>
    /// <param name="factory">Pickler factory instance.</param>
    member self.RegisterPicklerFactory<'T>(factory : IPicklerResolver -> Pickler<'T>) : unit =
        cache.WithLockedCache (fun () ->
            if cache.IsPicklerGenerated typeof<'T> then
                invalidOp <| sprintf "A pickler for type '%O' has already been generated." typeof<'T>

            let success = registry.RegisterFactory factory
            if not success then
                invalidOp <| sprintf "A pickler plugin for type '%O' has already been registered." typeof<'T>)

    /// <summary>
    ///     Registers a pickler instance for use by the pickler generation mechanism.
    ///     Picklers can only be registered before any serializations take place.
    /// </summary>
    /// <param name="Pickler">Pickler instance.</param>
    member self.RegisterPickler<'T>(pickler : Pickler<'T>) : unit =
        self.RegisterPicklerFactory(fun _ -> pickler)

    /// <summary>
    ///     Declares that supplied type should be treated as serializable.
    ///     This is equivalent to dynamically attaching a SerializableAttribute to the type.
    /// </summary>
    member self.DeclareSerializable<'T> () : unit =
        self.DeclareSerializable typeof<'T>

    /// <summary>
    ///     Declares that supplied type should be treated as serializable.
    ///     This is equivalent to dynamically attaching a SerializableAttribute to the type.
    /// </summary>
    member self.DeclareSerializable (t : Type) : unit =
        cache.WithLockedCache(fun () -> 
            if cache.IsPicklerGenerated t then
                invalidOp <| sprintf "A pickler for type '%O' has already been generated." t

            let success = registry.DeclareSerializable t
            if not success then
                invalidOp <| sprintf "A pickler plugin for type '%O' has already been registered." t)

    /// <summary>
    ///     Declares that types satisfying the provided predicate should be treated by
    ///     FsPickler as carrying the Serializable attribute.
    /// </summary>
    /// <param name="predicate">Serializable type predicate.</param>
    member self.DeclareSerializable (predicate : Type -> bool) : unit =
        registry.DeclareSerializable predicate

    //
    // Misc utils
    //

    /// <summary>
    ///     Performs an in-memory, deep cloning of provided serializable object graph.
    ///     Cloning is performed on a node-to-node basis and does not make use of intermediate
    ///     serialization buffers.
    /// </summary>
    /// <param name="value">Value to be cloned.</param>
    /// <param name="pickler">Pickler used for cloning. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    member self.Clone<'T> (value : 'T, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : 'T =
        let pickler = match pickler with None -> resolver.Resolve<'T> () | Some p -> p
        let state = new CloneState(resolver, ?streamingContext = streamingContext)
        pickler.Clone state value

    /// <summary>
    ///     Creates a clone of the provided object graph, sifting objects from the graph as specified by the provided sifter implementation.
    ///     Only reference types can be sifted from a graph.
    /// </summary>
    /// <param name="value">Value to be sifted.</param>
    /// <param name="sifter">Sifting predicate implementation.</param>
    /// <param name="pickler">Pickler to be used for traversal. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    /// <returns>A sifted wrapper together with all objects that have been sifted.</returns>
    member self.Sift<'T>(value : 'T, sifter : IObjectSifter, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : Sifted<'T> * (int64 * obj) [] =
        let pickler = match pickler with None -> resolver.Resolve<'T> () | Some p -> p
        let state = new CloneState(resolver, ?streamingContext = streamingContext, sifter = sifter)
        let sifted = pickler.Clone state value
        state.CreateSift(sifted)

    /// <summary>
    ///     Creates a clone of the provided object graph, sifting objects from the graph as specified by the provided sifter implementation.
    ///     Only reference types can be sifted from a graph.
    /// </summary>
    /// <param name="value">Value to be sifted.</param>
    /// <param name="sifter">Sifting predicate implementation.</param>
    /// <param name="pickler">Pickler to be used for traversal. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    /// <returns>A sifted wrapper together with all objects that have been sifted.</returns>
    member self.Sift<'T>(value : 'T, sifter : obj -> bool, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : Sifted<'T> * (int64 * obj) [] =
        let sifter = { new IObjectSifter with member __.Sift(_,_,t) = sifter t }
        self.Sift(value, sifter, ?pickler = pickler, ?streamingContext = streamingContext)

    /// <summary>
    ///     Unsifts a provided object graph with given values.
    /// </summary>
    /// <param name="sifted">Sifted object graph to be unsifted.</param>
    /// <param name="values">Values to be pushed in sift holes.</param>
    /// <param name="pickler">Pickler to be used for traversal. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    /// <returns>An unsifted object graph.</returns>
    member self.UnSift<'T>(sifted : Sifted<'T>, values:(int64 * obj) [], [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : 'T =
        let pickler = match pickler with None -> resolver.Resolve<'T> () | Some p -> p
        let state = new CloneState(resolver, ?streamingContext = streamingContext, unSiftData = (values, sifted.SiftedIndices))
        pickler.Clone state sifted.Value

    /// <summary>Compute size in bytes for given input.</summary>
    /// <param name="value">input value.</param>
    /// <param name="pickler">Pickler to be used for size computation. Defaults to auto-generated pickler.</param>
    member self.ComputeSize<'T>(value : 'T, [<O;D(null)>] ?pickler : Pickler<'T>) = defaultSerializer.ComputeSize(value, ?pickler = pickler)

    /// <summary>
    ///     Creates a state object used for computing accumulated sizes for multiple objects.
    /// </summary>
    /// <param name="encoding">Text encoding used by the serializer.</param>
    /// <param name="resetInterval">Specifies the serialized object interval after which serialization state will be reset. Defaults to no interval.</param>
    member self.CreateObjectSizeCounter([<O;D(null)>] ?encoding : Encoding, [<O;D(null)>] ?resetInterval:int64) : ObjectSizeCounter =
        defaultSerializer.CreateObjectSizeCounter(?encoding = encoding, ?resetInterval = resetInterval)

    /// <summary>
    ///     Visits all reference types that appear in the given object graph.
    /// </summary>
    /// <param name="visitor">Visitor implementation.</param>
    /// <param name="graph">Object graph.</param>
    /// <param name="pickler">Pickler to be used for traversal. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    /// <param name="visitOrder">Object graph traversal order. Defaults to pre-order traversal.</param>
    member self.VisitObject(visitor : IObjectVisitor, graph : 'T, [<O;D(null)>]?pickler:Pickler<'T>, 
                                [<O;D(null)>]?streamingContext:StreamingContext, [<O;D(null)>]?visitOrder:VisitOrder) =

        let resolver = resolver
        let pickler = match pickler with None -> resolver.Resolve<'T> () | Some p -> p
        let state = new VisitState(resolver, visitor, ?streamingContext = streamingContext, ?visitOrder = visitOrder)
        pickler.Accept state graph

    /// <summary>Compute size and hashcode for given input.</summary>
    /// <param name="value">input value.</param>
    /// <param name="hashFactory">the hashing algorithm to be used. MurMur3 by default.</param>
    member self.ComputeHash<'T>(value : 'T, [<O;D(null)>]?hashFactory : IHashStreamFactory) =
        defaultSerializer.ComputeHash(value, ?hashFactory = hashFactory)

    /// <summary>
    ///     Uses FsPickler to traverse the object graph, gathering types of objects as it goes.
    /// </summary>
    /// <param name="graph">input object graph.</param>
    member self.GatherTypesInObjectGraph(graph : obj) : Type [] =
        let gathered = new HashSet<Type> ()
        let visitor =
            {
                new IObjectVisitor with
                    member __.Visit (p, value : 'T) =
                        // avoid boxing value types
                        if p.Kind <= Kind.Value then 
                            match value.GetType() with
                            | null -> ()
                            | t -> gathered.Add t |> ignore
                        else
                            match box value with
                            | null -> ()
                            | :? Type as t -> gathered.Add t |> ignore
                            | :? MemberInfo as m when isNotNull m.DeclaringType ->
                                gathered.Add m.DeclaringType |> ignore

                            | value -> 
                                match value.GetType() with
                                | null -> ()
                                | t -> gathered.Add t |> ignore

                        true // always continue traversal
            }

        do self.VisitObject(visitor, graph)
        gathered |> Seq.toArray

    /// <summary>
    ///     Use FsPickler to traverse the object graph, gathering object instances as it goes.
    /// </summary>
    /// <param name="graph">input object graph.</param>
    member self.GatherObjectsInGraph (graph : obj) : obj [] =
        let gathered = new HashSet<obj> ()
        let visitor =
            {
                new IObjectVisitor with
                    member __.Visit (p : Pickler<'T>, value : 'T) =
                        if p.Kind <= Kind.Value then () 
                        else
                            match box value with
                            | null -> ()
                            | v -> gathered.Add v |> ignore

                        true // continue traversal
            }

        do self.VisitObject(visitor, graph)
        gathered |> Seq.toArray


    /// <summary>
    ///     Traverses the object graph, completing if serializable or raising a serialization exception if not.
    /// </summary>
    /// <param name="graph">Graph to be checked.</param>
    /// <param name="failOnCloneableOnlyTypes">Fail on types that are declared cloneable only. Defaults to true.</param>
    member self.EnsureSerializable (graph : 'T, [<O;D(null)>] ?failOnCloneableOnlyTypes : bool) : unit =
        let failOnCloneableOnlyTypes = defaultArg failOnCloneableOnlyTypes true
        let visitor = 
            { new IObjectVisitor with 
                member __.Visit<'T> (p : Pickler<'T>, _ : 'T) : bool = 
                    if failOnCloneableOnlyTypes && p.IsCloneableOnly then
                        raise <| new MBrace.FsPickler.NonSerializableTypeException(typeof<'T>)
                    else
                        true }

        self.VisitObject(visitor, graph, visitOrder = VisitOrder.PreOrder)


/// FsPickler static methods.
type FsPickler private () =
    
    static let defaultContext = lazy(
        let cache = PicklerCache(PicklerPluginRegistry.Default)
        PicklerContext(BinarySerializer(), cache, cache, PicklerPluginRegistry.Default)
    )

    /// <summary>
    ///     Create a new FsPickler serializer instance that uses the built-in binary format.
    /// </summary>
    /// <param name="forceLittleEndian">Force little-endian encoding in primitive arrays but is slower. Defaults to false.</param>
    /// <param name="tyConv">optional type name converter implementation.</param>
    static member CreateBinarySerializer([<O;D(null)>] ?forceLittleEndian : bool, [<O;D(null)>] ?typeConverter : ITypeNameConverter) = 
        defaultContext.Value.CreateBinarySerializer(?forceLittleEndian=forceLittleEndian, ?typeConverter=typeConverter)

    /// <summary>
    ///     Create a new FsPickler serializer instance that uses the XML format.
    /// </summary>
    /// <param name="tyConv">optional type name converter implementation.</param>
    static member CreateXmlSerializer([<O;D(null)>]?typeConverter : ITypeNameConverter, [<O;D(null)>]?indent : bool) = 
        defaultContext.Value.CreateXmlSerializer(?typeConverter=typeConverter, ?indent=indent)

    /// Decides if given type is serializable by FsPickler
    static member IsSerializableType<'T> () : bool = 
        defaultContext.Value.IsSerializableType<'T>()

    /// Decides if given type is serializable by FsPickler
    static member IsSerializableType (t : Type) : bool = 
        defaultContext.Value.IsSerializableType(t)

    /// <summary>
    ///     Decides if given value is serializable object graph without performing an actual serialization.
    /// </summary>
    /// <param name="graph">Graph to be checked.</param>
    /// <param name="failOnCloneableOnlyTypes">Fail on types that are declared cloneable only. Defaults to true.</param>
    static member IsSerializableValue (graph : 'T, [<O;D(null)>] ?failOnCloneableOnlyTypes : bool) : bool =
        defaultContext.Value.IsSerializableValue(graph, ?failOnCloneableOnlyTypes=failOnCloneableOnlyTypes)

    /// Auto generates a pickler for given type variable
    static member GeneratePickler<'T> () : Pickler<'T> = 
        defaultContext.Value.GeneratePickler<'T>()
        
    /// Auto generates a pickler for given type
    static member GeneratePickler (t : Type) : Pickler = 
        defaultContext.Value.GeneratePickler(t)

    /// <summary>
    ///     Registers a pickler factory for use by the pickler generation mechanism.
    ///     Factories can only be registered before any serializations take place.
    /// </summary>
    /// <param name="factory">Pickler factory instance.</param>
    static member RegisterPicklerFactory<'T>(factory : IPicklerResolver -> Pickler<'T>) : unit =
        defaultContext.Value.RegisterPicklerFactory(factory)

    /// <summary>
    ///     Registers a pickler instance for use by the pickler generation mechanism.
    ///     Picklers can only be registered before any serializations take place.
    /// </summary>
    /// <param name="Pickler">Pickler instance.</param>
    static member RegisterPickler<'T>(pickler : Pickler<'T>) : unit =
        defaultContext.Value.RegisterPickler(pickler)

    /// <summary>
    ///     Declares that supplied type should be treated as serializable.
    ///     This is equivalent to dynamically attaching a SerializableAttribute to the type.
    /// </summary>
    static member DeclareSerializable<'T> () : unit =
        defaultContext.Value.DeclareSerializable<'T>()

    /// <summary>
    ///     Declares that supplied type should be treated as serializable.
    ///     This is equivalent to dynamically attaching a SerializableAttribute to the type.
    /// </summary>
    static member DeclareSerializable (t : Type) : unit =
        defaultContext.Value.DeclareSerializable(t)

    /// <summary>
    ///     Declares that types satisfying the provided predicate should be treated by
    ///     FsPickler as carrying the Serializable attribute.
    /// </summary>
    /// <param name="predicate">Serializable type predicate.</param>
    static member DeclareSerializable (predicate : Type -> bool) : unit =
        defaultContext.Value.DeclareSerializable(predicate)

    //
    // Misc utils
    //

    /// <summary>
    ///     Performs an in-memory, deep cloning of provided serializable object graph.
    ///     Cloning is performed on a node-to-node basis and does not make use of intermediate
    ///     serialization buffers.
    /// </summary>
    /// <param name="value">Value to be cloned.</param>
    /// <param name="pickler">Pickler used for cloning. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    static member Clone<'T> (value : 'T, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : 'T =
        defaultContext.Value.Clone(value, ?pickler=pickler, ?streamingContext=streamingContext)

    /// <summary>
    ///     Creates a clone of the provided object graph, sifting objects from the graph as specified by the provided sifter implementation.
    ///     Only reference types can be sifted from a graph.
    /// </summary>
    /// <param name="value">Value to be sifted.</param>
    /// <param name="sifter">Sifting predicate implementation.</param>
    /// <param name="pickler">Pickler to be used for traversal. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    /// <returns>A sifted wrapper together with all objects that have been sifted.</returns>
    static member Sift<'T>(value : 'T, sifter : IObjectSifter, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : Sifted<'T> * (int64 * obj) [] =
        defaultContext.Value.Sift(value, sifter, ?pickler=pickler, ?streamingContext=streamingContext)

    /// <summary>
    ///     Creates a clone of the provided object graph, sifting objects from the graph as specified by the provided sifter implementation.
    ///     Only reference types can be sifted from a graph.
    /// </summary>
    /// <param name="value">Value to be sifted.</param>
    /// <param name="sifter">Sifting predicate implementation.</param>
    /// <param name="pickler">Pickler to be used for traversal. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    /// <returns>A sifted wrapper together with all objects that have been sifted.</returns>
    static member Sift<'T>(value : 'T, sifter : obj -> bool, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : Sifted<'T> * (int64 * obj) [] =
        defaultContext.Value.Sift(value, sifter, ?pickler=pickler, ?streamingContext=streamingContext)

    /// <summary>
    ///     Unsifts a provided object graph with given values.
    /// </summary>
    /// <param name="sifted">Sifted object graph to be unsifted.</param>
    /// <param name="values">Values to be pushed in sift holes.</param>
    /// <param name="pickler">Pickler to be used for traversal. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    /// <returns>An unsifted object graph.</returns>
    static member UnSift<'T>(sifted : Sifted<'T>, values:(int64 * obj) [], [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : 'T =
        defaultContext.Value.UnSift(sifted, values, ?pickler=pickler, ?streamingContext=streamingContext)

    /// <summary>Compute size in bytes for given input.</summary>
    /// <param name="value">input value.</param>
    /// <param name="pickler">Pickler to be used for size computation. Defaults to auto-generated pickler.</param>
    static member ComputeSize<'T>(value : 'T, [<O;D(null)>] ?pickler : Pickler<'T>) =
        defaultContext.Value.ComputeSize(value, ?pickler=pickler)

    /// <summary>
    ///     Creates a state object used for computing accumulated sizes for multiple objects.
    /// </summary>
    /// <param name="encoding">Text encoding used by the serializer.</param>
    /// <param name="resetInterval">Specifies the serialized object interval after which serialization state will be reset. Defaults to no interval.</param>
    static member CreateObjectSizeCounter([<O;D(null)>] ?encoding : Encoding, [<O;D(null)>] ?resetInterval:int64) : ObjectSizeCounter =
        defaultContext.Value.CreateObjectSizeCounter(?encoding=encoding, ?resetInterval=resetInterval)

    /// <summary>
    ///     Visits all reference types that appear in the given object graph.
    /// </summary>
    /// <param name="visitor">Visitor implementation.</param>
    /// <param name="graph">Object graph.</param>
    /// <param name="pickler">Pickler to be used for traversal. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    /// <param name="visitOrder">Object graph traversal order. Defaults to pre-order traversal.</param>
    static member VisitObject(visitor : IObjectVisitor, graph : 'T, [<O;D(null)>]?pickler:Pickler<'T>,
                                [<O;D(null)>]?streamingContext:StreamingContext, [<O;D(null)>]?visitOrder:VisitOrder) =
        defaultContext.Value.VisitObject(visitor, graph, ?pickler=pickler, ?streamingContext=streamingContext, ?visitOrder=visitOrder)

    /// <summary>Compute size and hashcode for given input.</summary>
    /// <param name="value">input value.</param>
    /// <param name="hashFactory">the hashing algorithm to be used. MurMur3 by default.</param>
    static member ComputeHash<'T>(value : 'T, [<O;D(null)>]?hashFactory : IHashStreamFactory) =
        defaultContext.Value.ComputeHash(value, ?hashFactory=hashFactory)

    /// <summary>
    ///     Uses FsPickler to traverse the object graph, gathering types of objects as it goes.
    /// </summary>
    /// <param name="graph">input object graph.</param>
    static member GatherTypesInObjectGraph(graph : obj) : Type [] =
        defaultContext.Value.GatherTypesInObjectGraph(graph)

    /// <summary>
    ///     Use FsPickler to traverse the object graph, gathering object instances as it goes.
    /// </summary>
    /// <param name="graph">input object graph.</param>
    static member GatherObjectsInGraph (graph : obj) : obj [] =
        defaultContext.Value.GatherObjectsInGraph(graph)


    /// <summary>
    ///     Traverses the object graph, completing if serializable or raising a serialization exception if not.
    /// </summary>
    /// <param name="graph">Graph to be checked.</param>
    /// <param name="failOnCloneableOnlyTypes">Fail on types that are declared cloneable only. Defaults to true.</param>
    static member EnsureSerializable (graph : 'T, [<O;D(null)>] ?failOnCloneableOnlyTypes : bool) : unit =
        defaultContext.Value.EnsureSerializable(graph, ?failOnCloneableOnlyTypes=failOnCloneableOnlyTypes)