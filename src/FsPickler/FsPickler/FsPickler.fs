namespace MBrace.FsPickler

open System
open System.Reflection
open System.Collections.Generic
open System.Runtime.Serialization
open System.Text
    
open MBrace.FsPickler.Hashing

/// FsPickler static methods.
type FsPickler private () =
        
    static let defaultSerializer = lazy(new BinarySerializer())
    static let resolver () = PicklerCache.Instance :> IPicklerResolver

    /// <summary>
    ///     Create a new FsPickler serializer instance that uses the built-in binary format.
    /// </summary>
    /// <param name="forceLittleEndian">Force little-endian encoding in primitive arrays but is slower. Defaults to false.</param>
    /// <param name="tyConv">optional type name converter implementation.</param>
    /// <param name="picklerResolver">Specify a custom pickler resolver/cache for serialization. Defaults to the singleton pickler cache.</param>
    static member CreateBinarySerializer([<O;D(null)>] ?forceLittleEndian : bool, [<O;D(null)>] ?typeConverter : ITypeNameConverter, [<O;D(null)>] ?picklerResolver : IPicklerResolver) = 
        new BinarySerializer(?forceLittleEndian = forceLittleEndian, ?typeConverter = typeConverter, ?picklerResolver = picklerResolver)

    /// <summary>
    ///     Create a new FsPickler serializer instance that uses the XML format.
    /// </summary>
    /// <param name="tyConv">optional type name converter implementation.</param>
    /// <param name="picklerResolver">Specify a custom pickler resolver/cache for serialization. Defaults to the singleton pickler cache.</param>
    static member CreateXmlSerializer([<O;D(null)>]?typeConverter : ITypeNameConverter, [<O;D(null)>]?indent : bool, [<O;D(null)>] ?picklerResolver : IPicklerResolver) = 
        new XmlSerializer(?typeConverter = typeConverter, ?indent = indent, ?picklerResolver = picklerResolver)

    /// <summary>
    ///     Decides if given type is serializable by FsPickler
    /// </summary>
    static member IsSerializableType<'T> () : bool = 
        resolver().IsSerializable<'T> ()

    /// <summary>
    ///     Decides if given type is serializable by FsPickler
    /// </summary>
    /// <param name="t">Type to be checked.</param>
    static member IsSerializableType (t : Type) : bool = 
        resolver().IsSerializable t

    /// <summary>
    ///     Decides if given value is serializable object graph without performing an actual serialization.
    /// </summary>
    /// <param name="graph">Graph to be checked.</param>
    /// <param name="failOnCloneableOnlyTypes">Fail on types that are declared cloneable only. Defaults to true.</param>
    static member IsSerializableValue (graph : 'T, [<O;D(null)>] ?failOnCloneableOnlyTypes : bool) : bool =
        try FsPickler.EnsureSerializable(graph, ?failOnCloneableOnlyTypes = failOnCloneableOnlyTypes) ; true
        with :? FsPicklerException -> false

    /// <summary>
    ///     Auto generates a pickler for given type variable
    /// </summary>
    static member GeneratePickler<'T> () : Pickler<'T> = 
        resolver().Resolve<'T> ()

    /// <summary>
    ///     Auto generates a pickler for given type
    /// </summary>
    /// <param name="t">Type for which pickler will be generated.</param>
    static member GeneratePickler (t : Type) : Pickler = 
        resolver().Resolve t

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
        FsPickler.Clone(value, resolver(), ?pickler = pickler, ?streamingContext = streamingContext)

    /// <summary>
    ///     Performs an in-memory, deep cloning of provided serializable object graph.
    ///     Cloning is performed on a node-to-node basis and does not make use of intermediate
    ///     serialization buffers.
    /// </summary>
    /// <param name="value">Value to be cloned.</param>
    /// <param name="picklerResolver">Specify a custom pickler resolver/cache for serialization.</param>
    /// <param name="pickler">Pickler used for cloning. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    static member Clone<'T> (value : 'T, picklerResolver : IPicklerResolver, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : 'T =
        let pickler = match pickler with None -> picklerResolver.Resolve<'T> () | Some p -> p
        let state = new CloneState(picklerResolver, ?streamingContext = streamingContext)
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
    static member Sift<'T>(value : 'T, sifter : IObjectSifter, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : Sifted<'T> * (int64 * obj) [] =
        FsPickler.Sift(value, sifter, resolver(), ?pickler = pickler, ?streamingContext = streamingContext)

    /// <summary>
    ///     Creates a clone of the provided object graph, sifting objects from the graph as specified by the provided sifter implementation.
    ///     Only reference types can be sifted from a graph.
    /// </summary>
    /// <param name="value">Value to be sifted.</param>
    /// <param name="sifter">Sifting predicate implementation.</param>
    /// <param name="picklerResolver">Specify a custom pickler resolver/cache for serialization.</param>
    /// <param name="pickler">Pickler to be used for traversal. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    /// <returns>A sifted wrapper together with all objects that have been sifted.</returns>
    static member Sift<'T>(value : 'T, sifter : IObjectSifter, picklerResolver : IPicklerResolver, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : Sifted<'T> * (int64 * obj) [] =
        let pickler = match pickler with None -> picklerResolver.Resolve<'T> () | Some p -> p
        let state = new CloneState(picklerResolver, ?streamingContext = streamingContext, sifter = sifter)
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
    static member Sift<'T>(value : 'T, sifter : obj -> bool, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : Sifted<'T> * (int64 * obj) [] =
        FsPickler.Sift(value, sifter, resolver(), ?pickler = pickler, ?streamingContext = streamingContext)

    /// <summary>
    ///     Creates a clone of the provided object graph, sifting objects from the graph as specified by the provided sifter implementation.
    ///     Only reference types can be sifted from a graph.
    /// </summary>
    /// <param name="value">Value to be sifted.</param>
    /// <param name="sifter">Sifting predicate implementation.</param>
    /// <param name="picklerResolver">Specify a custom pickler resolver/cache for serialization.</param>
    /// <param name="pickler">Pickler to be used for traversal. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    /// <returns>A sifted wrapper together with all objects that have been sifted.</returns>
    static member Sift<'T>(value : 'T, sifter : obj -> bool, picklerResolver : IPicklerResolver, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : Sifted<'T> * (int64 * obj) [] =
        let sifter = { new IObjectSifter with member __.Sift(_,_,t) = sifter t }
        FsPickler.Sift(value, sifter, picklerResolver, ?pickler = pickler, ?streamingContext = streamingContext)

    /// <summary>
    ///     Unsifts a provided object graph with given values.
    /// </summary>
    /// <param name="sifted">Sifted object graph to be unsifted.</param>
    /// <param name="values">Values to be pushed in sift holes.</param>
    /// <param name="pickler">Pickler to be used for traversal. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    /// <returns>An unsifted object graph.</returns>
    static member UnSift<'T>(sifted : Sifted<'T>, values:(int64 * obj) [], [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : 'T =
        let pickler = match pickler with None -> resolver().Resolve<'T> () | Some p -> p
        let state = new CloneState(resolver(), ?streamingContext = streamingContext, unSiftData = (values, sifted.SiftedIndices))
        pickler.Clone state sifted.Value

    /// <summary>
    ///     Unsifts a provided object graph with given values.
    /// </summary>
    /// <param name="sifted">Sifted object graph to be unsifted.</param>
    /// <param name="values">Values to be pushed in sift holes.</param>
    /// <param name="picklerResolver">Specify a custom pickler resolver/cache for serialization.</param>
    /// <param name="pickler">Pickler to be used for traversal. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    /// <returns>An unsifted object graph.</returns>
    static member UnSift<'T>(sifted : Sifted<'T>, values:(int64 * obj) [], picklerResolver : IPicklerResolver, [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : 'T =
        let pickler = match pickler with None -> picklerResolver.Resolve<'T> () | Some p -> p
        let state = new CloneState(picklerResolver, ?streamingContext = streamingContext, unSiftData = (values, sifted.SiftedIndices))
        pickler.Clone state sifted.Value

    /// <summary>Compute size in bytes for given input.</summary>
    /// <param name="value">input value.</param>
    /// <param name="pickler">Pickler to be used for size computation. Defaults to auto-generated pickler.</param>
    static member ComputeSize<'T>(value : 'T, [<O;D(null)>] ?pickler : Pickler<'T>) = defaultSerializer.Value.ComputeSize(value, ?pickler = pickler)

    /// <summary>
    ///     Creates a state object used for computing accumulated sizes for multiple objects.
    /// </summary>
    /// <param name="encoding">Text encoding used by the serializer.</param>
    /// <param name="resetInterval">Specifies the serialized object interval after which serialization state will be reset. Defaults to no interval.</param>
    static member CreateObjectSizeCounter([<O;D(null)>] ?encoding : Encoding, [<O;D(null)>] ?resetInterval:int64) : ObjectSizeCounter =
        defaultSerializer.Value.CreateObjectSizeCounter(?encoding = encoding, ?resetInterval = resetInterval)

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
        FsPickler.VisitObject(visitor, graph, resolver(), ?streamingContext = streamingContext, ?visitOrder = visitOrder)

    /// <summary>
    ///     Visits all reference types that appear in the given object graph.
    /// </summary>
    /// <param name="visitor">Visitor implementation.</param>
    /// <param name="graph">Object graph.</param>
    /// <param name="picklerResolver">Specify a custom pickler resolver/cache for serialization.</param>
    /// <param name="pickler">Pickler to be used for traversal. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    /// <param name="visitOrder">Object graph traversal order. Defaults to pre-order traversal.</param>
    static member VisitObject(visitor : IObjectVisitor, graph : 'T, picklerResolver : IPicklerResolver, [<O;D(null)>]?pickler:Pickler<'T>, 
                                [<O;D(null)>]?streamingContext:StreamingContext, [<O;D(null)>]?visitOrder:VisitOrder) =
        let pickler = match pickler with None -> picklerResolver.Resolve<'T> () | Some p -> p
        let state = new VisitState(picklerResolver, visitor, ?streamingContext = streamingContext, ?visitOrder = visitOrder)
        pickler.Accept state graph

    /// <summary>Compute size and hashcode for given input.</summary>
    /// <param name="value">input value.</param>
    /// <param name="hashFactory">the hashing algorithm to be used. MurMur3 by default.</param>
    static member ComputeHash<'T>(value : 'T, [<O;D(null)>]?hashFactory : IHashStreamFactory) =
        defaultSerializer.Value.ComputeHash(value, ?hashFactory = hashFactory)

    /// <summary>
    ///     Uses FsPickler to traverse the object graph, gathering types of objects as it goes.
    /// </summary>
    /// <param name="graph">input object graph.</param>
    static member GatherTypesInObjectGraph(graph : obj) : Type [] =
        FsPickler.GatherTypesInObjectGraph(graph, resolver())

    /// <summary>
    ///     Uses FsPickler to traverse the object graph, gathering types of objects as it goes.
    /// </summary>
    /// <param name="graph">input object graph.</param>
    /// <param name="picklerResolver">Specify a custom pickler resolver/cache for serialization.</param>
    static member GatherTypesInObjectGraph(graph : obj, picklerResolver : IPicklerResolver) : Type [] =
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

        do FsPickler.VisitObject(visitor, graph, picklerResolver)
        gathered |> Seq.toArray

    /// <summary>
    ///     Use FsPickler to traverse the object graph, gathering object instances as it goes.
    /// </summary>
    /// <param name="graph">input object graph.</param>
    static member GatherObjectsInGraph (graph : obj) : obj [] =
        FsPickler.GatherObjectsInGraph(graph, resolver())

    /// <summary>
    ///     Use FsPickler to traverse the object graph, gathering object instances as it goes.
    /// </summary>
    /// <param name="graph">input object graph.</param>
    /// <param name="picklerResolver">Specify a custom pickler resolver/cache for serialization.</param>
    static member GatherObjectsInGraph (graph : obj, picklerResolver : IPicklerResolver) : obj [] =
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

        do FsPickler.VisitObject(visitor, graph, picklerResolver)
        gathered |> Seq.toArray

    /// <summary>
    ///     Traverses the object graph, completing if serializable or raising a serialization exception if not.
    /// </summary>
    /// <param name="graph">Graph to be checked.</param>
    /// <param name="failOnCloneableOnlyTypes">Fail on types that are declared cloneable only. Defaults to true.</param>
    static member EnsureSerializable (graph : 'T, [<O;D(null)>] ?failOnCloneableOnlyTypes : bool) : unit =
        FsPickler.EnsureSerializable(graph, resolver(), ?failOnCloneableOnlyTypes = failOnCloneableOnlyTypes)
    
    /// <summary>
    ///     Traverses the object graph, completing if serializable or raising a serialization exception if not.
    /// </summary>
    /// <param name="graph">Graph to be checked.</param>
    /// <param name="picklerResolver">Specify a custom pickler resolver/cache for serialization.</param>
    /// <param name="failOnCloneableOnlyTypes">Fail on types that are declared cloneable only. Defaults to true.</param>
    static member EnsureSerializable (graph : 'T, picklerResolver : IPicklerResolver, [<O;D(null)>] ?failOnCloneableOnlyTypes : bool) : unit =
        let failOnCloneableOnlyTypes = defaultArg failOnCloneableOnlyTypes true
        let visitor = 
            { new IObjectVisitor with 
                member __.Visit<'T> (p : Pickler<'T>, _ : 'T) : bool = 
                    if failOnCloneableOnlyTypes && p.IsCloneableOnly then
                        raise <| new MBrace.FsPickler.NonSerializableTypeException(typeof<'T>)
                    else
                        true }

        FsPickler.VisitObject(visitor, graph, picklerResolver, visitOrder = VisitOrder.PreOrder)