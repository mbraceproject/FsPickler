namespace Nessos.FsPickler

open System
open System.Reflection
open System.Collections.Generic
open System.Runtime.Serialization
    
open Nessos.FsPickler.Hashing

/// FsPickler static methods.
type FsPickler private () =
        
    static let defaultSerializer = lazy(new BinarySerializer())
    static let resolver = lazy(PicklerCache.Instance :> IPicklerResolver)

    /// <summary>
    ///     Create a new binary pickler instance.
    /// </summary>
    /// <param name="forceLittleEndian">Force little-endian encoding in primitive arrays but is slower. Defaults to false.</param>
    /// <param name="tyConv">optional type name converter implementation.</param>
    static member CreateBinary([<O;D(null)>] ?forceLittleEndian : bool, [<O;D(null)>] ?typeConverter : ITypeNameConverter) = 
        new BinarySerializer(?forceLittleEndian = forceLittleEndian, ?typeConverter = typeConverter)

    /// <summary>
    ///     Create a new XML pickler instance.
    /// </summary>
    /// <param name="tyConv">optional type name converter implementation.</param>
    static member CreateXml([<O;D(null)>]?typeConverter : ITypeNameConverter, [<O;D(null)>]?indent : bool) = 
        new XmlSerializer(?typeConverter = typeConverter, ?indent = indent)

    /// Decides if given type is serializable by FsPickler
    static member IsSerializableType (t : Type) = 
        resolver.Value.IsSerializable t

    /// Decides if given type is serializable by FsPickler
    static member IsSerializableType<'T> () = 
        resolver.Value.IsSerializable<'T> ()

    /// Auto generates a pickler for given type variable
    static member GeneratePickler<'T> () = 
        resolver.Value.Resolve<'T> ()
        
    /// Auto generates a pickler for given type
    static member GeneratePickler (t : Type) = 
        resolver.Value.Resolve t

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
    static member Clone<'T> (value : 'T,  [<O;D(null)>]?pickler : Pickler<'T>,  [<O;D(null)>]?streamingContext : StreamingContext) : 'T =
        let pickler = match pickler with None -> resolver.Value.Resolve<'T> () | Some p -> p
        let state = new CloneState(resolver.Value, ?streamingContext = streamingContext)
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
    static member Sift<'T>(value : 'T, sifter : IObjectSifter,  [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : Sifted<'T> * (int64 * obj) [] =
        let pickler = match pickler with None -> resolver.Value.Resolve<'T> () | Some p -> p
        let state = new CloneState(resolver.Value, ?streamingContext = streamingContext, sifter = sifter)
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
        let sifter = { new IObjectSifter with member __.Sift(_,t) = sifter t }
        FsPickler.Sift(value, sifter, ?pickler = pickler, ?streamingContext = streamingContext)

    /// <summary>
    ///     Unsifts a provided object graph with given values.
    /// </summary>
    /// <param name="sifted">Sifted object graph to be unsifted.</param>
    /// <param name="values">Values to be pushed in sift holes.</param>
    /// <param name="pickler">Pickler to be used for traversal. Defaults to auto-generated pickler.</param>
    /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
    /// <returns>An unsifted object graph.</returns>
    static member UnSift<'T>(sifted : Sifted<'T>, values:(int64 * obj) [], [<O;D(null)>]?pickler : Pickler<'T>, [<O;D(null)>]?streamingContext : StreamingContext) : 'T =
        let pickler = match pickler with None -> resolver.Value.Resolve<'T> () | Some p -> p
        let state = new CloneState(resolver.Value, ?streamingContext = streamingContext, unSiftData = (values, sifted.SiftedIndices))
        pickler.Clone state sifted.Value

    /// <summary>Compute size in bytes for given input.</summary>
    /// <param name="value">input value.</param>
    static member ComputeSize<'T>(value : 'T) = defaultSerializer.Value.ComputeSize(value)

    /// <summary>
    ///     Visits all reference types that appear in the given object graph.
    /// </summary>
    /// <param name="visitor">Visitor implementation.</param>
    /// <param name="graph">Object graph.</param>
    static member VisitObject(visitor : IObjectVisitor, graph : 'T) =
        let resolver = defaultSerializer.Value.Resolver
        let cache = defaultSerializer.Value.ReflectionCache
        let pickler = resolver.Resolve<'T> ()
        use nullFormat = new NullPickleWriter()
        let state = new WriteState(nullFormat, resolver, cache, visitor = visitor)
        pickler.Write state "value" graph

    /// <summary>Compute size and hashcode for given input.</summary>
    /// <param name="value">input value.</param>
    /// <param name="hashFactory">the hashing algorithm to be used. MurMur3 by default.</param>
    static member ComputeHash<'T>(value : 'T, [<O;D(null)>]?hashFactory : IHashStreamFactory) =
        defaultSerializer.Value.ComputeHash(value, ?hashFactory = hashFactory)

    /// <summary>
    ///     Uses FsPickler to traverse the object graph, gathering types of objects as it goes.
    /// </summary>
    /// <param name="graph">input object graph.</param>
    static member GatherTypesInObjectGraph(graph : obj) =
        let gathered = new HashSet<Type> ()
        let visitor =
            {
                new IObjectVisitor with
                    member __.Visit (_, value : 'T) = 
                        match box value with
                        | null -> ()
                        | :? Type as t -> gathered.Add t |> ignore
                        | :? MemberInfo as m when m.DeclaringType <> null ->
                            gathered.Add m.DeclaringType |> ignore

                        | value -> 
                            match value.GetType() with
                            | null -> ()
                            | t -> gathered.Add t |> ignore
            }

        do FsPickler.VisitObject(visitor, graph)
        gathered |> Seq.toArray

    /// <summary>
    ///     Use FsPickler to traverse the object graph, gathering object instances as it goes.
    /// </summary>
    /// <param name="graph">input object graph.</param>
    static member GatherObjectsInGraph (graph : obj) =
        let gathered = new HashSet<obj> ()
        let visitor =
            {
                new IObjectVisitor with
                    member __.Visit (_, value : 'T) =
                        match box value with
                        | null -> ()
                        | v -> gathered.Add v |> ignore
            }

        do FsPickler.VisitObject(visitor, graph)
        gathered |> Seq.toArray