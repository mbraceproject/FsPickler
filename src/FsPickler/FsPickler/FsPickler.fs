namespace Nessos.FsPickler

open System
open System.Reflection
open System.Collections.Generic
    
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
    static member CreateBinary([<O;D(null)>] ?forceLittleEndian, [<O;D(null)>] ?typeConverter) = 
        new BinarySerializer(?forceLittleEndian = forceLittleEndian, ?typeConverter = typeConverter)

    /// <summary>
    ///     Create a new XML pickler instance.
    /// </summary>
    /// <param name="tyConv">optional type name converter implementation.</param>
    static member CreateXml([<O;D(null)>]?typeConverter, [<O;D(null)>]?indent) = 
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
    ///     Creates a deep clone of given object.
    /// </summary>
    /// <param name="t"></param>
    static member Clone (t : 'T) =
        use m = new System.IO.MemoryStream()
        defaultSerializer.Value.Serialize(m, t, leaveOpen = true)
        m.Position <- 0L
        defaultSerializer.Value.Deserialize<'T>(m)

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
    static member ComputeHash<'T>(value : 'T, [<O;D(null)>] ?hashFactory : IHashStreamFactory) =
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
                    member __.Visit (value : 'T) = 
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
                    member __.Visit (value : 'T) = 
                        match box value with
                        | null -> ()
                        | v -> gathered.Add v |> ignore
            }

        do FsPickler.VisitObject(visitor, graph)
        gathered |> Seq.toArray