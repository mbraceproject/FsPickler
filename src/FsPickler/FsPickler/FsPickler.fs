namespace Nessos.FsPickler

    open System
    open System.Reflection
    open System.Collections.Generic
    
    open Nessos.FsPickler.Hashing
    open Nessos.FsPickler.TypeCache

    /// FsPickler public facade
    type FsPickler private () =
        
        static let defaultPickler = lazy(new BinaryPickler())

        static member internal DefaultPickler = defaultPickler.Value
        static member internal Resolver = PicklerCache.Instance :> IPicklerResolver

        /// <summary>
        ///     Create a new binary pickler instance.
        /// </summary>
        /// <param name="forceLittleEndian">Force little-endian encoding in primitive arrays but is slower. Defaults to false.</param>
        /// <param name="tyConv">optional type name converter implementation.</param>
        static member CreateBinary([<O;D(null)>] ?forceLittleEndian, [<O;D(null)>] ?typeConverter) = 
            new BinaryPickler(?forceLittleEndian = forceLittleEndian, ?typeConverter = typeConverter)

        /// <summary>
        ///     Create a new XML pickler instance.
        /// </summary>
        /// <param name="tyConv">optional type name converter implementation.</param>
        static member CreateXml([<O;D(null)>]?typeConverter, [<O;D(null)>]?indent) = 
            new XmlPickler(?typeConverter = typeConverter, ?indent = indent)

        /// Decides if given type is serializable by FsPickler
        static member IsSerializableType (t : Type) = 
            FsPickler.Resolver.IsSerializable t

        /// Decides if given type is serializable by FsPickler
        static member IsSerializableType<'T> () = 
            FsPickler.Resolver.IsSerializable<'T> ()

        /// Auto generates a pickler for given type variable
        static member GeneratePickler<'T> () = 
            FsPickler.Resolver.Resolve<'T> ()
        
        /// Auto generates a pickler for given type
        static member GeneratePickler (t : Type) = 
            FsPickler.Resolver.Resolve t

        //
        // Misc utils
        //

        /// <summary>Compute size in bytes for given input.</summary>
        /// <param name="value">input value.</param>
        static member ComputeSize<'T>(value : 'T) = defaultPickler.Value.ComputeSize(value)

        /// <summary>
        ///     Visits all reference types that appear in the given object graph.
        /// </summary>
        /// <param name="visitor">Visitor implementation.</param>
        /// <param name="graph">Object graph.</param>
        static member VisitObject(visitor : IObjectVisitor, graph : 'T) =
            defaultPickler.Value.VisitObject(visitor, graph)

        /// <summary>Compute size and hashcode for given input.</summary>
        /// <param name="value">input value.</param>
        /// <param name="hashFactory">the hashing algorithm to be used. MurMur3 by default.</param>
        static member ComputeHash<'T>(value : 'T, [<O;D(null)>] ?hashFactory : IHashStreamFactory) =
            defaultPickler.Value.ComputeHash(value, ?hashFactory = hashFactory)

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
                            | :? MemberInfo as m -> 
                                match m.ReflectedType, m.DeclaringType with
                                | null, null -> ()
                                | null, dt -> gathered.Add dt |> ignore
                                | rt, _ -> gathered.Add rt |> ignore

                            | :? Assembly
                            | :? AssemblyInfo -> ()
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
                            | :? Type | :? AssemblyInfo -> ()
                            | _ -> gathered.Add value |> ignore
                }

            do FsPickler.VisitObject(visitor, graph)
            gathered |> Seq.toArray