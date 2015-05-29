namespace Nessos.FsPickler

open System
open System.Collections.Generic
open System.Runtime.Serialization

open Nessos.FsPickler.Hashing

/// F# Extension methods for FsPickler

[<AutoOpen>]
module ExtensionMethods =

    /// Object pickle with type annotation
    [<AutoSerializable(true)>]
    type Pickle<'T> internal (bytes : byte []) =
        /// Byte array pickle
        member __.Bytes = bytes

    /// Contains a sifted graph whose sifted values are distinguished by hashcode.
    [<Sealed; DataContract>]
    type HashSift<'T> internal (sifted : Sifted<'T>, hashIndices : (HashResult * int64 []) []) =
        [<DataMember(Name = "Sifted")>]
        let sifted = sifted
        [<DataMember(Name = "Indices")>]
        let hashIndices = hashIndices
        /// Collection of all hashes of objects sifted from object graph.
        member __.Hashes = hashIndices |> Array.map fst
        member internal __.Sifted = sifted
        member internal __.HashIndices = hashIndices

        override __.ToString() = sifted.ToString()

    type FsPicklerSerializer with

        /// <summary>
        ///     Creates a type annotated pickle for given value.
        /// </summary>
        /// <param name="value">Value to be pickled.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        member fsp.PickleTyped(value : 'T, ?streamingContext, ?encoding) : Pickle<'T> = 
            let bytes = fsp.Pickle(value, ?streamingContext = streamingContext, ?encoding = encoding)
            new Pickle<'T>(bytes)

        /// <summary>
        ///     Deserializes a type annotated pickle.
        /// </summary>
        /// <param name="pickle">Type annotated pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        member fsp.UnPickleTyped(pickle : Pickle<'T>, ?streamingContext, ?encoding) : 'T =
            fsp.UnPickle<'T>(pickle.Bytes, ?streamingContext = streamingContext, ?encoding = encoding)


        /// <summary>
        ///    Creates a sifted copy of provided graph in which sifted values are distinguished by hash code.
        ///     Returns a HashSift container as well as a manifest of all objects that were sifted and their hashcodes.
        /// </summary>
        /// <param name="graph">Object graph to be sifted.</param>
        /// <param name="shouldSift">Predicate deciding whether supplied object should be sifted.</param>
        /// <param name="pickler">Pickler used for sifting. Defaults to auto-generated pickler.</param>
        /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
        member fsp.HashSift<'T>(graph : 'T, shouldSift : obj -> HashResult -> bool, ?pickler : Pickler<'T>, ?streamingContext : StreamingContext) : HashSift<'T> * (obj * HashResult) [] =
            let hashed = new Dictionary<HashResult, bool> ()
            let siftIndex = new Dictionary<int64, HashResult> ()
            let sifter = 
                {
                    new IObjectSifter with
                        member __.Sift(pickler: Pickler<'a>, id: int64, value: 'a) = 
                            let hash = fsp.ComputeHash value
                            let mutable result = false
                            if hashed.TryGetValue(hash, &result) then 
                                if result then siftIndex.Add(id, hash)
                                result

                            elif shouldSift value hash then
                                hashed.Add(hash, true)
                                siftIndex.Add(id, hash)
                                true
                            else
                                hashed.Add(hash, false)
                                false
                }

            let siftedGraph, siftedValues = FsPickler.Sift(graph, sifter, ?pickler = pickler, ?streamingContext = streamingContext)

            let hashIndices, hashObjs =
                siftedValues 
                |> Seq.map (fun (id, obj) -> id, obj, siftIndex.[id])
                |> Seq.groupBy (fun (_,_,hash) -> hash)
                |> Seq.map (fun (hash, values) -> 
                                let _,obj,_ = Seq.head values 
                                let ids = values |> Seq.map (fun (id,_,_) -> id) |> Seq.toArray
                                (hash, ids), (obj, hash))
                |> Seq.toArray
                |> Array.unzip

            let hs = new HashSift<'T>(siftedGraph, hashIndices)
            hs, hashObjs

        /// <summary>
        ///     Unsifts a provided sifted object graph using supplied values and their corresponding hash codes.
        /// </summary>
        /// <param name="sifted">Graph to be unsifted.</param>
        /// <param name="hashValues">Values to be used in unsifting by hash code.</param>
        /// <param name="pickler">Pickler used for sifting. Defaults to auto-generated pickler.</param>
        /// <param name="streamingContext">Streaming context used for cloning. Defaults to null streaming context.</param>
        member fsp.HashUnsift<'T>(sifted : HashSift<'T>, hashValues : (obj * HashResult) [], ?pickler : Pickler<'T>, ?streamingContext : StreamingContext) : 'T =
            let hashIndices = dict sifted.HashIndices
            let siftedValues =
                hashValues
                |> Seq.collect (fun (obj, hash) -> hashIndices.[hash] |> Seq.map (fun id -> (id,obj)))
                |> Seq.toArray

            FsPickler.UnSift(sifted.Sifted, siftedValues, ?pickler = pickler, ?streamingContext = streamingContext)

    type Pickler with
        /// <summary>Initializes a pickler out of a pair of read/write lambdas. Unsafe pickler generation method.</summary>
        /// <param name="reader">Deserialization logic for the pickler.</param>
        /// <param name="writer">Serialization logic for the pickler.</param>
        /// <param name="cloner">In-memory cloning logic for the pickler. Defaults to no cloning implementation.</param>
        /// <param name="accepter">Visitor accepting function for the descendand nodes of the graph. Defaults to no visitor implementation.</param>
        /// <param name="cacheByRef">Specifies whether objects serialized by this pickler should be cached by reference.</param>
        /// <param name="useWithSubtypes">Specifies whether pickler should also apply for all subtypes.</param>
        static member FromPrimitives<'T>(reader : ReadState -> 'T, writer : WriteState -> 'T -> unit, ?cloner : CloneState -> 'T -> 'T, ?accepter : VisitState -> 'T -> unit, ?cacheByRef, ?useWithSubtypes) =
            if typeof<'T>.IsPrimitive || typeof<'T> = typeof<string> then
                invalidArg typeof<'T>.FullName "defining custom picklers for primitives not supported."

            let reader r (tag : string) = reader r
            let writer w (tag : string) (t : 'T) = writer w t
            let cloner = match cloner with Some c -> c | None -> fun _ _ -> invalidOp <| sprintf "User-defined pickler of type '%O' does not implement a cloning function." typeof<'T>
            let accepter = match accepter with Some a -> a | None -> fun _ _ -> invalidOp <| sprintf "User-defined pickler of type '%O' does not implement a visitor function." typeof<'T>

            CompositePickler.Create(reader, writer, cloner, accepter, PicklerInfo.UserDefined, ?cacheByRef = cacheByRef, ?useWithSubtypes = useWithSubtypes)


    type SerializationInfo with
        /// <summary>
        ///     Adds value of given type to SerializationInfo instance.
        /// </summary>
        /// <param name="name">Name for value.</param>
        /// <param name="value">Input value.</param>
        member inline sI.Add<'T>(name : string, value : 'T) : unit =
            sI.AddValue(name, value, typeof<'T>)

        /// <summary>
        ///     Gets value of given type and provided name from SerializationInfo instance.
        /// </summary>
        /// <param name="name">Name for value.</param>
        member inline sI.Get<'T>(name : string) : 'T =
            sI.GetValue(name, typeof<'T>) :?> 'T

        /// <summary>
        ///     Try getting value of provided type and name from SerializationInfo instance.
        ///     Returns 'None' if not found.
        /// </summary>
        /// <param name="name">Name for value.</param>
        member sI.TryGet<'T>(name : string) : 'T option =
            // we use linear traversal; that's ok since entry count
            // is typically small and this is how it's done in the
            // proper SerializationInfo.GetValue() implementation.
            let e = sI.GetEnumerator()
            let mutable found = false
            let mutable entry = Unchecked.defaultof<SerializationEntry>
            while not found && e.MoveNext() do
                entry <- e.Current
                found <- entry.Name = name

            if found && entry.ObjectType = typeof<'T> then
                Some (entry.Value :?> 'T)
            else None

        /// <summary>
        ///     Try getting value of provided name from SerializationInfo instance.
        ///     Returns 'None' if not found.
        /// </summary>
        /// <param name="name">Name for value.</param>
        member sI.TryGetObj(name : string) : obj option =
            // we use linear traversal; that's ok since entry count
            // is typically small and this is how it's done in the
            // proper SerializationInfo.GetValue() implementation.
            let e = sI.GetEnumerator()
            let mutable found = false
            let mutable entry = Unchecked.defaultof<SerializationEntry>
            while not found && e.MoveNext() do
                entry <- e.Current
                found <- entry.Name = name

            if found then Some entry.Value
            else None