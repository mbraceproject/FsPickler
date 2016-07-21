namespace MBrace.FsPickler

open System
open System.Collections.Generic
open System.Runtime.Serialization
open System.Text
open System.Text.RegularExpressions

open MBrace.FsPickler.Hashing

/// F# Extension methods for FsPickler

[<AutoOpen>]
module ExtensionMethods =

    /// Pickled object with type annotation
    [<Sealed; DataContract>]
    type Pickle<'T> =
        [<DataMember(Name = "Bytes")>]
        val mutable private bytes : byte []
        /// Wraps a byte array into a type annotated pickle.
        new (bytes : byte[]) = { bytes = bytes }
        /// Byte array pickle
        member __.Bytes = __.bytes

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

            let reader r (_ : string) = reader r
            let writer w (_ : string) (t : 'T) = writer w t
            let cloner = match cloner with Some c -> c | None -> fun _ _ -> invalidOp <| sprintf "User-defined pickler of type '%O' does not implement a cloning function." typeof<'T>
            let accepter = match accepter with Some a -> a | None -> fun _ _ -> invalidOp <| sprintf "User-defined pickler of type '%O' does not implement a visitor function." typeof<'T>

            CompositePickler.Create(reader, writer, cloner, accepter, PicklerInfo.UserDefined, ?cacheByRef = cacheByRef, ?useWithSubtypes = useWithSubtypes)

        /// <summary>
        ///     Initializes a pickler instance using a pair of SerializationInfo lambdas.
        /// </summary>
        /// <param name="reader">SerializationInfo deserializer.</param>
        /// <param name="writer">SerializationInfo serializer.</param>
        /// <param name="useWithSubtypes">Use with all subtypes of given type. Defaults to false.</param>
        static member FromSerializationInfo<'T>(reader : SerializationInfo -> 'T, writer : SerializationInfo -> 'T -> unit, ?useWithSubtypes : bool) : Pickler<'T> =
            ISerializablePickler.FromSerializationInfo(reader, writer, ?useWithSubtypes = useWithSubtypes)

        /// A pickler that always serializes instances as zero(null).
        /// Useful for forcing serialization of non-serializable fields.
        static member Null<'T>(?useWithSubtypes : bool) : Pickler<'T> =
            CompositePickler.FromPrimitives<'T>((fun _ -> Unchecked.defaultof<'T>), (fun _ _ -> ()), ?useWithSubtypes = useWithSubtypes, cacheByRef = false)


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


    let private hashRegex = new Regex("^([^/:]+)://([^/]*)/([0-9]+)/([^/]*)$", RegexOptions.Compiled)
    type HashResult with
        /// Returns a unique, case-sensitive hash identifier
        member h.Id = 
            (new StringBuilder())
                .Append(h.Algorithm)
                .Append("://")
                .Append(h.Type)
                .Append('/')
                .Append(h.Length)
                .Append('/')
                .Append(Convert.ToBase64String(h.Hash).Replace('/','-'))
                .ToString()

        /// Parses hash identifier to receive a hash record
        static member Parse(id : string) : HashResult =
            let m = hashRegex.Match(id)
            if not m.Success then raise <| new FormatException("Input string does not match hash id format.")
            else
                {
                    Algorithm = m.Groups.[1].Value
                    Type = m.Groups.[2].Value
                    Length = m.Groups.[3].Value |> int64
                    Hash = m.Groups.[4].Value.Replace('-','/') |> Convert.FromBase64String
                }