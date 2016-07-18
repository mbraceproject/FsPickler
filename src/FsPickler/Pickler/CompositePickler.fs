namespace Nessos.FsPickler

//
//  CompositePickler
//
//  Any non-trivial pickler is defined as an instance of CompositePickler.
//  CompositePickler defines all the logic required for handling arbitrary 
//  .NET objects: null instances, caching, subtype resolution, cyclic objects.
//
//  CompositePickler is designed so that implementation can be copied fieldwise.
//  This permits the definition of recursive picklers without need for wrappers.
//

open System
open System.Runtime.CompilerServices
open System.Runtime.Serialization

[<AutoSerializable(false)>]
type internal CompositePickler<'T> =
    inherit Pickler<'T>

    val mutable private m_IsInitialized : bool

    // stores the supertype pickler, if generated using subtype resolution
    val mutable private m_NestedPickler : Pickler option

    val mutable private m_Writer   : WriteState -> string -> 'T -> unit
    val mutable private m_Reader   : ReadState -> string -> 'T
    val mutable private m_Cloner   : CloneState -> 'T -> 'T
    val mutable private m_Accepter : VisitState -> 'T -> unit

    val mutable private m_PicklerInfo : PicklerInfo

    val mutable private m_IsCacheByRef : bool
    val mutable private m_UseWithSubtypes : bool
    val mutable private m_SkipHeaderWrite : bool
    val mutable private m_Bypass : bool
    val mutable private m_SkipVisit : bool
    val mutable private m_IsCloneableOnly : bool

    private new (reader, writer, cloner, accepter, nested : Pickler option, picklerInfo, ?cacheByRef, ?useWithSubtypes, ?skipHeaderWrite, ?bypass, ?skipVisit, ?isCloneableOnly) =
        {
            inherit Pickler<'T> ()

            m_IsInitialized = true

            m_NestedPickler = nested

            m_Writer = writer
            m_Reader = reader
            m_Cloner = cloner
            m_Accepter = accepter

            m_PicklerInfo = picklerInfo

            m_IsCacheByRef = match cacheByRef with Some c -> c || base.IsRecursiveType | None -> base.Kind > Kind.String
            m_UseWithSubtypes = defaultArg useWithSubtypes false
            m_SkipHeaderWrite = defaultArg skipHeaderWrite false
            m_Bypass = defaultArg bypass false
            m_SkipVisit = defaultArg skipVisit false
            m_IsCloneableOnly = defaultArg isCloneableOnly false
        }

    /// <summary>
    ///     Primary constructor for definining a materialized composite pickler
    /// </summary>
    /// <param name="reader">deserialization lambda.</param>
    /// <param name="writer">serialization lambda.</param>
    /// <param name="picklerInfo">pickler generation metadata.</param>
    /// <param name="cacheByRef">enable caching by reference for serialized instances.</param>
    /// <param name="useWithSubtypes">allow casting of pickler implementation to proper subtypes.</param>
    /// <param name="skipHeaderWrite">skip header serialization of instances.</param>
    /// <param name="bypass">pickle using serialization/deserialization lambdas directly.</param>
    /// <param name="skipVisit">do not apply visitor to instances if specified.</param>
    new (reader, writer, cloner, accepter, picklerInfo, ?cacheByRef, ?useWithSubtypes, ?skipHeaderWrite, ?bypass, ?skipVisit, ?isCloneableOnly) =
        new CompositePickler<'T>(reader, writer, cloner, accepter, None, picklerInfo, ?cacheByRef = cacheByRef, ?useWithSubtypes = useWithSubtypes, 
                                                    ?skipHeaderWrite = skipHeaderWrite, ?bypass = bypass, ?skipVisit = skipVisit, ?isCloneableOnly = isCloneableOnly)

    /// <summary>
    ///     Uninitialized pickler constructor
    /// </summary>
    new () = 
        {
            inherit Pickler<'T>()

            m_IsInitialized = false

            m_NestedPickler = None

            m_Writer = fun _ _ -> invalidOp "Attempting to consume pickler at initialization time."
            m_Reader = fun _ -> invalidOp "Attempting to consume pickler at initialization time."
            m_Cloner = fun _ _ -> invalidOp "Attempting to consume pickler at initialization time."
            m_Accepter = fun _ _ -> invalidOp "Attempting to consume pickler at initialization time."

            m_PicklerInfo = Unchecked.defaultof<_>
            m_IsCacheByRef = Unchecked.defaultof<_>
            m_UseWithSubtypes = Unchecked.defaultof<_>
            m_SkipHeaderWrite = Unchecked.defaultof<_>
            m_Bypass = Unchecked.defaultof<_>
            m_SkipVisit = Unchecked.defaultof<_>
            m_IsCloneableOnly = Unchecked.defaultof<_>
        }

    override p.ImplementationType = 
        match p.m_NestedPickler with 
        | None -> typeof<'T> 
        | Some p -> p.Type

    override p.PicklerInfo = p.m_PicklerInfo
    override p.IsCacheByRef = p.m_IsCacheByRef
    override p.UseWithSubtypes = p.m_UseWithSubtypes
    override p.IsCloneableOnly = p.m_IsCloneableOnly

    override p.Cast<'S> () =
        match p.m_NestedPickler with
        | Some nested -> nested.Cast<'S> ()
        | None ->
            if not p.m_IsInitialized then invalidOp "Pickler has not been initialized."
            elif typeof<'T> = typeof<'S> then fastUnbox<Pickler<'S>> p
            elif typeof<'T>.IsAssignableFrom typeof<'S> && p.m_UseWithSubtypes then
                let writer = let wf = p.m_Writer in fun w t x -> wf w t (fastUnbox<'T> x)
                let reader = let rf = p.m_Reader in fun r t -> fastUnbox<'S> (rf r t)
                let cloner = let cf = p.m_Cloner in fun w x -> cf w (fastUnbox<'T> x) |> fastUnbox<'S>
                let accepter = let af = p.m_Accepter in fun w x -> af w (fastUnbox<'T> x)

                new CompositePickler<'S>(reader, writer, cloner, accepter, Some(p :> _), p.m_PicklerInfo, p.m_IsCacheByRef, p.m_UseWithSubtypes, 
                                            skipHeaderWrite = p.m_SkipHeaderWrite, bypass = p.m_Bypass, skipVisit = p.m_SkipVisit, isCloneableOnly = p.m_IsCloneableOnly) :> Pickler<'S>

            else
                let msg = sprintf "Cannot cast pickler of type '%O' to '%O'." typeof<'T> typeof<'S>
                raise <| new InvalidCastException(msg)

    /// Pickler initialization code
    member internal p.InitializeFrom(p' : Pickler) : unit =
        match p'.Cast<'T> () with
        | :? CompositePickler<'T> as p' ->
            if p.m_IsInitialized then
                invalidOp "target pickler has already been initialized."
            elif not p'.m_IsInitialized then 
                raise <| new PicklerGenerationException(p.Type, "source pickler has not been initialized.")
            else
                p.m_PicklerInfo <- p'.m_PicklerInfo
                p.m_IsCacheByRef <- p'.m_IsCacheByRef
                p.m_UseWithSubtypes <- p'.m_UseWithSubtypes
                p.m_Writer <- p'.m_Writer
                p.m_Reader <- p'.m_Reader
                p.m_Cloner <- p'.m_Cloner
                p.m_Accepter <- p'.m_Accepter
                p.m_NestedPickler <- p'.m_NestedPickler
                p.m_SkipHeaderWrite <- p'.m_SkipHeaderWrite
                p.m_Bypass <- p'.m_Bypass
                p.m_SkipVisit <- p'.m_SkipVisit
                p.m_IsCloneableOnly <- p'.m_IsCloneableOnly
                p.m_IsInitialized <- true

        | _ -> raise <| new PicklerGenerationException(p.Type, sprintf "source pickler is of invalid type (%O)." p'.Type)

    member internal p.Writer = p.m_Writer
    member internal p.Reader = p.m_Reader
    member internal p.Cloner = p.m_Cloner
    member internal p.Accepter = p.m_Accepter

    override p.Write (state : WriteState) (tag : string) (value : 'T) =
        let formatter = state.Formatter

        if p.m_Bypass then p.m_Writer state tag value
        elif p.Kind <= Kind.Value then
            formatter.BeginWriteObject tag ObjectFlags.None
            p.m_Writer state tag value
            formatter.EndWriteObject ()

        elif obj.ReferenceEquals(value, null) then
            formatter.BeginWriteObject tag ObjectFlags.IsNull
            formatter.EndWriteObject ()
        else

#if PROTECT_STACK_OVERFLOWS
        do RuntimeHelpers.EnsureSufficientExecutionStack()
#endif
        let mutable isProperSubtype = false
        let mutable subtype = Unchecked.defaultof<Type>
        if p.Kind > Kind.Sealed && not p.m_UseWithSubtypes then
            subtype <- value.GetType()
            if subtype <> p.Type then
                isProperSubtype <- true

        if isProperSubtype then
            if state.DisableSubtypes then
                let msg = sprintf "Subtype serialization has been disabled. Value %A is of proper subtype %O." value subtype
                raise <| new FsPicklerException(msg)

            let p0 = state.PicklerResolver.Resolve subtype
            formatter.BeginWriteObject tag ObjectFlags.IsProperSubtype
            state.TypePickler.Write state "subtype" subtype
            p0.UntypedWrite state "instance" value
            formatter.EndWriteObject()

        elif p.m_IsCacheByRef || Option.isSome state.Sifter then
            let mutable firstOccurence = false
            let id = state.GetObjectId(value, &firstOccurence)

            if firstOccurence then
                // check if value should be sifted from serialization
                match state.Sifter with
                | Some sifter when sifter.Sift(p, id, value) ->
                    state.Sifted.Add(id, box value)
                    formatter.BeginWriteObject tag ObjectFlags.IsSiftedValue
                    formatter.EndWriteObject()

                | _ ->
                    if p.IsRecursiveType then state.ObjectStack.Push id

                    if p.m_SkipHeaderWrite then
                        p.m_Writer state tag value
                    else
                        formatter.BeginWriteObject tag ObjectFlags.None
                        p.m_Writer state tag value
                        formatter.EndWriteObject ()

                    if p.IsRecursiveType then state.ObjectStack.Pop () |> ignore

            elif p.IsRecursiveType && p.Kind <> Kind.Array && not <| state.CyclicObjectSet.Contains id && state.ObjectStack.Contains id then
                // came across cyclic object, record fixup-related data
                // cyclic objects are handled once per instance
                // instances of cyclic arrays are handled differently than other reference types
                do state.CyclicObjectSet.Add(id) |> ignore
                    
                formatter.BeginWriteObject tag ObjectFlags.IsCyclicInstance
                formatter.WriteCachedObjectId id
                formatter.EndWriteObject()
            else
                formatter.BeginWriteObject tag ObjectFlags.IsCachedInstance
                formatter.WriteCachedObjectId id
                formatter.EndWriteObject()
        else
            if p.m_SkipHeaderWrite then
                p.m_Writer state tag value
            else
                formatter.BeginWriteObject tag ObjectFlags.None
                p.m_Writer state tag value
                formatter.EndWriteObject ()


    override p.Read (state : ReadState) (tag : string) =
        if p.m_Bypass then p.m_Reader state tag else

        let formatter = state.Formatter
        let flags = formatter.BeginReadObject tag

        if Enum.hasFlag flags ObjectFlags.IsNull then 
            formatter.EndReadObject ()
            fastUnbox<'T> null

        elif Enum.hasFlag flags ObjectFlags.IsProperSubtype then
            if state.DisableSubtypes then
                let msg = sprintf "Subtype deserialization has been disabled. Attempting to deserialize value of type %O" typeof<'T>
                raise <| new FsPicklerException(msg)

            let t0 = state.TypePickler.Read state "subtype"
            let p0 = state.PicklerResolver.Resolve t0
            let value = p0.UntypedRead state "instance" |> fastUnbox<'T>
            formatter.EndReadObject()
            value

        elif Enum.hasFlag flags ObjectFlags.IsCyclicInstance then
            // came across a nested instance of a cyclic object
            // add an uninitialized object to the cache and schedule
            // reflection-based fixup at the root level.
            let id = formatter.ReadCachedObjectId ()
            formatter.EndReadObject()

            let value = FormatterServices.GetUninitializedObject p.Type |> fastUnbox<'T>
            state.ObjectCache.Add(id, value)
            value

        elif Enum.hasFlag flags ObjectFlags.IsCachedInstance then
            let id = formatter.ReadCachedObjectId ()
            formatter.EndReadObject ()
            state.ObjectCache.[id] |> fastUnbox<'T>

        elif Enum.hasFlag flags ObjectFlags.IsSiftedValue then
            formatter.EndReadObject ()
            let id = state.NextObjectId()
            try state.ObjectCache.[id] |> fastUnbox<'T>
            with 
            | :? System.Collections.Generic.KeyNotFoundException ->
                raise <| new FsPicklerException(sprintf "Write state missing sifted object of id '%d'." id)
            | :? InvalidCastException ->
                let result = match state.ObjectCache.[id] with null -> "null" | o -> o.GetType().ToString()
                let msg = sprintf "Sifted object of id '%d' was expected to be of type '%O' but was '%O'." id typeof<'T> result
                raise <| new FsPicklerException(msg)

        // force caching on all reference types in case of sift mode
        elif p.m_IsCacheByRef || state.IsUnSifting then
            let id = state.NextObjectId()
            let value = p.m_Reader state tag
            formatter.EndReadObject()

            if p.Kind = Kind.Array then
                // depending on the format implementation,
                // array picklers may or may not cache deserialized values early.
                // solve this ambiguity by forcing an update here.
                state.ObjectCache.[id] <- value ; value

            elif p.IsRecursiveType then
                let mutable cachedInstance : obj = null
                let found = state.ObjectCache.TryGetValue(id, &cachedInstance)

                if found then
                    // deserialization reached root level of a cyclic object
                    // create cyclic binding by performing shallow field copying
                    let cachedInstance = fastUnbox<'T> cachedInstance
                    do ShallowObjectCopier<'T>.Copy value cachedInstance
                    cachedInstance
                else
                    state.ObjectCache.Add(id, value) ; value
            else
                state.ObjectCache.Add(id, value) ; value
        else
            let value = p.m_Reader state tag
            formatter.EndReadObject ()
            value

    override p.Clone (state : CloneState) (value : 'T) : 'T =
        // create a unique id for node in graph 
        // this differs from the id generated by the ObjectIdGenerator
        // in that identical instances may generate multiple node id's while being traversed
        let nodeId = state.NextNodeId()

        if p.m_Bypass || p.Kind <= Kind.Value then p.m_Cloner state value
        elif obj.ReferenceEquals(value, null) then
            // in-memory sifted nodes in graph are represented as nulls
            // check unsift data to check if current node requires unsifting
            match state.UnSiftData with
            | Some d ->
                let mutable unsifted = Unchecked.defaultof<int64 * obj>
                if d.TryGetValue(nodeId, &unsifted) then
                    match snd unsifted with
                    | :? 'T as t -> t
                    | o ->
                        let result = match o with null -> "null" | o -> o.GetType().ToString()
                        let msg = sprintf "Sifted object of id '%d' was expected to be of type '%O' but was '%O'." (fst unsifted) typeof<'T> result
                        raise <| new FsPicklerException(msg)
                else
                    value

            | None -> value
        else

#if PROTECT_STACK_OVERFLOWS
        do RuntimeHelpers.EnsureSufficientExecutionStack()
#endif
        let mutable isProperSubtype = false
        let mutable subtype = Unchecked.defaultof<Type>
        if p.Kind > Kind.Sealed && not p.m_UseWithSubtypes then
            let t0 = value.GetType()
            if t0 <> p.Type then
                isProperSubtype <- true
                subtype <- t0

        if isProperSubtype then
            state.DeclareProperSubtype()
            let p0 = state.PicklerResolver.Resolve subtype
            p0.UntypedClone state value |> fastUnbox<'T>

        elif p.m_IsCacheByRef then
            let mutable firstOccurence = false
            let id = state.GetReferenceId(value, &firstOccurence)

            let mutable isSifted = false
            match state.SiftData with
            | Some(sifter, container) ->
                if firstOccurence then
                    if sifter.Sift(p, id, value) then
                        let ra = new ResizeArray<int64> ()
                        container.Add(id, (box value, ra))
                        ra.Add nodeId
                        isSifted <- true
                else
                    let mutable contents = Unchecked.defaultof<obj * ResizeArray<int64>>
                    if container.TryGetValue(id, &contents) then
                        (snd contents).Add nodeId
                        isSifted <- true

            | None -> ()

            if isSifted then fastUnbox<'T> null
            elif firstOccurence then
                // cyclic arrays require special logic which is
                // contained in the picklers, access directly from here.
                if p.Kind = Kind.Array then
                    p.m_Cloner state value

                elif p.IsRecursiveType then 
                    state.ObjectStack.Push id
                    let t = p.m_Cloner state value
                    let _ = state.ObjectStack.Pop ()
                    let mutable cachedInstance : obj = null
                    let found = state.ObjectCache.TryGetValue(id, &cachedInstance)
                    if found then
                        // deserialization reached root level of a cyclic object
                        // create cyclic binding by performing shallow field copying
                        let cachedInstance = fastUnbox<'T> cachedInstance
                        do ShallowObjectCopier<'T>.Copy t cachedInstance
                        cachedInstance
                    else
                        state.ObjectCache.Add(id, t) ; t

                else
                    let t = p.m_Cloner state value
                    state.ObjectCache.Add(id, t) ; t

            elif p.IsRecursiveType && p.Kind <> Kind.Array && state.ObjectStack.Contains id && not <| state.CyclicObjectSet.Contains id then
                do state.CyclicObjectSet.Add(id) |> ignore
                let value = FormatterServices.GetUninitializedObject p.Type |> fastUnbox<'T>
                state.ObjectCache.Add(id, value)
                value
            else
                state.ObjectCache.[id] |> fastUnbox<'T>
        else
            p.m_Cloner state value

    override p.Accept (state : VisitState) (value : 'T) =
        if state.IsCancelled then () else

        let inline acceptNode () =
            let preorder = Enum.hasFlag state.VisitOrder VisitOrder.PreOrder
            let mutable shouldContinue = true

            if preorder && not p.m_SkipVisit then
                shouldContinue <- 
                    match state.Visitor with
                    | :? ISpecializedObjectVisitor<'T> as fv -> fv.VisitSpecialized(p, value)
                    | v -> v.Visit(p, value)

                if not shouldContinue then 
                    state.IsCancelled <- true

            // visit children
            if shouldContinue then p.m_Accepter state value

            if not preorder && not p.m_SkipVisit then
                shouldContinue <- 
                    match state.Visitor with
                    | :? ISpecializedObjectVisitor<'T> as fv -> fv.VisitSpecialized(p, value)
                    | v -> v.Visit(p,value)

                if not shouldContinue then 
                    state.IsCancelled <- true

        if p.Kind <= Kind.Value then acceptNode ()
        elif obj.ReferenceEquals(value, null) then
            let shouldContinue =
                match state.Visitor with
                | :? ISpecializedObjectVisitor<'T> as fv -> fv.VisitSpecialized(p, value)
                | v -> v.Visit(p, value)

            if not shouldContinue then state.IsCancelled <- true
        else

#if PROTECT_STACK_OVERFLOWS
        do RuntimeHelpers.EnsureSufficientExecutionStack()
#endif
        let mutable isProperSubtype = false
        let mutable subtype = Unchecked.defaultof<Type>
        if p.Kind > Kind.Sealed && not p.m_UseWithSubtypes then
            subtype <- value.GetType()
            if subtype <> p.Type then
                isProperSubtype <- true

        if isProperSubtype then
            let p0 = state.PicklerResolver.Resolve subtype
            p0.UntypedAccept state value

        elif p.m_IsCacheByRef then
            let mutable firstOccurence = false
            let _ = state.ObjectIDGenerator.GetId(value, &firstOccurence)
            if firstOccurence then acceptNode ()
        else
            acceptNode ()

and internal CompositePickler =

    /// <summary>
    ///     Creates an empty composite pickler for given type.
    /// </summary>
    static member CreateUninitialized<'T> () = new CompositePickler<'T> ()

    /// <summary>
    ///     Initializes a CompositePickler by copying fields from a source pickler
    /// </summary>
    /// <param name="source"></param>
    /// <param name="target"></param>
    static member Copy(source : Pickler, target : Pickler) =
        target.Unpack {
            new IPicklerUnpacker<bool> with
                member __.Apply (p : Pickler<'T>) =
                    match p with
                    | :? CompositePickler<'T> as cp -> cp.InitializeFrom source ; true
                    | _ -> 
                        let msg = sprintf "expected CompositePickler but was '%O'" <| p.GetType()
                        invalidArg "target" <| msg
        } |> ignore

    /// <summary>
    ///     Primary constructor for definining a materialized composite pickler
    /// </summary>
    /// <param name="reader">deserialization lambda.</param>
    /// <param name="writer">serialization lambda.</param>
    /// <param name="picklerInfo">pickler generation metadata.</param>
    /// <param name="cacheByRef">enable caching by reference for serialized instances.</param>
    /// <param name="useWithSubtypes">allow casting of pickler implementation to proper subtypes.</param>
    /// <param name="skipHeaderWrite">skip header serialization of instances.</param>
    /// <param name="bypass">pickle using serialization/deserialization lambdas directly.</param>
    /// <param name="skipVisit">do not apply visitor to instances if specified.</param>
    static member Create<'T>(reader, writer, cloner, accepter, picklerInfo, ?cacheByRef : bool, ?useWithSubtypes : bool, ?skipHeaderWrite : bool, ?bypass : bool, ?skipVisit : bool, ?isCloneableOnly : bool) =
        new CompositePickler<'T>(reader, writer, cloner, accepter, picklerInfo, ?cacheByRef = cacheByRef, ?useWithSubtypes = useWithSubtypes, 
                                                    ?skipHeaderWrite = skipHeaderWrite, ?bypass = bypass, ?skipVisit = skipVisit, ?isCloneableOnly = isCloneableOnly) :> Pickler<'T>

    static member ObjectPickler =
        CompositePickler.Create<obj>((fun _ _ -> obj ()), (fun _ _ _ -> ()), (fun _ _ -> obj ()), (fun _ _ -> ()), PicklerInfo.Object, cacheByRef = true)