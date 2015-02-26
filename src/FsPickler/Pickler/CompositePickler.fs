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

    val mutable private m_Writer : WriteState -> string -> 'T -> unit
    val mutable private m_Reader : ReadState -> string -> 'T

    val mutable private m_PicklerInfo : PicklerInfo

    val mutable private m_IsCacheByRef : bool
    val mutable private m_UseWithSubtypes : bool
    val mutable private m_SkipHeaderWrite : bool
    val mutable private m_Bypass : bool
    val mutable private m_SkipVisit : bool

    private new (reader, writer, nested : Pickler option, picklerInfo, cacheByRef, ?useWithSubtypes, ?skipHeaderWrite, ?bypass, ?skipVisit) =
        {
            inherit Pickler<'T> ()

            m_IsInitialized = true

            m_NestedPickler = nested

            m_Writer = writer
            m_Reader = reader

            m_PicklerInfo = picklerInfo

            m_IsCacheByRef = cacheByRef
            m_UseWithSubtypes = defaultArg useWithSubtypes false
            m_SkipHeaderWrite = defaultArg skipHeaderWrite false
            m_Bypass = defaultArg bypass false
            m_SkipVisit = defaultArg skipVisit false
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
    new (reader, writer, picklerInfo, cacheByRef, ?useWithSubtypes, ?skipHeaderWrite, ?bypass, ?skipVisit) =
        new CompositePickler<'T>(reader, writer, None, picklerInfo, cacheByRef, ?useWithSubtypes = useWithSubtypes, 
                                                    ?skipHeaderWrite = skipHeaderWrite, ?bypass = bypass, ?skipVisit = skipVisit)

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

            m_PicklerInfo = Unchecked.defaultof<_>
            m_IsCacheByRef = Unchecked.defaultof<_>
            m_UseWithSubtypes = Unchecked.defaultof<_>
            m_SkipHeaderWrite = Unchecked.defaultof<_>
            m_Bypass = Unchecked.defaultof<_>
            m_SkipVisit = Unchecked.defaultof<_>
        }

    override p.ImplementationType = 
        match p.m_NestedPickler with 
        | None -> typeof<'T> 
        | Some p -> p.Type

    override p.PicklerInfo = p.m_PicklerInfo
    override p.IsCacheByRef = p.m_IsCacheByRef
    override p.UseWithSubtypes = p.m_UseWithSubtypes

    override p.Cast<'S> () =
        match p.m_NestedPickler with
        | Some nested -> nested.Cast<'S> ()
        | None ->
            if not p.m_IsInitialized then invalidOp "Pickler has not been initialized."
            elif typeof<'T> = typeof<'S> then fastUnbox<Pickler<'S>> p
            elif typeof<'T>.IsAssignableFrom typeof<'S> && p.m_UseWithSubtypes then
                let writer = let wf = p.m_Writer in fun w t x -> wf w t (fastUnbox<'T> x)
                let reader = let rf = p.m_Reader in fun r t -> fastUnbox<'S> (rf r t)

                new CompositePickler<'S>(reader, writer, Some(p :> _), p.m_PicklerInfo, p.m_IsCacheByRef, p.m_UseWithSubtypes, 
                                            skipHeaderWrite = p.m_SkipHeaderWrite, bypass = p.m_Bypass, skipVisit = p.m_SkipVisit) :> Pickler<'S>
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
                p.m_NestedPickler <- p'.m_NestedPickler
                p.m_SkipHeaderWrite <- p'.m_SkipHeaderWrite
                p.m_Bypass <- p'.m_Bypass
                p.m_SkipVisit <- p'.m_SkipVisit
                p.m_IsInitialized <- true

        | _ -> raise <| new PicklerGenerationException(p.Type, sprintf "source pickler is of invalid type (%O)." p'.Type)

    member internal p.Writer = p.m_Writer
    member internal p.Reader = p.m_Reader

    override p.Write (state : WriteState) (tag : string) (value : 'T) =

        let formatter = state.Formatter

        match state.Visitor with
        | Some v when not p.m_SkipVisit -> v.Visit value
        | _ -> ()

        if p.m_Bypass then p.m_Writer state tag value else

        if p.TypeKind = TypeKind.Value then
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

        let isProperSubtype = 
            if p.TypeKind <= TypeKind.Sealed || p.m_UseWithSubtypes then false
            else
                let t0 = value.GetType()
                if t0 <> p.Type then
                    let p0 = state.PicklerResolver.Resolve t0
                    formatter.BeginWriteObject tag ObjectFlags.IsProperSubtype
                    state.TypePickler.Write state "subtype" t0
                    p0.UntypedWrite state "instance" value
                    formatter.EndWriteObject()
                    true
                else
                    false

        if isProperSubtype then () else

        let isRecursive = p.IsRecursiveType

        if isRecursive || p.m_IsCacheByRef then
            let mutable firstOccurence = false
            let id = state.GetObjectId(value, &firstOccurence)

            let cyclicObjects = state.CyclicObjectSet
            let objStack = state.ObjectStack

            if firstOccurence then
                if isRecursive then objStack.Push id

                if p.m_SkipHeaderWrite then
                    p.m_Writer state tag value
                else
                    formatter.BeginWriteObject tag ObjectFlags.None
                    p.m_Writer state tag value
                    formatter.EndWriteObject ()

                if isRecursive then objStack.Pop () |> ignore

            elif p.IsRecursiveType && p.TypeKind <> TypeKind.Array && objStack.Contains id && not <| cyclicObjects.Contains id then
                // came across cyclic object, record fixup-related data
                // cyclic objects are handled once per instance
                // instances of cyclic arrays are handled differently than other reference types

                do cyclicObjects.Add(id) |> ignore
                    
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

        if p.TypeKind <= TypeKind.Value then
            let value = p.m_Reader state tag
            formatter.EndReadObject ()
            value

        elif ObjectFlags.hasFlag flags ObjectFlags.IsNull then 
            formatter.EndReadObject ()
            fastUnbox<'T> null

        elif ObjectFlags.hasFlag flags ObjectFlags.IsProperSubtype then
            let t0 = state.TypePickler.Read state "subtype"
            let p0 = state.PicklerResolver.Resolve t0
            let value = p0.UntypedRead state "instance" |> fastUnbox<'T>
            formatter.EndReadObject()
            value

        elif ObjectFlags.hasFlag flags ObjectFlags.IsCyclicInstance then
            // came across a nested instance of a cyclic object
            // add an uninitialized object to the cache and schedule
            // reflection-based fixup at the root level.
            let id = formatter.ReadCachedObjectId ()
            formatter.EndReadObject()

            let value = FormatterServices.GetUninitializedObject(p.Type)
            state.ObjectCache.Add(id, value)
            fastUnbox<'T> value

        elif ObjectFlags.hasFlag flags ObjectFlags.IsCachedInstance then
            let id = formatter.ReadCachedObjectId ()
            formatter.EndReadObject ()
            state.ObjectCache.[id] |> fastUnbox<'T>

        elif p.m_IsCacheByRef || p.IsRecursiveType then

            let id = state.NextObjectId()
            let value = p.m_Reader state tag
            formatter.EndReadObject()

            if p.TypeKind = TypeKind.Array then
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
                    do ShallowObjectCopier.Copy p.Type value cachedInstance
                    fastUnbox<'T> cachedInstance
                else
                    state.ObjectCache.Add(id, value) ; value
            else
                state.ObjectCache.Add(id, value) ; value
        else
            let value = p.m_Reader state tag
            formatter.EndReadObject ()
            value

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
    static member Create<'T>(reader, writer, picklerInfo, cacheByRef : bool, ?useWithSubtypes : bool, ?skipHeaderWrite : bool, ?bypass : bool, ?skipVisit : bool) =
        new CompositePickler<'T>(reader, writer, picklerInfo, cacheByRef = cacheByRef, ?useWithSubtypes = useWithSubtypes, 
                                                    ?skipHeaderWrite = skipHeaderWrite, ?bypass = bypass, ?skipVisit = skipVisit) :> Pickler<'T>

    static member ObjectPickler =
        CompositePickler.Create<obj>((fun _ _ -> obj ()), (fun _ _ _ -> ()), PicklerInfo.Object, cacheByRef = true)