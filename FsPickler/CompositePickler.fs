namespace Nessos.FsPickler

    open System
    open System.Runtime.CompilerServices
    open System.Runtime.Serialization

    open Nessos.FsPickler.Utils
    open Nessos.FsPickler.Header


    [<AutoSerializable(false)>]
    type internal CompositePickler<'T> =
        inherit Pickler<'T>

        val mutable private m_IsInitialized : bool

        // stores the supertype pickler, if generated using subtype resolution
        val mutable private m_NestedPickler : Pickler option

        val mutable private m_Writer : WriteState -> 'T -> unit
        val mutable private m_Reader : ReadState -> 'T

        val mutable private m_TypeKind : TypeKind
        val mutable private m_PicklerFlags : PicklerFlags
        val mutable private m_PicklerInfo : PicklerInfo

        val mutable private m_IsCyclicType : bool
        val mutable private m_IsOfFixedSize : bool

        val mutable private m_IsCacheByRef : bool
        val mutable private m_UseWithSubtypes : bool

        new (reader, writer, nested : Pickler option, picklerInfo, cacheByRef, useWithSubtypes) =

            let typeKind = computeTypeKind typeof<'T>
            let isFixedSize = isOfFixedSize typeof<'T>
            let isCyclic =
#if OPTIMIZE_FSHARP
                    isCyclicType true typeof<'T>
#else
                    isCyclicType false declaredType
#endif
            let picklerFlags = computePicklerFlags picklerInfo typeKind cacheByRef useWithSubtypes isCyclic isFixedSize typeof<'T>

            { 
                inherit Pickler<'T> ()

                m_IsInitialized = true

                m_NestedPickler = nested

                m_Writer = writer
                m_Reader = reader

                m_TypeKind = typeKind
                m_PicklerFlags = picklerFlags
                m_PicklerInfo = picklerInfo

                m_IsCyclicType = isCyclic
                m_IsOfFixedSize = isFixedSize

                m_IsCacheByRef = cacheByRef
                m_UseWithSubtypes = useWithSubtypes
            }

        new () = 
            {
                inherit Pickler<'T>()

                m_IsInitialized = false

                m_NestedPickler = None

                m_Writer = fun _ _ -> invalidOp "Attempting to consume pickler at initialization time."
                m_Reader = fun _ -> invalidOp "Attempting to consume pickler at initialization time."

                m_TypeKind = Unchecked.defaultof<_>
                m_PicklerFlags = Unchecked.defaultof<_>
                m_PicklerInfo = Unchecked.defaultof<_>
 
                m_IsCyclicType = Unchecked.defaultof<_>
                m_IsOfFixedSize = Unchecked.defaultof<_>

                m_IsCacheByRef = Unchecked.defaultof<_>
                m_UseWithSubtypes = Unchecked.defaultof<_>
            }

        override p.ImplementationType = 
            match p.m_NestedPickler with 
            | None -> typeof<'T> 
            | Some p -> p.Type

        override p.IsCyclicType = p.m_IsCyclicType
        override p.PicklerFlags = p.m_PicklerFlags
        override p.TypeKind = p.m_TypeKind
        override p.IsPrimitive = false
        override p.IsCacheByRef = p.m_IsCacheByRef
        override p.IsOfFixedSize = p.m_IsOfFixedSize
        override p.UseWithSubtypes = p.m_UseWithSubtypes

        override p.Clone () =
            if p.m_IsInitialized then
                new CompositePickler<'T>(p.m_Reader, p.m_Writer, p.m_NestedPickler, p.m_PicklerInfo, p.IsCacheByRef, p.m_UseWithSubtypes) :> Pickler
            else
                invalidOp "Attempting to consume pickler at initialization time."

        override p.Cast<'S> () =
            match p.m_NestedPickler with
            | Some nested -> nested.Cast<'S> ()
            | None ->
                if not p.m_IsInitialized then invalidOp "Pickler has not been initialized."
                elif typeof<'T> = typeof<'S> then fastUnbox<Pickler<'S>> p
                elif typeof<'T>.IsAssignableFrom typeof<'S> && p.m_UseWithSubtypes then
                    let writer = let wf = p.m_Writer in fun w x -> wf w (fastUnbox<'T> x)
                    let reader = let rf = p.m_Reader in fun r -> fastUnbox<'S> (rf r)

                    new CompositePickler<'S>(reader, writer, Some(p :> _), p.m_PicklerInfo, p.m_IsCacheByRef, p.m_UseWithSubtypes) :> Pickler<'S>
                else
                    let msg = sprintf "Cannot cast pickler of type '%O' to '%O'." typeof<'T> typeof<'S>
                    raise <| new InvalidCastException(msg)

        override p.InitializeFrom(p' : Pickler) : unit =
            match p'.Cast<'T> () with
            | :? CompositePickler<'T> as p' ->
                if p.m_IsInitialized then
                    invalidOp "Target pickler has already been initialized."
                if p.m_IsInitialized then
                    invalidOp "Target pickler has already been initialized."
                elif not p'.m_IsInitialized then 
                    invalidOp "Source pickler has not been initialized."
                else
                    p.m_PicklerFlags <- p'.m_PicklerFlags
                    p.m_TypeKind <- p'.m_TypeKind
                    p.m_PicklerInfo <- p'.m_PicklerInfo
                    p.m_IsCacheByRef <- p'.m_IsCacheByRef
                    p.m_UseWithSubtypes <- p'.m_UseWithSubtypes
                    p.m_IsCyclicType <- p'.m_IsCyclicType
                    p.m_IsOfFixedSize <- p'.m_IsOfFixedSize
                    p.m_Writer <- p'.m_Writer
                    p.m_Reader <- p'.m_Reader
                    p.m_NestedPickler <- p'.m_NestedPickler
                    p.m_IsInitialized <- true

            | _ -> invalidOp <| sprintf "Source pickler is of invalid type (%O)." p'.Type


        override p.UntypedWrite (state:WriteState) (tag:string) (value:obj) =
            if state.NextWriteIsSubtype then
                state.NextWriteIsSubtype <- false
                p.m_Writer state (fastUnbox value)
            else
                p.Write state tag (fastUnbox value)

        override p.UntypedRead (state:ReadState) (tag:string) =
            if state.NextWriteIsSubtype then
                state.NextWriteIsSubtype <- false
                p.m_Reader state :> obj
            else
                p.Read<'T> state tag :> obj

        override p.Write<'T> (state : WriteState) (tag : string) (value : 'T) =

            
            let formatter = state.Formatter
//            // ad-hoc System.Type caching
//            let inline writeType (t : Type) =
//                let id, firstOccurence = state.ObjectIdGenerator.GetId t
//                if firstOccurence then
//                    formatter.BeginWriteObject "subtype" ObjectFlags.IsNewCachedInstance
//                    state.TypePickler.Write w t
//                    formatter.EndWriteObject ()
//                else
//                    formatter.BeginWriteObject tyPickler "subtype" ObjectFlags.IsOldCachedInstance
//                    formatter.WriteInt64 "id" id
//                    formatter.EndWriteObject ()
#if DEBUG
            let writeObject header =
#else
            let inline writeObject header =
#endif
                if p.m_TypeKind <= TypeKind.Sealed || p.m_UseWithSubtypes then
                    formatter.BeginWriteObject p tag header
                    p.m_Writer state value
                    formatter.EndWriteObject ()
                else
                    // object might be of proper subtype, perform reflection resolution
                    let t0 = value.GetType()
                    if t0 <> typeof<'T> then
                        let subPickler = state.PicklerResolver.Resolve t0
                        formatter.BeginWriteObject p tag (header ||| ObjectFlags.IsProperSubtype)
                        state.TypePickler.Write state "subtype" t0
                        state.NextWriteIsSubtype <- true
                        subPickler.UntypedWrite state tag value
                        formatter.EndWriteObject ()
                    else
                        formatter.BeginWriteObject p tag header
                        p.m_Writer state value
                        formatter.EndWriteObject ()

            try
//                if pickler.TypeKind <= TypeKind.String then
//                    formatter.BeginWriteObject pickler tag ObjectFlags.IsPrimitive
//                    pickler.Write w x
//                    formatter.EndWriteObject ()

                if p.m_TypeKind = TypeKind.Value then
                    formatter.BeginWriteObject p tag ObjectFlags.IsValue
                    p.m_Writer state value
                    formatter.EndWriteObject ()

                elif obj.ReferenceEquals(value, null) then
                    formatter.BeginWriteObject p tag ObjectFlags.IsNull
                    formatter.EndWriteObject ()

                else

#if PROTECT_STACK_OVERFLOWS
                do RuntimeHelpers.EnsureSufficientExecutionStack()
#endif

                if p.m_IsCacheByRef || p.m_IsCyclicType then
                    let id, firstOccurence = state.ObjectIdGenerator.GetId value

                    let cyclicObjects = state.CyclicObjectSet
                    let objStack = state.ObjectStack

                    if firstOccurence then
                        if p.m_IsCyclicType then 
                            // push id to the symbolic stack to detect cyclic objects during traversal
                            objStack.Push id

                            writeObject ObjectFlags.IsNewCachedInstance

                            objStack.Pop () |> ignore
                            cyclicObjects.Remove id |> ignore
                        else
                            writeObject ObjectFlags.IsNewCachedInstance

                    elif p.m_IsCyclicType && objStack.Contains id && not <| cyclicObjects.Contains id then
                        // came across cyclic object, record fixup-related data
                        // cyclic objects are handled once per instance
                        // instances of cyclic arrays are handled differently than other reference types

                        do cyclicObjects.Add(id) |> ignore
                    
                        if p.m_TypeKind = TypeKind.Array then
                            formatter.BeginWriteObject p tag ObjectFlags.IsOldCachedInstance
                        elif p.m_TypeKind <= TypeKind.Sealed || p.m_UseWithSubtypes then
                            formatter.BeginWriteObject p tag ObjectFlags.IsCyclicInstance
                        else
                            let t = value.GetType()

                            if t.IsArray then
                                formatter.BeginWriteObject p tag ObjectFlags.IsOldCachedInstance
                            elif t <> typeof<'T> then
                                formatter.BeginWriteObject p tag (ObjectFlags.IsCyclicInstance ||| ObjectFlags.IsProperSubtype)
                                state.TypePickler.Write state "subtype" t
                            else
                                formatter.BeginWriteObject p tag ObjectFlags.IsCyclicInstance

                        formatter.WriteInt64 "id" id
                        formatter.EndWriteObject()
                    else
                        formatter.BeginWriteObject p tag ObjectFlags.IsOldCachedInstance
                        formatter.WriteInt64 "id" id
                        formatter.EndWriteObject()
                else
                    writeObject ObjectFlags.Zero

            with 
            | :? SerializationException -> reraise ()
            | e -> raise <| new SerializationException(sprintf "Error serializing instance of type '%O'." typeof<'T>, e)


        override p.Read (state : ReadState) (tag : string) =
        
//            let inline beginReadObject (pickler : Pickler) tag =
//                let objFlags = formatter.BeginReadObject(tag, &picklerFlags)
//                if picklerFlags <> pickler.PicklerFlags then
//                    if byte picklerFlags <> byte pickler.PicklerFlags then
//                        let msg = sprintf "FsPickler: next object is of unexpected type (anticipated %O)." pickler.Type
//                        raise <| new SerializationException(msg)
//                    else
//                        let msg = sprintf "FsPickler: object of type '%O' was serialized with incompatible pickler." pickler.Type
//                        raise <| new SerializationException(msg)
//                else
//                    objFlags

//            // ad-hoc System.Type caching
//            let inline readType () =
//                let flags = formatter.BeginReadObject tyPickler "subtype"
//                if ObjectFlags.hasFlag flags ObjectFlags.IsNewCachedInstance then
//                    let id = counter
//                    counter <- counter + 1L
//                    let t = tyPickler.Read r
//                    objCache.Add(id, t)
//                    formatter.EndReadObject()
//                    t
//                else
//                    let id = formatter.ReadInt64 "id"
//                    formatter.EndReadObject()
//                    objCache.[id] |> fastUnbox<Type>
#if DEBUG
            let readObject flags =
#else
            let inline readObject flags =
#endif
                if ObjectFlags.hasFlag flags ObjectFlags.IsProperSubtype then
                    let t = state.TypePickler.Read state "subtype"
                    let subPickler = state.PicklerResolver.Resolve t
                    state.NextWriteIsSubtype <- true
                    subPickler.UntypedRead state tag |> fastUnbox<'T>
                else
                    p.m_Reader state

            try
                let formatter = state.Formatter

                let flags = formatter.BeginReadObject p tag

                if ObjectFlags.hasFlag flags ObjectFlags.IsNull then 
                    formatter.EndReadObject ()
                    fastUnbox<'T> null

                elif p.m_TypeKind = TypeKind.Value then 
                    let value = p.m_Reader state
                    formatter.EndReadObject ()
                    value

                elif ObjectFlags.hasFlag flags ObjectFlags.IsCyclicInstance then
                    // came across a nested instance of a cyclic object
                    // add an uninitialized object to the cache and schedule
                    // reflection-based fixup at the root level.
                    let reflecteType =
                        if ObjectFlags.hasFlag flags ObjectFlags.IsProperSubtype then state.TypePickler.Read state "subtype"
                        else typeof<'T>

                    let id = formatter.ReadInt64 "id"

                    let value = FormatterServices.GetUninitializedObject(reflecteType)

                    // register a fixup operation & cache
                    state.FixupIndex.Add(id, (reflecteType,value))
                    state.ObjectCache.Add(id, value)

                    formatter.EndReadObject()

                    fastUnbox<'T> value

                elif ObjectFlags.hasFlag flags ObjectFlags.IsNewCachedInstance then
                    let isArray = 
                        match p.m_TypeKind with
                        | TypeKind.Array | TypeKind.ArrayCompatible -> true
                        | _ -> false

                    let id = state.GetObjectId(isArray)

                    let value = readObject flags

                    formatter.EndReadObject()

                    if p.m_IsCyclicType then 
                        let found, contents = state.FixupIndex.TryGetValue id

                        if found then
                            // deserialization reached root level of a cyclic object
                            // perform fixup by doing reflection-based field copying
                            let t,o = contents
                            do ShallowObjectCopier.Copy t value o
                            fastUnbox<'T> o
                        else
                            state.ObjectCache.[id] <- value ; value
                    else
                        state.ObjectCache.[id] <- value ; value

                elif ObjectFlags.hasFlag flags ObjectFlags.IsOldCachedInstance then
                    let id = formatter.ReadInt64 "id"
                    formatter.EndReadObject ()
                    state.ObjectCache.[id] |> fastUnbox<'T>
                else
                    let value = readObject flags
                    formatter.EndReadObject ()
                    value

            with 
            | :? SerializationException -> reraise ()
            | e -> raise <| new SerializationException(sprintf "Error deserializing instance of type '%O'." typeof<'T>, e)

    and internal CompositePickler =
        
        static member CreateUninitialized<'T>() = new CompositePickler<'T> () :> Pickler<'T>
        static member Create<'T>(reader, writer, picklerInfo, cacheByRef : bool, useWithSubtypes : bool) =
            new CompositePickler<'T>(reader, writer, None, picklerInfo, cacheByRef, useWithSubtypes) :> Pickler<'T>