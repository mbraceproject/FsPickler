namespace Nessos.FsPickler

    open System
    open System.Runtime.CompilerServices
    open System.Runtime.Serialization

    open Nessos.FsPickler.Reflection

    [<AutoSerializable(false)>]
    type internal CompositePickler<'T> =
        inherit Pickler<'T>

        val mutable private m_IsInitialized : bool

        // stores the supertype pickler, if generated using subtype resolution
        val mutable private m_NestedPickler : Pickler option

        val mutable private m_Writer : WriteState -> 'T -> unit
        val mutable private m_Reader : ReadState -> 'T

        val mutable private m_TypeInfo : TypeKind
        val mutable private m_PicklerInfo : PicklerInfo

        val mutable private m_IsRecursiveType : bool
        val mutable private m_IsOfFixedSize : bool

        val mutable private m_IsCacheByRef : bool
        val mutable private m_UseWithSubtypes : bool

        new (reader, writer, nested : Pickler option, picklerInfo, cacheByRef, useWithSubtypes) =
            let TypeInfo = TypeInfo.compute typeof<'T>
            let isFixedSize = isOfFixedSize typeof<'T>
            let isRecursive =
#if OPTIMIZE_FSHARP
                isRecursiveType true typeof<'T>
#else
                isRecursiveType false typeof<'T>
#endif
            {
                inherit Pickler<'T> ()

                m_IsInitialized = true

                m_NestedPickler = nested

                m_Writer = writer
                m_Reader = reader

                m_TypeInfo = TypeInfo
                m_PicklerInfo = picklerInfo

                m_IsRecursiveType = isRecursive
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

                m_TypeInfo = Unchecked.defaultof<_>
                m_PicklerInfo = Unchecked.defaultof<_>
 
                m_IsRecursiveType = Unchecked.defaultof<_>
                m_IsOfFixedSize = Unchecked.defaultof<_>

                m_IsCacheByRef = Unchecked.defaultof<_>
                m_UseWithSubtypes = Unchecked.defaultof<_>
            }

        override p.ImplementationType = 
            match p.m_NestedPickler with 
            | None -> typeof<'T> 
            | Some p -> p.Type

        override p.TypeInfo = p.m_TypeInfo
        override p.PicklerInfo = p.m_PicklerInfo

        override p.IsRecursiveType = p.m_IsRecursiveType
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
                    p.m_TypeInfo <- p'.m_TypeInfo
                    p.m_PicklerInfo <- p'.m_PicklerInfo
                    p.m_IsCacheByRef <- p'.m_IsCacheByRef
                    p.m_UseWithSubtypes <- p'.m_UseWithSubtypes
                    p.m_IsRecursiveType <- p'.m_IsRecursiveType
                    p.m_IsOfFixedSize <- p'.m_IsOfFixedSize
                    p.m_Writer <- p'.m_Writer
                    p.m_Reader <- p'.m_Reader
                    p.m_NestedPickler <- p'.m_NestedPickler
                    p.m_IsInitialized <- true

            | _ -> invalidOp <| sprintf "Source pickler is of invalid type (%O)." p'.Type


        override p.UntypedWrite (state:WriteState) (tag:string) (value:obj) =
            if state.NextObjectIsSubtype then
                state.NextObjectIsSubtype <- false
                p.m_Writer state (fastUnbox value)
            else
                p.Write state tag (fastUnbox value)

        override p.UntypedRead (state:ReadState) (tag:string) =
            if state.NextObjectIsSubtype then
                state.NextObjectIsSubtype <- false
                p.m_Reader state :> obj
            else
                p.Read state tag :> obj

        override p.Write (state : WriteState) (tag : string) (value : 'T) =

            let formatter = state.Formatter

            let inline beginWriteObject tag flags =
                formatter.BeginWriteObject p.m_TypeInfo p.m_PicklerInfo tag flags

            // writes a non-null instance of a reference type
#if DEBUG
            let writeObject () =
#else
            let inline writeObject () =
#endif
                if p.m_TypeInfo <= TypeKind.Sealed || p.m_UseWithSubtypes then
                    beginWriteObject tag ObjectFlags.None
                    p.m_Writer state value
                    formatter.EndWriteObject ()
                else
                    // object might be of proper subtype, perform reflection resolution
                    let t0 = value.GetType()
                    if t0 <> p.Type then
                        let subPickler = state.PicklerResolver.Resolve t0
                        beginWriteObject tag ObjectFlags.IsProperSubtype
                        state.TypePickler.Write state "subtype" t0
                        state.NextObjectIsSubtype <- true
                        subPickler.UntypedWrite state tag value
                        formatter.EndWriteObject ()
                    else
                        beginWriteObject tag ObjectFlags.None
                        p.m_Writer state value
                        formatter.EndWriteObject ()

            try
                if p.m_TypeInfo = TypeKind.Value then
                    beginWriteObject tag ObjectFlags.None
                    p.m_Writer state value
                    formatter.EndWriteObject ()

                elif obj.ReferenceEquals(value, null) then
                    beginWriteObject tag ObjectFlags.IsNull
                    formatter.EndWriteObject ()

                else

#if PROTECT_STACK_OVERFLOWS
                do RuntimeHelpers.EnsureSufficientExecutionStack()
#endif

                if p.m_IsCacheByRef || p.m_IsRecursiveType then
                    let id, firstOccurence = state.ObjectIdGenerator.GetId value

                    let cyclicObjects = state.CyclicObjectSet
                    let objStack = state.ObjectStack

                    if firstOccurence then
                        if p.m_IsRecursiveType then 
                            // push id to the symbolic stack to detect cyclic objects during traversal
                            objStack.Push id

                            writeObject ()

                            objStack.Pop () |> ignore
                            cyclicObjects.Remove id |> ignore
                        else
                            writeObject ()

                    elif p.m_IsRecursiveType && objStack.Contains id && not <| cyclicObjects.Contains id then
                        // came across cyclic object, record fixup-related data
                        // cyclic objects are handled once per instance
                        // instances of cyclic arrays are handled differently than other reference types

                        do cyclicObjects.Add(id) |> ignore
                    
                        if p.m_TypeInfo = TypeKind.Array then
                            beginWriteObject tag ObjectFlags.IsCachedInstance

                        elif p.m_TypeInfo <= TypeKind.Sealed || p.m_UseWithSubtypes then
                            beginWriteObject tag ObjectFlags.IsCyclicInstance
                        else
                            let t = value.GetType()

                            if t.IsArray then
                                beginWriteObject tag ObjectFlags.IsCachedInstance
                            elif t <> p.Type then
                                beginWriteObject tag (ObjectFlags.IsCyclicInstance ||| ObjectFlags.IsProperSubtype)
                                state.TypePickler.Write state "subtype" t
                            else
                                beginWriteObject tag ObjectFlags.IsCyclicInstance

                        formatter.WriteInt64 "id" id
                        formatter.EndWriteObject()
                    else
                        beginWriteObject tag ObjectFlags.IsCachedInstance
                        formatter.WriteInt64 "id" id
                        formatter.EndWriteObject()
                else
                    writeObject ()

            with 
            | :? SerializationException -> reraise ()
            | e -> raise <| new SerializationException(sprintf "Error serializing instance of type '%O'." typeof<'T>, e)


        override p.Read (state : ReadState) (tag : string) =

#if DEBUG
            let readObject flags =
#else
            let inline readObject flags =
#endif
                if ObjectFlags.hasFlag flags ObjectFlags.IsProperSubtype then
                    let t = state.TypePickler.Read state "subtype"
                    let subPickler = state.PicklerResolver.Resolve t
                    state.NextObjectIsSubtype <- true
                    subPickler.UntypedRead state tag |> fastUnbox<'T>
                else
                    p.m_Reader state

            try
                let formatter = state.Formatter

                let flags = formatter.BeginReadObject p.m_TypeInfo p.m_PicklerInfo tag

                if ObjectFlags.hasFlag flags ObjectFlags.IsNull then 
                    formatter.EndReadObject ()
                    fastUnbox<'T> null

                elif p.m_TypeInfo = TypeKind.Value then 
                    let value = p.m_Reader state
                    formatter.EndReadObject ()
                    value

                elif ObjectFlags.hasFlag flags ObjectFlags.IsCyclicInstance then
                    // came across a nested instance of a cyclic object
                    // add an uninitialized object to the cache and schedule
                    // reflection-based fixup at the root level.
                    let reflectedType =
                        if ObjectFlags.hasFlag flags ObjectFlags.IsProperSubtype then state.TypePickler.Read state "subtype"
                        else typeof<'T>

                    let id = formatter.ReadInt64 "id"

                    let value = FormatterServices.GetUninitializedObject(reflectedType)

                    // register a fixup operation & cache
                    state.FixupIndex.Add(id, (reflectedType,value))
                    state.ObjectCache.Add(id, value)

                    formatter.EndReadObject()

                    fastUnbox<'T> value

                elif ObjectFlags.hasFlag flags ObjectFlags.IsCachedInstance then
                    let id = formatter.ReadInt64 "id"
                    formatter.EndReadObject ()
                    state.ObjectCache.[id] |> fastUnbox<'T>

                elif p.m_IsCacheByRef || p.m_IsRecursiveType then
                    let isArray = 
                        match p.m_TypeInfo with
                        | TypeKind.Array | TypeKind.ArrayCompatible -> true
                        | _ -> false

                    let id = state.GetObjectId(isArray)

                    let value = readObject flags

                    formatter.EndReadObject()

                    if p.m_IsRecursiveType then 
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

        static member ObjectPickler =
            CompositePickler.Create<obj>((fun _ -> obj ()), (fun _ _ -> ()), PicklerInfo.Object, cacheByRef = true, useWithSubtypes = false)