namespace Nessos.FsPickler

    open System
    open System.IO
    open System.Text
    open System.Collections
    open System.Collections.Generic
    open System.Runtime.CompilerServices
    open System.Runtime.Serialization

    open Nessos.FsPickler.Utils
    open Nessos.FsPickler.Header


    [<AutoSerializable(false)>]
    [<AbstractClass>]
    type Pickler =

        val private declared_type : Type

        val mutable private m_isInitialized : bool

        val mutable private m_pickler_type : Type
        val mutable private m_TypeKind : TypeKind
        val mutable private m_picklerFlags : PicklerFlags
        val mutable private m_picklerInfo : PicklerInfo
        
        val mutable private m_is_cyclic_type : bool
        val mutable private m_is_fixed_size : bool

        val mutable private m_isCacheByRef : bool
        val mutable private m_useWithSubtypes : bool

        val mutable private m_cache_id : string

        internal new (t : Type) =
            {
                declared_type = t

                m_isInitialized = false

                m_is_cyclic_type = Unchecked.defaultof<_>
                m_is_fixed_size = Unchecked.defaultof<_>
                m_pickler_type = Unchecked.defaultof<_>
                m_TypeKind = Unchecked.defaultof<_>
                m_picklerFlags = Unchecked.defaultof<_>
                m_picklerInfo = Unchecked.defaultof<_>
                m_isCacheByRef = Unchecked.defaultof<_>
                m_useWithSubtypes = Unchecked.defaultof<_>

                m_cache_id = null
            }

        internal new (declaredType : Type, picklerType : Type, picklerInfo, isCacheByRef, useWithSubtypes) =
            assert(picklerType.IsAssignableFrom declaredType)

            let isCyclic =
#if OPTIMIZE_FSHARP
                isCyclicType true declaredType
#else
                isCyclicType false declaredType
#endif
            let tyKind = computeTypeKind picklerType
            let isFixedSize = isOfFixedSize declaredType

            {
                declared_type = declaredType

                m_is_cyclic_type = isCyclic
                m_is_fixed_size = isFixedSize

                m_isInitialized = true

                m_pickler_type = picklerType
                m_TypeKind = tyKind
                m_picklerFlags = computePicklerFlags picklerInfo tyKind isCacheByRef useWithSubtypes isCyclic isFixedSize declaredType

                m_picklerInfo = picklerInfo
                m_isCacheByRef = isCacheByRef
                m_useWithSubtypes = useWithSubtypes

                m_cache_id = null
            }

        member p.Type = p.declared_type

        member internal p.TypeKind = p.m_TypeKind
        member internal p.PicklerFlags = p.m_picklerFlags

        member p.CacheId
            with get () = p.m_cache_id
            and internal set id = p.m_cache_id <- id

        member p.IsCacheByRef =
            if p.m_isInitialized then  p.m_isCacheByRef
            else
                invalidOp "Attempting to consume pickler at initialization time."

        member p.UseWithSubtypes =
            if p.m_isInitialized then  p.m_useWithSubtypes
            else
                invalidOp "Attempting to consume pickler at initialization time."

        member p.IsCyclicType = 
            if p.m_isInitialized then  p.m_is_cyclic_type
            else
                invalidOp "Attempting to consume pickler at initialization time."
                
        member p.IsFixedSize =
            if p.m_isInitialized then  p.m_is_fixed_size
            else
                invalidOp "Attempting to consume pickler at initialization time."

        member p.PicklerType =
            if p.m_isInitialized then p.m_pickler_type
            else
                invalidOp "Attempting to consume pickler at initialization time."

        member p.PicklerInfo =
            if p.m_isInitialized then p.m_picklerInfo
            else
                invalidOp "Attempting to consume pickler at initialization time."


        member internal p.IsInitialized = p.m_isInitialized

        abstract member UntypedWrite : Writer * value:obj -> unit
        abstract member UntypedRead : Reader -> obj

        abstract member ManagedWrite : Writer * tag:string * value:obj -> unit
        abstract member ManagedRead  : Reader * tag:string -> obj

        abstract member WriteRootObject : Writer * id : string * value : obj -> unit
        abstract member ReadRootObject : Reader * id : string -> obj

//        abstract member WriteSequence : Writer * id : string * sequence : IEnumerable -> int
//        abstract member ReadSequence : Reader * id : string * length : int -> IEnumerator

        abstract member Cast<'S> : unit -> Pickler<'S>
        abstract member ClonePickler : unit -> Pickler

        abstract member InitializeFrom : Pickler -> unit
        default p.InitializeFrom(f' : Pickler) : unit =
            if p.m_isInitialized then
                invalidOp "Target pickler has already been initialized."
            elif not f'.m_isInitialized then 
                invalidOp "Attempting to consume pickler at initialization time."
            elif p.Type <> f'.Type && not (f'.Type.IsAssignableFrom(p.Type) && f'.UseWithSubtypes) then
                raise <| new InvalidCastException(sprintf "Cannot cast pickler from '%O' to '%O'." f'.Type p.Type)
            else
                p.m_pickler_type <- f'.m_pickler_type
                p.m_cache_id <- f'.m_cache_id
                p.m_picklerFlags <- f'.m_picklerFlags
                p.m_TypeKind <- f'.m_TypeKind
                p.m_picklerInfo <- f'.m_picklerInfo
                p.m_isCacheByRef <- f'.m_isCacheByRef
                p.m_useWithSubtypes <- f'.m_useWithSubtypes
                p.m_is_cyclic_type <- f'.m_is_cyclic_type
                p.m_is_fixed_size <- f'.m_is_fixed_size
                p.m_isInitialized <- true

    and [<Sealed>]
        [<AutoSerializable(false)>] 
        Pickler<'T> =

        inherit Pickler
        
        val mutable private m_writer : Writer -> 'T -> unit
        val mutable private m_reader : Reader -> 'T

        // stores the supertype pickler, if generated using subtype resolution
        val mutable private m_nested_pickler : Pickler option

        internal new (reader, writer, picklerInfo, cacheByRef, useWithSubtypes) = 
            { 
                inherit Pickler(typeof<'T>, typeof<'T>, picklerInfo, cacheByRef, useWithSubtypes) ;

                m_writer = writer
                m_reader = reader

                m_nested_pickler = None
            }

        // constructor called by Cast<_> method
        private new (nested : Pickler, reader, writer) =
            { 
                inherit Pickler(typeof<'T>, nested.Type, nested.PicklerInfo, nested.IsCacheByRef, nested.UseWithSubtypes) ;
                
                m_writer = writer
                m_reader = reader

                m_nested_pickler = Some nested
            }

        internal new () = 
            {
                inherit Pickler(typeof<'T>)
                
                m_writer = fun _ _ -> invalidOp "Attempting to consume pickler at initialization time."
                m_reader = fun _ -> invalidOp "Attempting to consume pickler at initialization time."

                m_nested_pickler = None
            }

        override p.UntypedWrite (w : Writer, value : obj) = p.m_writer w (fastUnbox<'T> value)
        override p.UntypedRead (r : Reader) = p.m_reader r :> obj

        override p.ManagedWrite (w : Writer, tag:string, value:obj) = w.Write(p, tag, fastUnbox<'T> value)
        override p.ManagedRead (r : Reader, tag:string) = r.Read(p, tag) :> obj

        override p.WriteRootObject (w : Writer, id, o : obj) = w.WriteRootObject(p, id, o :?> 'T)
        override p.ReadRootObject (r : Reader, id) = r.ReadRootObject (p, id) :> obj
//        override p.WriteSequence (w : Writer, id, e : IEnumerable) = w.WriteSequence(f, id, e :?> seq<'T>)
//        override p.ReadSequence (r : Reader, id, length : int) = r.ReadSequence(f, id, length) :> IEnumerator

        override p.ClonePickler () =
            if p.IsInitialized then
                new Pickler<'T>(p.m_reader, p.m_writer, p.PicklerInfo, p.IsCacheByRef, p.UseWithSubtypes) :> Pickler
            else
                invalidOp "Attempting to consume pickler at initialization time."

        override p.Cast<'S> () =
            match p.m_nested_pickler with
            | Some nested -> nested.Cast<'S> ()
            | None ->
                if not p.IsInitialized then invalidOp "Attempting to consume pickler at initialization time."
                elif typeof<'T> = typeof<'S> then fastUnbox<Pickler<'S>> p
                elif typeof<'T>.IsAssignableFrom typeof<'S> && p.UseWithSubtypes then
                    let writer = let wf = p.m_writer in fun w x -> wf w (fastUnbox<'T> x)
                    let reader = let rf = p.m_reader in fun r -> fastUnbox<'S> (rf r)

                    new Pickler<'S>(p, reader, writer)
                else
                    let msg = sprintf "Cannot cast pickler of type '%O' to '%O'." typeof<'T> typeof<'S>
                    raise <| new InvalidCastException(msg)
                

        override p.InitializeFrom(f' : Pickler) : unit =
            let f' = f'.Cast<'T> ()
            base.InitializeFrom f'
            p.m_nested_pickler <- f'.m_nested_pickler
            p.m_writer <- f'.m_writer
            p.m_reader <- f'.m_reader
            
        member internal p.Write = p.m_writer
        member internal p.Read = p.m_reader


    and IPicklerResolver =
        abstract Resolve : Type -> Pickler
        abstract Resolve<'T> : unit -> Pickler<'T>

    and [<AutoSerializable(false)>]
        Writer internal (formatter : IPickleFormatWriter, resolver : IPicklerResolver, ?streamingContext) =
        
//        do if not stream.CanWrite then invalidOp "Cannot write to stream."

        // using UTF8 gives an observed performance improvement ~200%
//        let encoding = defaultArg encoding Encoding.UTF8

//        let bw = BinaryWriter.Create(stream, encoding, defaultArg leaveOpen true)
        let sc = match streamingContext with None -> new StreamingContext() | Some sc -> sc
        let mutable idGen = new ObjectIDGenerator()
        let objStack = new Stack<int64> ()
        let cyclicObjects = new HashSet<int64> ()

        let tyPickler = resolver.Resolve<Type> ()

        member w.Formatter = formatter

        member w.StreamingContext = sc

        member internal w.Resolver = resolver

        // the primary serialization routine; handles all the caching, subtype resolution logic, etc
        member w.Write<'T> (pickler : Pickler<'T>, tag : string, x : 'T) =
//
//            let (*inline*) writeHeader tag (flags : byte) =
//                let header = ObjHeader.create pickler.PicklerFlags flags
//                formatter.BeginWriteObject tag header

            // ad-hoc System.Type caching
            let (*inline*) writeType (t : Type) =
                let id, firstOccurence = idGen.GetId t
                if firstOccurence then
                    formatter.BeginWriteObject "subtype" ObjectFlags.IsNewCachedInstance
                    tyPickler.Write w t
                    formatter.EndWriteObject ()
                else
                    formatter.BeginWriteObject "subtype" ObjectFlags.IsOldCachedInstance
                    formatter.WriteInt64 "id" id
                    formatter.EndWriteObject ()

            let (*inline*) writeObject header =
                if pickler.TypeKind <= TypeKind.Sealed || pickler.UseWithSubtypes then
                    formatter.BeginWriteObject tag header
                    pickler.Write w x
                    formatter.EndWriteObject ()
                else
                    // object might be of proper subtype, perform reflection resolution
                    let t0 = x.GetType()
                    if t0 <> pickler.Type then
                        let pickler' = resolver.Resolve t0
                        formatter.BeginWriteObject tag (header ||| ObjectFlags.IsProperSubtype)
                        writeType t0
                        pickler'.UntypedWrite(w, x)
                        formatter.EndWriteObject ()
                    else
                        formatter.BeginWriteObject tag header
                        pickler.Write w x
                        formatter.EndWriteObject ()

            try
                if pickler.TypeKind <= TypeKind.String then
                    formatter.BeginWriteObject tag ObjectFlags.IsPrimitive
                    pickler.Write w x
                    formatter.EndWriteObject ()

                if pickler.TypeKind <= TypeKind.Value then
                    formatter.BeginWriteObject tag ObjectFlags.IsValue
                    pickler.Write w x
                    formatter.EndWriteObject ()

                elif obj.ReferenceEquals(x, null) then
                    formatter.BeginWriteObject tag ObjectFlags.IsNull
                    pickler.Write w x
                    formatter.EndWriteObject ()

                else

#if PROTECT_STACK_OVERFLOWS
                do RuntimeHelpers.EnsureSufficientExecutionStack()
#endif

                if pickler.IsCacheByRef || pickler.IsCyclicType then
                    let id, firstOccurence = idGen.GetId x

                    if firstOccurence then
                        if pickler.IsCyclicType then 
                            // push id to the symbolic stack to detect cyclic objects during traversal
                            objStack.Push id

                            writeObject ObjectFlags.IsNewCachedInstance

                            objStack.Pop () |> ignore
                            cyclicObjects.Remove id |> ignore
                        else
                            writeObject ObjectFlags.IsNewCachedInstance

                    elif pickler.IsCyclicType && objStack.Contains id && not <| cyclicObjects.Contains id then
                        // came across cyclic object, record fixup-related data
                        // cyclic objects are handled once per instance
                        // instances of cyclic arrays are handled differently than other reference types

                        do cyclicObjects.Add(id) |> ignore
                    
                        if pickler.TypeKind = TypeKind.Array then
                              formatter.BeginWriteObject tag ObjectFlags.IsOldCachedInstance
                        elif pickler.TypeKind <= TypeKind.Sealed || pickler.UseWithSubtypes then
                            formatter.BeginWriteObject tag ObjectFlags.IsCyclicInstance
                        else
                            let t = x.GetType()

                            if t.IsArray then
                                formatter.BeginWriteObject tag ObjectFlags.IsOldCachedInstance
                            elif t <> pickler.Type then
                                formatter.BeginWriteObject tag (ObjectFlags.IsCyclicInstance ||| ObjectFlags.IsProperSubtype)
                                writeType t
                            else
                                formatter.BeginWriteObject tag ObjectFlags.IsCyclicInstance

                        formatter.WriteInt64 "id" id
                        formatter.EndWriteObject()
                    else
                        formatter.BeginWriteObject tag ObjectFlags.IsOldCachedInstance
                        formatter.WriteInt64 "id" id
                        formatter.EndWriteObject()
                else
                    writeObject ObjectFlags.Zero

            with 
            | :? SerializationException -> reraise ()
            | e -> raise <| new SerializationException(sprintf "Error serializing instance of type '%O'." typeof<'T>, e)

        member internal w.WriteRootObject(pickler : Pickler<'T>, id : string, x : 'T) =
            formatter.BeginWriteRoot id
            w.Write(pickler, "root", x)
            formatter.EndWriteRoot ()

//        // efficient sequence serialization; must be used as top-level operation only
//        member internal w.WriteSequence<'T>(pickler : Pickler<'T>, id : string, xs : seq<'T>) : int =
//            let inline flushState () =
//                idGen <- new ObjectIDGenerator()
//                
//            let isPrimitive = pickler.TypeKind = TypeKind.Primitive
//            let isNonAtomic = pickler.PicklerInfo <> PicklerInfo.Atomic
//
//            let inline write idx (x : 'T) =
//                if isPrimitive then pickler.Write w x
//                elif isNonAtomic then
//                    if idx % sequenceCounterResetThreshold = 0 then
//                        flushState ()
//                    w.Write(pickler, x)
//                elif obj.ReferenceEquals(x, null) then
//                    bw.Write true
//                else
//                    bw.Write false 
//                    pickler.Write w x
//
//            // write id
//            bw.Write id
//            // write sequence header
//            bw.Write(ObjHeader.create pickler.PicklerFlags ObjHeader.isSequenceHeader)
//            
//            // specialize enumeration
//            match xs with
//            | :? ('T []) as array ->
//                let n = array.Length
//                for i = 0 to n - 1 do
//                    write i array.[i]
//                n
//            | :? ('T list) as list ->
//                let rec writeLst i lst =
//                    match lst with
//                    | [] -> i
//                    | hd :: tl -> write i hd ; writeLst (i+1) tl
//
//                writeLst 0 list
//            | _ ->
//                let mutable i = 0
//                for x in xs do
//                    write i x
//                    i <- i + 1
//                i
//
//        interface IDisposable with
//            member __.Dispose () = bw.Dispose ()

    and [<AutoSerializable(false)>] 
        Reader internal (formatter : IPickleFormatReader, resolver : IPicklerResolver, ?streamingContext) =

//        do if not stream.CanRead then invalidOp "Cannot read from stream."

//        // using UTF8 gives an observed performance improvement ~200%
//        let encoding = defaultArg encoding Encoding.UTF8

//        let br = BinaryReader.Create(stream, encoding, defaultArg leaveOpen true)
        let sc = match streamingContext with None -> new StreamingContext() | Some sc -> sc
        let objCache = new Dictionary<int64, obj> ()
        let fixupIndex = new Dictionary<int64, Type * obj> ()
        let tyPickler = resolver.Resolve<Type> ()

        let mutable counter = 1L
        // keeps track of a currently deserialized array
        let mutable currentDeserializedArrayId = 0L

        // Arrays are special reference types;
        // Unlike other objects, their proper initialization depends on
        // setting the correct rank & dimensions.
        // Therefore, early registration in the event of a cyclic array
        // is a responsibility of the pickler. This method gives access
        // to the object cache for the array pickler.
        member internal r.EarlyRegisterArray (a : Array) = 
            objCache.Add(currentDeserializedArrayId, a)

        member r.Formatter = formatter

        member r.StreamingContext = sc

        member internal r.Resolver = resolver


        // the primary deserialization routine; handles all the caching, subtype resolution logic, etc
        member r.Read(pickler : Pickler<'T>, tag : string) : 'T =

            // ad-hoc System.Type caching
            let (*inline*) readType () =
                let flags = formatter.BeginReadObject "subtype"
                if ObjectFlags.hasFlag flags ObjectFlags.IsNewCachedInstance then
                    let id = counter
                    counter <- counter + 1L
                    let t = tyPickler.Read r
                    objCache.Add(id, t)
                    formatter.EndReadObject()
                    t
                else
                    let id = formatter.ReadInt64 "id"
                    formatter.EndReadObject()
                    objCache.[id] |> fastUnbox<Type>

            let (*inline*) readObject flags =
                if ObjectFlags.hasFlag flags ObjectFlags.IsProperSubtype then
                    let t = readType ()
                    let pickler' = resolver.Resolve t
                    pickler'.UntypedRead r |> fastUnbox<'T>
                else
                    pickler.Read r

            try
                let flags = formatter.BeginReadObject tag

                if ObjectFlags.hasFlag flags ObjectFlags.IsNull then 
                    formatter.EndReadObject ()
                    fastUnbox<'T> null

                elif pickler.TypeKind <= TypeKind.Value then 
                    let x = pickler.Read r
                    formatter.EndReadObject ()
                    x

                elif ObjectFlags.hasFlag flags ObjectFlags.IsCyclicInstance then
                    // came across a nested instance of a cyclic object
                    // add an uninitialized object to the cache and schedule
                    // reflection-based fixup at the root level.
                    let t =
                        if ObjectFlags.hasFlag flags ObjectFlags.IsProperSubtype then readType ()
                        else pickler.Type

                    let id = formatter.ReadInt64 "id"

                    let x = FormatterServices.GetUninitializedObject(t)

                    // register a fixup operation & cache
                    fixupIndex.Add(id, (t,x))
                    objCache.Add(id, x)

                    formatter.EndReadObject()

                    fastUnbox<'T> x

                elif ObjectFlags.hasFlag flags ObjectFlags.IsNewCachedInstance then
                    let id = counter
                    if pickler.TypeKind = TypeKind.Array || pickler.TypeKind = TypeKind.ArrayCompatible then
                        currentDeserializedArrayId <- id
                    counter <- counter + 1L

                    let x = readObject flags

                    formatter.EndReadObject()

                    if pickler.IsCyclicType then 
                        let found, contents = fixupIndex.TryGetValue id

                        if found then
                            // deserialization reached root level of a cyclic object
                            // perform fixup by doing reflection-based field copying
                            let t,o = contents
                            do ShallowObjectCopier.Copy t x o
                            fastUnbox<'T> o
                        else
                            objCache.[id] <- x ; x
                    else
                        objCache.[id] <- x ; x

                elif ObjectFlags.hasFlag flags ObjectFlags.IsOldCachedInstance then
                    let id = formatter.ReadInt64 "id"
                    formatter.EndReadObject ()
                    objCache.[id] |> fastUnbox<'T>
                else
                    let x = readObject flags
                    formatter.EndReadObject ()
                    x

            with 
            | :? SerializationException -> reraise ()
            | e -> raise <| new SerializationException(sprintf "Error deserializing instance of type '%O'." typeof<'T>, e)

        member internal r.ReadRootObject(pickler : Pickler<'T>, id : string) =
            let id' = formatter.BeginReadRoot()
            if id <> id' then
                let msg = sprintf "FsPickler: Root object is of type '%s', expected '%s'." id' id
                raise <| new SerializationException(msg)

            let x = r.Read(pickler, "root")
            formatter.EndReadRoot ()
            x


//        // efficient sequence deserialization; must be used as top-level operation only
//        member internal r.ReadSequence<'T> (pickler : Pickler<'T>, id : string, length : int) =
//            let inline flushState () =
//                objCache.Clear ()
//                counter <- 1L
//
//            let isPrimitive = pickler.TypeKind = TypeKind.Primitive
//            let isNonAtomic = pickler.PicklerInfo <> PicklerInfo.Atomic
//
//            let read idx =
//                if isPrimitive then pickler.Read r
//                elif isNonAtomic then
//                    if idx % sequenceCounterResetThreshold = 0 then
//                        flushState ()
//                    r.Read pickler
//                elif br.ReadBoolean () then fastUnbox<'T> null
//                else
//                    pickler.Read r
//
//            // read id
//            let id' = br.ReadString()
//            if id <> id' then
//                let msg = sprintf "FsPickler: Sequence is of type '%s', expected '%s'." id' id
//                raise <| new SerializationException(msg)
//            
//            // read object header
//            match ObjHeader.read pickler.Type pickler.PicklerFlags (br.ReadUInt32()) with
//            | ObjHeader.isSequenceHeader -> ()
//            | _ -> 
//                let msg = "FsPickler: invalid stream data; expected sequence serialization."
//                raise <| new SerializationException(msg)
//
//            let cnt = ref 0
//            let curr = ref Unchecked.defaultof<'T>
//            let initPos = stream.Position
//            {
//                new IEnumerator<'T> with
//                    member __.Current = !curr
//                    member __.Current = box !curr
//                    member __.Dispose () = (r :> IDisposable).Dispose ()
//                    member __.MoveNext () =
//                        if !cnt < length then
//                            curr := read !cnt
//                            incr cnt
//                            true
//                        else
//                            false
//
//                    member __.Reset () =
//                        stream.Position <- initPos
//                        curr := Unchecked.defaultof<'T>
//                        cnt := 0
//                        do flushState ()
//            }
//
//        interface IDisposable with
//            member __.Dispose () = br.Dispose ()
