namespace FsPickler

    open System
    open System.IO
    open System.Text
    open System.Collections
    open System.Collections.Generic
    open System.Runtime.CompilerServices
    open System.Runtime.Serialization

    open FsPickler.Utils
    open FsPickler.Header


    [<AutoSerializable(false)>]
    [<AbstractClass>]
    type Pickler =

        val private declared_type : Type
        val private is_cyclic_type : bool
        val private is_fixed_size : bool

        val mutable private m_isInitialized : bool

        val mutable private m_pickler_type : Type
        val mutable private m_TypeKind : TypeKind
        val mutable private m_typeHash : TypeHash

        val mutable private m_picklerInfo : PicklerInfo
        val mutable private m_isCacheByRef : bool
        val mutable private m_useWithSubtypes : bool

        val mutable private m_cache_id : string

        internal new (t : Type) =
            {
                declared_type = t ;
#if OPTIMIZE_FSHARP
                is_cyclic_type = isCyclicType true t ;
#else
                is_cyclic_type = isCyclicType false t ;
#endif
                is_fixed_size = isOfFixedSize t ;

                m_isInitialized = false ;

                m_pickler_type = Unchecked.defaultof<_> ; 
                m_TypeKind = Unchecked.defaultof<_> ; 
                m_typeHash = Unchecked.defaultof<_> ;
                m_picklerInfo = Unchecked.defaultof<_> ; 
                m_isCacheByRef = Unchecked.defaultof<_> ; 
                m_useWithSubtypes = Unchecked.defaultof<_> ;

                m_cache_id = null ;
            }

        internal new (declaredType : Type, picklerType : Type, picklerInfo, isCacheByRef, useWithSubtypes) =
            assert(picklerType.IsAssignableFrom declaredType)
            {
                declared_type = declaredType ; 
#if OPTIMIZE_FSHARP
                is_cyclic_type = isCyclicType true declaredType ;
#else
                is_cyclic_type = isCyclicType false declaredType ;
#endif
                is_fixed_size = isOfFixedSize declaredType ;

                m_isInitialized = true ;

                m_pickler_type = picklerType ; 
                m_TypeKind = computeTypeKind picklerType ; 
                m_typeHash = computeTypeHash picklerType ;

                m_picklerInfo = picklerInfo ;
                m_isCacheByRef = isCacheByRef ;
                m_useWithSubtypes = useWithSubtypes ;

                m_cache_id = null ;
            }

        member f.Type = f.declared_type
        member f.IsCyclicType = f.is_cyclic_type
        member f.IsFixedSize = f.is_fixed_size

        member internal f.TypeKind = f.m_TypeKind
        member internal f.TypeHash = f.m_typeHash

        member f.CacheId
            with get () = f.m_cache_id
            and internal set id = f.m_cache_id <- id

        member f.PicklerType =
            if f.m_isInitialized then f.m_pickler_type
            else
                invalidOp "Attempting to consume pickler at initialization time."

        member f.PicklerInfo =
            if f.m_isInitialized then f.m_picklerInfo
            else
                invalidOp "Attempting to consume pickler at initialization time."

        member f.IsCacheByRef = f.m_isCacheByRef
        member f.UseWithSubtypes = f.m_useWithSubtypes
        member internal f.IsInitialized = f.m_isInitialized

        abstract member UntypedWrite : Writer -> obj -> unit
        abstract member UntypedRead : Reader -> obj

        abstract member ManagedWrite : Writer -> obj -> unit
        abstract member ManagedRead : Reader -> obj

        abstract member WriteSequence : Writer * IEnumerable -> int
        abstract member ReadSequence : Reader * int -> IEnumerator

        abstract member Cast<'S> : unit -> Pickler<'S>
        abstract member ClonePickler : unit -> Pickler

        abstract member InitializeFrom : Pickler -> unit
        default f.InitializeFrom(f' : Pickler) : unit =
            if f.m_isInitialized then
                invalidOp "Target pickler has already been initialized."
            elif not f'.m_isInitialized then 
                invalidOp "Attempting to consume pickler at initialization time."
            elif f.Type <> f'.Type && not (f'.Type.IsAssignableFrom(f.Type) && f'.UseWithSubtypes) then
                raise <| new InvalidCastException(sprintf "Cannot cast pickler from '%O' to '%O'." f'.Type f.Type)
            else
                f.m_pickler_type <- f'.m_pickler_type
                f.m_cache_id <- f'.m_cache_id
                f.m_typeHash <- f'.m_typeHash
                f.m_TypeKind <- f'.m_TypeKind
                f.m_picklerInfo <- f'.m_picklerInfo
                f.m_isCacheByRef <- f'.m_isCacheByRef
                f.m_useWithSubtypes <- f'.m_useWithSubtypes
                f.m_isInitialized <- true

    and [<Sealed>]
        [<AutoSerializable(false)>] 
        Pickler<'T> =

        inherit Pickler
        
        val mutable private m_writer : Writer -> 'T -> unit
        val mutable private m_reader : Reader -> 'T

        // stores the supertype pickler, if generated using subtype resolution
        val mutable private m_nested_pickler : Pickler option

        internal new (reader, writer, picklerInfo, isCacheByRef, useWithSubtypes) = 
            { 
                inherit Pickler(typeof<'T>, typeof<'T>, picklerInfo, isCacheByRef, useWithSubtypes) ;

                m_writer = writer ;
                m_reader = reader ;

                m_nested_pickler = None ;
            }

        // constructor called by Cast<_> method
        private new (nested : Pickler, reader, writer) =
            { 
                inherit Pickler(typeof<'T>, nested.Type, nested.PicklerInfo, nested.IsCacheByRef, nested.UseWithSubtypes) ;
                
                m_writer = writer ;
                m_reader = reader ;

                m_nested_pickler = Some nested ;
            }

        internal new () = 
            {
                inherit Pickler(typeof<'T>) ;
                
                m_writer = fun _ _ -> invalidOp "Attempting to consume pickler at initialization time." ;
                m_reader = fun _ -> invalidOp "Attempting to consume pickler at initialization time." ;

                m_nested_pickler = None ;
            }

        override f.UntypedWrite (w : Writer) (o : obj) = f.m_writer w (fastUnbox<'T> o)
        override f.UntypedRead (r : Reader) = f.m_reader r :> obj
        override f.ManagedWrite (w : Writer) (o : obj) = w.Write(f, fastUnbox<'T> o)
        override f.ManagedRead (r : Reader) = r.Read f :> obj
        override f.WriteSequence (w : Writer, e : IEnumerable) = w.WriteSequence(f, e :?> seq<'T>)
        override f.ReadSequence (r : Reader, length : int) = r.ReadSequence(f, length) :> IEnumerator

        override f.ClonePickler () =
            if f.IsInitialized then
                new Pickler<'T>(f.m_reader, f.m_writer, f.PicklerInfo, f.IsCacheByRef, f.UseWithSubtypes) :> Pickler
            else
                invalidOp "Attempting to consume pickler at initialization time."

        override f.Cast<'S> () =
            match f.m_nested_pickler with
            | Some nested -> nested.Cast<'S> ()
            | None ->
                if not f.IsInitialized then invalidOp "Attempting to consume pickler at initialization time."
                elif typeof<'T> = typeof<'S> then fastUnbox<Pickler<'S>> f
                elif typeof<'T>.IsAssignableFrom typeof<'S> && f.UseWithSubtypes then
                    let writer = let wf = f.m_writer in fun w x -> wf w (fastUnbox<'T> x)
                    let reader = let rf = f.m_reader in fun r -> fastUnbox<'S> (rf r)

                    new Pickler<'S>(f, reader, writer)
                else
                    let msg = sprintf "Cannot cast pickler of type '%O' to '%O'." typeof<'T> typeof<'S>
                    raise <| new InvalidCastException(msg)
                

        override f.InitializeFrom(f' : Pickler) : unit =
            let f' = f'.Cast<'T> ()
            base.InitializeFrom f'
            f.m_nested_pickler <- f'.m_nested_pickler
            f.m_writer <- f'.m_writer
            f.m_reader <- f'.m_reader
            
        member internal f.Write = f.m_writer
        member internal f.Read = f.m_reader


    and IPicklerResolver =
        abstract UUId : string
        abstract Resolve : Type -> Pickler
        abstract Resolve<'T> : unit -> Pickler<'T>

    and [<AutoSerializable(false)>]
        Writer internal (stream : Stream, resolver : IPicklerResolver, ?streamingContext, ?leaveOpen, ?encoding) =
        
        do if not stream.CanWrite then invalidOp "Cannot write to stream."

        // using UTF8 gives an observed performance improvement ~200%
        let encoding = defaultArg encoding Encoding.UTF8

        let bw = new BinaryWriter(stream, encoding, defaultArg leaveOpen true)
        let sc = initStreamingContext streamingContext
        let mutable idGen = new ObjectIDGenerator()
        let objStack = new Stack<int64> ()
        let cyclicObjects = new SortedSet<int64> ()

        let tyPickler = resolver.Resolve<Type> ()

        member w.BinaryWriter = bw

        member w.StreamingContext = sc

        member internal w.Resolver = resolver

        // the primary serialization routine; handles all the caching, subtype resolution logic, etc
        member w.Write<'T> (fmt : Pickler<'T>, x : 'T) =

            let inline writeHeader (flags : byte) =
                bw.Write(ObjHeader.create fmt.TypeHash flags)

            let inline write header =
                if fmt.TypeKind <= TypeKind.Sealed || fmt.UseWithSubtypes then
                    writeHeader header
                    fmt.Write w x
                else
                    // object might be of proper subtype, perform reflection resolution
                    let t0 = x.GetType()
                    if t0 <> fmt.Type then
                        let fmt' = resolver.Resolve t0
                        writeHeader (header ||| ObjHeader.isProperSubtype)
                        tyPickler.Write w t0
                        fmt'.UntypedWrite w x
                    else
                        writeHeader header
                        fmt.Write w x

            if fmt.TypeKind <= TypeKind.Value then 
                writeHeader ObjHeader.empty
                fmt.Write w x

            elif obj.ReferenceEquals(x, null) then writeHeader ObjHeader.isNull else

#if PROTECT_STACK_OVERFLOWS
            do RuntimeHelpers.EnsureSufficientExecutionStack()
#endif

            let isCyclic = fmt.IsCyclicType

            if isCyclic || fmt.IsCacheByRef then
                let id, firstOccurence = idGen.GetId x

                if firstOccurence then
                    // push id to the symbolic stack to detect cyclic objects during traversal
                    if isCyclic then 
                        objStack.Push id

                    write ObjHeader.isNewCachedInstance

                    if isCyclic then
                        objStack.Pop () |> ignore
                        cyclicObjects.Remove id |> ignore

                elif isCyclic && objStack.Contains id && not <| cyclicObjects.Contains id then
                    // came across cyclic object, record fixup-related data
                    // cyclic objects are handled once per instance
                    // instances of cyclic arrays are handled differently than other reference types

                    do cyclicObjects.Add(id) |> ignore
                    
                    if fmt.TypeKind <= TypeKind.Sealed || fmt.UseWithSubtypes then
                        if fmt.TypeKind = TypeKind.Array then
                            writeHeader ObjHeader.isOldCachedInstance
                        else
                            writeHeader ObjHeader.isCyclicInstance
                    else
                        let t = x.GetType()

                        if t.IsArray then
                            writeHeader ObjHeader.isOldCachedInstance
                        elif t <> fmt.Type then
                            writeHeader (ObjHeader.isCyclicInstance ||| ObjHeader.isProperSubtype)
                            tyPickler.Write w t
                        else
                            writeHeader ObjHeader.isCyclicInstance

                    bw.Write id
                else
                    writeHeader ObjHeader.isOldCachedInstance
                    bw.Write id

            else
                write ObjHeader.empty

        // efficient sequence serialization; must be used as top-level operation only
        member internal w.WriteSequence<'T>(f : Pickler<'T>, xs : seq<'T>) : int =
            let inline flushState () =
                idGen <- new ObjectIDGenerator()
                
            let isPrimitive = f.TypeKind = TypeKind.Primitive
            let isNonAtomic = f.PicklerInfo <> PicklerInfo.Atomic

            let inline write idx (x : 'T) =
                if isPrimitive then f.Write w x
                elif isNonAtomic then
                    if idx % sequenceCounterResetThreshold = 0 then
                        flushState ()
                    w.Write(f, x)
                elif obj.ReferenceEquals(x, null) then
                    bw.Write true
                else
                    bw.Write false 
                    f.Write w x

            // write sequence header
            bw.Write(ObjHeader.create f.TypeHash ObjHeader.isSequenceHeader)
            
            // specialize enumeration
            match xs with
            | :? ('T []) as array ->
                let n = array.Length
                for i = 0 to n - 1 do
                    write i array.[i]
                n
            | :? ('T list) as list ->
                let rec writeLst i lst =
                    match lst with
                    | [] -> i
                    | hd :: tl -> write i hd ; writeLst (i+1) tl

                writeLst 0 list
            | _ ->
                let mutable i = 0
                for x in xs do
                    write i x
                    i <- i + 1
                i

        interface IDisposable with
            member __.Dispose () = bw.Dispose ()

    and [<AutoSerializable(false)>] 
        Reader internal (stream : Stream, resolver : IPicklerResolver, ?streamingContext : obj, ?leaveOpen, ?encoding) =

        do if not stream.CanRead then invalidOp "Cannot read from stream."

        // using UTF8 gives an observed performance improvement ~200%
        let encoding = defaultArg encoding Encoding.UTF8

        let br = new BinaryReader(stream, encoding, defaultArg leaveOpen true)
        let sc = initStreamingContext streamingContext
        let objCache = new Dictionary<int64, obj> ()
        let fixupIndex = new Dictionary<int64, Type * obj> ()
        let tyPickler = resolver.Resolve<Type> ()

        let mutable counter = 1L
        let mutable currentDeserializedObjectId = 0L

        // used internally for early registration of cyclic objects
        member internal r.EarlyRegisterObject (o : obj) = 
            objCache.Add(currentDeserializedObjectId, o)

        member r.BinaryReader = br

        member r.StreamingContext = sc

        member internal r.Resolver = resolver


        // the primary deserialization routine; handles all the caching, subtype resolution logic, etc
        member r.Read(fmt : Pickler<'T>) : 'T =

            let inline read flags =
                if ObjHeader.hasFlag flags ObjHeader.isProperSubtype then
                    let t = tyPickler.Read r
                    let fmt' = resolver.Resolve t
                    fmt'.UntypedRead r |> fastUnbox<'T>
                else
                    fmt.Read r

            let flags = ObjHeader.read fmt.Type fmt.TypeHash (br.ReadUInt32())

            if ObjHeader.hasFlag flags ObjHeader.isNull then fastUnbox<'T> null
            elif fmt.TypeKind <= TypeKind.Value then fmt.Read r
            elif ObjHeader.hasFlag flags ObjHeader.isCyclicInstance then
                // came across a nested instance of a cyclic object
                // crete an uninitialized object to the cache and schedule
                // reflection-based fixup at the root level.
                let t =
                    if ObjHeader.hasFlag flags ObjHeader.isProperSubtype then tyPickler.Read r
                    else fmt.Type

                let id = br.ReadInt64()

                let x = FormatterServices.GetUninitializedObject(t)

                // register a fixup operation & cache
                fixupIndex.Add(id, (t,x))
                objCache.Add(id, x)

                fastUnbox<'T> x

            elif ObjHeader.hasFlag flags ObjHeader.isNewCachedInstance then
                let id = counter
                currentDeserializedObjectId <- id
                counter <- counter + 1L

                let x = read flags

                if fmt.IsCyclicType then 
                    let found, contents = fixupIndex.TryGetValue id

                    if found then
                        // deserialization reached root level of a cyclic object
                        // perform fixup by doing reflection-based field copying
                        let t,o = contents
                        do shallowCopy t x o
                        fastUnbox<'T> o
                    else
                        objCache.[id] <- x ; x
                else
                    objCache.[id] <- x ; x

            elif ObjHeader.hasFlag flags ObjHeader.isOldCachedInstance then
                let id = br.ReadInt64() in objCache.[id] |> fastUnbox<'T>
            else
                read flags

        // efficient sequence deserialization; must be used as top-level operation only
        member internal r.ReadSequence<'T> (f : Pickler<'T>, length : int) =
            let inline flushState () =
                objCache.Clear ()
                counter <- 1L

            let isPrimitive = f.TypeKind = TypeKind.Primitive
            let isNonAtomic = f.PicklerInfo <> PicklerInfo.Atomic

            let read idx =
                if isPrimitive then f.Read r
                elif isNonAtomic then
                    if idx % sequenceCounterResetThreshold = 0 then
                        flushState ()
                    r.Read f
                elif br.ReadBoolean () then fastUnbox<'T> null
                else
                    f.Read r
            
            // read object header
            match ObjHeader.read f.Type f.TypeHash (br.ReadUInt32()) with
            | ObjHeader.isSequenceHeader -> ()
            | _ -> 
                let msg = "FsPickler: invalid stream data; expected sequence serialization."
                raise <| new SerializationException(msg)

            let cnt = ref 0
            let curr = ref Unchecked.defaultof<'T>
            let initPos = stream.Position
            {
                new IEnumerator<'T> with
                    member __.Current = !curr
                    member __.Current = box !curr
                    member __.Dispose () = (r :> IDisposable).Dispose ()
                    member __.MoveNext () =
                        if !cnt < length then
                            curr := read !cnt
                            incr cnt
                            true
                        else
                            false

                    member __.Reset () =
                        stream.Position <- initPos
                        curr := Unchecked.defaultof<'T>
                        cnt := 0
                        do flushState ()
            }

        interface IDisposable with
            member __.Dispose () = br.Dispose ()