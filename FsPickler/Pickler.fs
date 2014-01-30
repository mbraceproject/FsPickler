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

        member f.Type = f.declared_type

        member internal f.TypeKind = f.m_TypeKind
        member internal f.PicklerFlags = f.m_picklerFlags

        member f.CacheId
            with get () = f.m_cache_id
            and internal set id = f.m_cache_id <- id

        member f.IsCacheByRef =
            if f.m_isInitialized then  f.m_isCacheByRef
            else
                invalidOp "Attempting to consume pickler at initialization time."

        member f.UseWithSubtypes =
            if f.m_isInitialized then  f.m_useWithSubtypes
            else
                invalidOp "Attempting to consume pickler at initialization time."

        member f.IsCyclicType = 
            if f.m_isInitialized then  f.m_is_cyclic_type
            else
                invalidOp "Attempting to consume pickler at initialization time."
                
        member f.IsFixedSize =
            if f.m_isInitialized then  f.m_is_fixed_size
            else
                invalidOp "Attempting to consume pickler at initialization time."

        member f.PicklerType =
            if f.m_isInitialized then f.m_pickler_type
            else
                invalidOp "Attempting to consume pickler at initialization time."

        member f.PicklerInfo =
            if f.m_isInitialized then f.m_picklerInfo
            else
                invalidOp "Attempting to consume pickler at initialization time."


        member internal f.IsInitialized = f.m_isInitialized

        abstract member UntypedWrite : Writer * value:obj * managed:bool -> unit
        abstract member UntypedRead : Reader * managed:bool -> obj

        abstract member WriteRootObject : Writer * id : string * value : obj -> unit
        abstract member ReadRootObject : Reader * id : string -> obj

        abstract member WriteSequence : Writer * id : string * sequence : IEnumerable -> int
        abstract member ReadSequence : Reader * id : string * length : int -> IEnumerator

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
                f.m_picklerFlags <- f'.m_picklerFlags
                f.m_TypeKind <- f'.m_TypeKind
                f.m_picklerInfo <- f'.m_picklerInfo
                f.m_isCacheByRef <- f'.m_isCacheByRef
                f.m_useWithSubtypes <- f'.m_useWithSubtypes
                f.m_is_cyclic_type <- f'.m_is_cyclic_type
                f.m_is_fixed_size <- f'.m_is_fixed_size
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

        override f.UntypedWrite (w : Writer, value : obj, managed:bool) =
            if managed then w.Write(f, fastUnbox<'T> value)
            else
                f.m_writer w (fastUnbox<'T> value)

        override f.UntypedRead (r : Reader, managed : bool) = 
            if managed then r.Read f :> obj
            else
                f.m_reader r :> obj

        override f.WriteRootObject (w : Writer, id, o : obj) = w.WriteRootObject(f, id, o :?> 'T)
        override f.ReadRootObject (r : Reader, id) = r.ReadRootObject (f, id) :> obj
        override f.WriteSequence (w : Writer, id, e : IEnumerable) = w.WriteSequence(f, id, e :?> seq<'T>)
        override f.ReadSequence (r : Reader, id, length : int) = r.ReadSequence(f, id, length) :> IEnumerator

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
        abstract Resolve : Type -> Pickler
        abstract Resolve<'T> : unit -> Pickler<'T>

    and [<AutoSerializable(false)>]
        Writer internal (stream : Stream, resolver : IPicklerResolver, ?streamingContext, ?encoding, ?leaveOpen) =
        
        do if not stream.CanWrite then invalidOp "Cannot write to stream."

        // using UTF8 gives an observed performance improvement ~200%
        let encoding = defaultArg encoding Encoding.UTF8

        let bw = new BinaryWriter(stream, encoding, defaultArg leaveOpen true)
        let sc = match streamingContext with None -> new StreamingContext() | Some sc -> sc
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
                bw.Write(ObjHeader.create fmt.PicklerFlags flags)

            // ad-hoc System.Type caching
            let inline writeType (t : Type) =
                let id, firstOccurence = idGen.GetId t
                if firstOccurence then
                    bw.Write true
                    tyPickler.Write w t
                else
                    bw.Write false
                    bw.Write id

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
                        writeType t0
                        fmt'.UntypedWrite(w, x, managed = false)
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

            if fmt.IsCacheByRef || fmt.IsCyclicType then
                let id, firstOccurence = idGen.GetId x

                if firstOccurence then
                    if fmt.IsCyclicType then 
                        // push id to the symbolic stack to detect cyclic objects during traversal
                        objStack.Push id

                        write ObjHeader.isNewCachedInstance

                        objStack.Pop () |> ignore
                        cyclicObjects.Remove id |> ignore
                    else
                        write ObjHeader.isNewCachedInstance

                elif fmt.IsCyclicType && objStack.Contains id && not <| cyclicObjects.Contains id then
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
                            writeType t
                        else
                            writeHeader ObjHeader.isCyclicInstance

                    bw.Write id
                else
                    writeHeader ObjHeader.isOldCachedInstance
                    bw.Write id

            else
                write ObjHeader.empty

        member internal w.WriteRootObject(f : Pickler<'T>, id : string, x : 'T) =
            bw.Write id
            w.Write(f, x)

        // efficient sequence serialization; must be used as top-level operation only
        member internal w.WriteSequence<'T>(f : Pickler<'T>, id : string, xs : seq<'T>) : int =
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

            // write id
            bw.Write id
            // write sequence header
            bw.Write(ObjHeader.create f.PicklerFlags ObjHeader.isSequenceHeader)
            
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
        Reader internal (stream : Stream, resolver : IPicklerResolver, ?streamingContext, ?encoding, ?leaveOpen) =

        do if not stream.CanRead then invalidOp "Cannot read from stream."

        // using UTF8 gives an observed performance improvement ~200%
        let encoding = defaultArg encoding Encoding.UTF8

        let br = new BinaryReader(stream, encoding, defaultArg leaveOpen true)
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

        member r.BinaryReader = br

        member r.StreamingContext = sc

        member internal r.Resolver = resolver


        // the primary deserialization routine; handles all the caching, subtype resolution logic, etc
        member r.Read(fmt : Pickler<'T>) : 'T =

            // ad-hoc System.Type caching
            let inline readType () =
                if br.ReadBoolean() then
                    let id = counter
                    counter <- counter + 1L
                    let t = tyPickler.Read r
                    objCache.Add(id, t)
                    t
                else
                    let id = br.ReadInt64()
                    objCache.[id] |> fastUnbox<Type>

            let inline read flags =
                if ObjHeader.hasFlag flags ObjHeader.isProperSubtype then
                    let t = readType ()
                    let fmt' = resolver.Resolve t
                    fmt'.UntypedRead(r, managed = false) |> fastUnbox<'T>
                else
                    fmt.Read r

            let flags = ObjHeader.read fmt.Type fmt.PicklerFlags (br.ReadUInt32())

            if ObjHeader.hasFlag flags ObjHeader.isNull then fastUnbox<'T> null
            elif fmt.TypeKind <= TypeKind.Value then fmt.Read r
            elif ObjHeader.hasFlag flags ObjHeader.isCyclicInstance then
                // came across a nested instance of a cyclic object
                // add an uninitialized object to the cache and schedule
                // reflection-based fixup at the root level.
                let t =
                    if ObjHeader.hasFlag flags ObjHeader.isProperSubtype then readType ()
                    else fmt.Type

                let id = br.ReadInt64()

                let x = FormatterServices.GetUninitializedObject(t)

                // register a fixup operation & cache
                fixupIndex.Add(id, (t,x))
                objCache.Add(id, x)

                fastUnbox<'T> x

            elif ObjHeader.hasFlag flags ObjHeader.isNewCachedInstance then
                let id = counter
                if fmt.TypeKind = TypeKind.Array || fmt.TypeKind = TypeKind.ArrayCompatible then
                    currentDeserializedArrayId <- id
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

        member internal r.ReadRootObject(f : Pickler<'T>, id : string) =
            let id' = br.ReadString()
            if id <> id' then
                let msg = sprintf "FsPickler: Root object is of type '%s', expected '%s'." id' id
                raise <| new SerializationException(msg)
            r.Read f

        // efficient sequence deserialization; must be used as top-level operation only
        member internal r.ReadSequence<'T> (f : Pickler<'T>, id : string, length : int) =
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

            // read id
            let id' = br.ReadString()
            if id <> id' then
                let msg = sprintf "FsPickler: Sequence is of type '%s', expected '%s'." id' id
                raise <| new SerializationException(msg)
            
            // read object header
            match ObjHeader.read f.Type f.PicklerFlags (br.ReadUInt32()) with
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