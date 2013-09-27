namespace FsPickler

    open System
    open System.IO
    open System.Text
    open System.Runtime.CompilerServices
    open System.Collections.Generic
    open System.Runtime.Serialization

    open FsPickler.Utils
    open FsPickler.Header



    [<AutoSerializable(false)>]
    [<AbstractClass>]
    type Pickler =

        val private declared_type : Type
        val private is_recursive_type : bool
        val mutable private m_pickler_type : Type
        val mutable private m_typeInfo : TypeInfo
        val mutable private m_typeHash : TypeHash
        
        val mutable private m_isInitialized : bool

        val mutable private m_picklerInfo : PicklerInfo
        val mutable private m_isCacheByRef : bool
        val mutable private m_useWithSubtypes : bool

        internal new (t : Type) =
            {
                declared_type = t ; is_recursive_type = isRecursiveType t ;

                m_isInitialized = false ;

                m_pickler_type = Unchecked.defaultof<_> ; 
                m_typeInfo = Unchecked.defaultof<_> ; 
                m_typeHash = Unchecked.defaultof<_> ;
                m_picklerInfo = Unchecked.defaultof<_> ; 
                m_isCacheByRef = Unchecked.defaultof<_> ; 
                m_useWithSubtypes = Unchecked.defaultof<_> ; 
            }

        internal new (t : Type, picklerInfo, isCacheByRef, useWithSubtypes) =
            {
                declared_type = t ; is_recursive_type = isRecursiveType t ;

                m_isInitialized = true ;

                m_pickler_type = t ; m_typeInfo = computeTypeInfo t ; m_typeHash = computeTypeHash t ;
                
                m_picklerInfo = picklerInfo ;
                m_isCacheByRef = isCacheByRef ;
                m_useWithSubtypes = useWithSubtypes ;
            }

        member f.Type = f.declared_type
        member f.IsRecursiveType = f.is_recursive_type

        member internal f.TypeInfo = f.m_typeInfo
        member internal f.TypeHash = f.m_typeHash

        member f.PicklerType =
            if f.m_isInitialized then f.m_pickler_type
            else
                invalidOp "Attempting to consume formatter at construction time."

        member f.PicklerInfo =
            if f.m_isInitialized then f.m_picklerInfo
            else
                invalidOp "Attempting to consume formatter at construction time."

        member f.IsCacheByRef =
            if f.m_isInitialized then f.m_isCacheByRef
            else
                invalidOp "Attempting to consume formatter at construction time."

        member f.UseWithSubtypes =
            if f.m_isInitialized then f.m_useWithSubtypes
            else
                invalidOp "Attempting to consume formatter at construction time."

        member internal f.IsInitialized = f.m_isInitialized

        abstract member UntypedWrite : Writer -> obj -> unit
        abstract member UntypedRead : Reader -> obj

        abstract member ManagedWrite : Writer -> obj -> unit
        abstract member ManagedRead : Reader -> obj

        abstract member Cast<'S> : unit -> Pickler<'S>

        abstract member InitializeFrom : Pickler -> unit
        default f.InitializeFrom(f' : Pickler) : unit =
            if f.m_isInitialized then
                invalidOp "Pickler has already been initialized."
            elif not f'.m_isInitialized then 
                invalidOp "Attempting to consume formatter at construction time."
            elif f.Type <> f'.Type && not (f'.Type.IsAssignableFrom(f.Type) && f'.UseWithSubtypes) then
                raise <| new InvalidCastException(sprintf "Cannot cast formatter from %O to %O." f'.Type f.Type)
            else
                f.m_pickler_type <- f'.m_pickler_type
                f.m_typeHash <- f'.m_typeHash
                f.m_typeInfo <- f'.m_typeInfo
                f.m_picklerInfo <- f'.m_picklerInfo
                f.m_isCacheByRef <- f'.m_isCacheByRef
                f.m_useWithSubtypes <- f'.m_useWithSubtypes
                f.m_isInitialized <- true

    and [<Sealed>][<AutoSerializable(false)>] Pickler<'T> =
        inherit Pickler
        
        val mutable private m_writer : Writer -> 'T -> unit
        val mutable private m_reader : Reader -> 'T

        internal new (reader, writer, picklerInfo, isCacheByRef, useWithSubtypes) = 
            { 
                inherit Pickler(typeof<'T>, picklerInfo, isCacheByRef, useWithSubtypes) ;
                m_writer = writer ;
                m_reader = reader ;
            }

        private new (t, reader, writer, picklerInfo, isCacheByRef, useWithSubtypes) = 
            { 
                inherit Pickler(t, picklerInfo, isCacheByRef, useWithSubtypes) ;
                m_writer = writer ;
                m_reader = reader ;
            }

        internal new () = 
            {
                inherit Pickler(typeof<'T>) ;
                m_writer = fun _ _ -> invalidOp "Attempting to consume formatter at construction time." ;
                m_reader = fun _ -> invalidOp "Attempting to consume formatter at construction time." ;
            }

        override f.UntypedWrite (w : Writer) (o : obj) = f.m_writer w (fastUnbox<'T> o)
        override f.UntypedRead (r : Reader) = f.m_reader r :> obj
        override f.ManagedWrite (w : Writer) (o : obj) = w.Write(f, fastUnbox<'T> o)
        override f.ManagedRead (r : Reader) = r.Read f :> obj

        override f.Cast<'S> () =
            if typeof<'T> = typeof<'S> then f |> fastUnbox<Pickler<'S>>
            elif typeof<'T>.IsAssignableFrom typeof<'S> && f.UseWithSubtypes then
                let writer = let wf = f.m_writer in fun w x -> wf w (fastUnbox<'T> x)
                let reader = let rf = f.m_reader in fun r -> rf r |> fastUnbox<'S>
                new Pickler<'S>(typeof<'T>, reader, writer, f.PicklerInfo, f.IsCacheByRef, f.UseWithSubtypes)
            else
                raise <| new InvalidCastException(sprintf "Cannot cast pickler of type '%O' to type '%O'." typeof<'T> typeof<'S>)
                

        override f.InitializeFrom(f' : Pickler) : unit =
            let f' = f'.Cast<'T> ()
            base.InitializeFrom f'
            f.m_writer <- f'.m_writer
            f.m_reader <- f'.m_reader
            

        member internal f.Write = f.m_writer
        member internal f.Read = f.m_reader


    and IPicklerResolver =
        abstract Resolve<'T> : unit -> Pickler<'T>
        abstract Resolve : Type -> Pickler

    and [<AutoSerializable(false)>]
        Writer internal (stream : Stream, resolver : IPicklerResolver, ?streamingContext, ?leaveOpen, ?encoding) =
        
        do if not stream.CanWrite then invalidOp "Cannot write to stream."

        // using UTF8 gives an observed performance improvement ~200%
        let encoding = defaultArg encoding Encoding.UTF8

        let bw = new BinaryWriter(stream, encoding, defaultArg leaveOpen true)
        let sc = initStreamingContext streamingContext
        let idGen = new ObjectIDGenerator()
        let objStack = new Stack<int64> ()

        let tyPickler = resolver.Resolve<Type> ()

        member w.BinaryWriter = bw

        member w.StreamingContext = sc

        member internal w.Resolver = resolver

        member w.Write<'T> (fmt : Pickler<'T>, x : 'T) =

            let inline writeHeader (flags : byte) =
                bw.Write(ObjHeader.create fmt.TypeHash flags)

            let inline writeType (t : Type) =
                let mutable firstOccurence = false
                let id = idGen.GetId(t, &firstOccurence)
                bw.Write firstOccurence
                if firstOccurence then tyPickler.Write w t
                else
                    bw.Write id

            if fmt.TypeInfo <= TypeInfo.Value then 
                writeHeader ObjHeader.empty
                fmt.Write w x
            elif obj.ReferenceEquals(x, null) then writeHeader ObjHeader.isNull else

            do RuntimeHelpers.EnsureSufficientExecutionStack()

            if fmt.IsCacheByRef || fmt.IsRecursiveType then
                let mutable firstOccurence = false
                let id = idGen.GetId(x, &firstOccurence)
                if firstOccurence then

                    let inline write (fmt : Pickler) (writeOp : unit -> unit) =
                        if fmt.PicklerInfo = PicklerInfo.ReflectionDerived then
                            writeOp ()
                        else
                            objStack.Push id
                            writeOp ()
                            objStack.Pop () |> ignore
                    
                    if fmt.TypeInfo <= TypeInfo.Sealed || fmt.UseWithSubtypes then
                        writeHeader ObjHeader.isNewInstance
                        write fmt (fun () -> fmt.Write w x)
                    else
                        // type is not sealed, do subtype resolution
                        let t0 = x.GetType()
                        if t0 <> fmt.Type then
                            let fmt' = resolver.Resolve t0
                            writeHeader (ObjHeader.isNewInstance ||| ObjHeader.isProperSubtype)
                            writeType t0
                            write fmt' (fun () -> fmt'.UntypedWrite w x)
                        else
                            writeHeader ObjHeader.isNewInstance
                            write fmt (fun () -> fmt.Write w x)

                elif objStack.Contains id then
                    raise <| new SerializationException(sprintf "Unsupported cyclic object graph '%s'." fmt.Type.FullName)
                else
                    writeHeader ObjHeader.isCachedInstance
                    bw.Write id
            else
                if fmt.TypeInfo <= TypeInfo.Sealed || fmt.UseWithSubtypes then
                    writeHeader ObjHeader.empty
                    fmt.Write w x
                else
                    // type is not sealed, do subtype resolution
                    let t0 = x.GetType ()
                    if t0 <> fmt.Type then
                        let f0 = resolver.Resolve t0
                        writeHeader ObjHeader.isProperSubtype
                        f0.UntypedWrite w (x :> obj)
                    else
                        writeHeader ObjHeader.empty
                        fmt.Write w x

        member w.Write<'T>(t : 'T) = let f = resolver.Resolve<'T> () in w.Write(f, t)

        member internal w.WriteObj(t : Type, o : obj) =
            let f = resolver.Resolve t in f.ManagedWrite w o

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
        let mutable counter = 1L
        let mutable currentReflectedObjId = 0L

        let tyPickler = resolver.Resolve<Type> ()

        // objects deserialized with reflection-based rules are registered to the cache
        // at the initialization stage to support cyclic object graphs.
        member internal r.EarlyRegisterObject (o : obj) =
            if currentReflectedObjId = 0L then
                raise <| new SerializationException("Unanticipated reflected object binding.")
            else
                objCache.Add(currentReflectedObjId, o)
                currentReflectedObjId <- 0L

        member r.BinaryReader = br

        member r.StreamingContext = sc

        member internal r.Resolver = resolver

        // the mean deserialization logic
        member r.Read(fmt : Pickler<'T>) : 'T =
            let flags = ObjHeader.read fmt.TypeHash (br.ReadUInt32())

            let inline readType () =
                if br.ReadBoolean () then
                    let t = tyPickler.Read r
                    objCache.Add(counter, t)
                    counter <- counter + 1L
                    t
                else
                    let id = br.ReadInt64()
                    objCache.[id] |> fastUnbox<Type>

            if ObjHeader.hasFlag flags ObjHeader.isNull then Unchecked.defaultof<'T>
            elif fmt.TypeInfo <= TypeInfo.Value then fmt.Read r
            elif ObjHeader.hasFlag flags ObjHeader.isNewInstance then
                let id = counter
                counter <- counter + 1L

                let inline read (fmt : Pickler) (readOp : unit -> 'T) =
                    let inline checkState () =
                        if currentReflectedObjId <> 0L then
                            raise <| new SerializationException("Internal error: reader state is corrupt.")

                    if fmt.PicklerInfo = PicklerInfo.ReflectionDerived && fmt.TypeInfo > TypeInfo.Value then
                        do checkState ()

                        currentReflectedObjId <- id
                        let x = readOp ()

                        do checkState ()
                        x
                    else
                        let x = readOp ()
                        objCache.Add(id, x) ; x
                
                if ObjHeader.hasFlag flags ObjHeader.isProperSubtype then
                    let t0 = readType ()
                    let fmt' = resolver.Resolve t0
                    read fmt' (fun () -> fmt'.UntypedRead r |> fastUnbox<'T>)
                else
                    read fmt (fun () -> fmt.Read r)

            elif ObjHeader.hasFlag flags ObjHeader.isCachedInstance then
                let id = br.ReadInt64() in objCache.[id] |> fastUnbox<'T>

            elif ObjHeader.hasFlag flags ObjHeader.isProperSubtype then
                let t0 = readType ()
                let f0 = resolver.Resolve t0
                f0.UntypedRead r |> fastUnbox<'T>
            else
                fmt.Read r

        member r.Read<'T> () : 'T = let f = resolver.Resolve<'T> () in r.Read f

        member internal r.ReadObj(t : Type) = let f = resolver.Resolve t in f.ManagedRead r

        interface IDisposable with
            member __.Dispose () = br.Dispose ()