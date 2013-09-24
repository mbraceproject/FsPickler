namespace FsCoreSerializer

    open System
    open System.IO
    open System.Text
    open System.Runtime.CompilerServices
    open System.Collections.Generic
    open System.Runtime.Serialization

    open FsCoreSerializer.Utils
    open FsCoreSerializer.FormatterHeader

    type FormatterInfo =
        | Atomic = 0
        | ReflectionType = 1
        | ReflectionDerived = 2
        | ISerializable = 3
        | FSharpValue = 4 // tuples, records and DUs
        | IFsCoreSerializable = 5
        | Custom = 6

    [<AutoSerializable(false)>]
    [<AbstractClass>]
    type Formatter =

        val private declared_type : Type
        val mutable private m_reflected_type : Type
        val mutable private m_typeInfo : TypeInfo
        val mutable private m_typeHash : TypeHash
        
        val mutable private m_isInitialized : bool

        val mutable private m_formatterInfo : FormatterInfo
        val mutable private m_cacheObj : bool
        val mutable private m_useWithSubtypes : bool

        internal new (t : Type) =
            {
                declared_type = t ;

                m_isInitialized = false ;

                m_reflected_type = null ; 
                m_typeInfo = Unchecked.defaultof<_> ; 
                m_typeHash = Unchecked.defaultof<_> ;
                m_formatterInfo = Unchecked.defaultof<_> ; 
                m_cacheObj = false ; 
                m_useWithSubtypes = false ;
            }

        internal new (t : Type, formatterInfo, cacheObj, useWithSubtypes) =
            {
                declared_type = t ;

                m_isInitialized = true ;

                m_reflected_type = t ; m_typeInfo = getTypeInfo t ; m_typeHash = ObjHeader.computeHash t ;
                
                m_formatterInfo = formatterInfo ;
                m_cacheObj = cacheObj ;
                m_useWithSubtypes = useWithSubtypes ;
            }

        member f.Type = f.declared_type
        member f.FormatterType = f.m_reflected_type
        member internal f.TypeInfo = f.m_typeInfo
        member internal f.TypeHash = f.m_typeHash

        member f.FormatterInfo =
            if f.m_isInitialized then f.m_formatterInfo
            else
                // TODO : define message elsewhere
                invalidOp "Attempting to consume formatter at construction time."

        member f.CacheObj =
            if f.m_isInitialized then f.m_cacheObj
            else
                invalidOp "Attempting to consume formatter at construction time."

        member f.UseWithSubtypes =
            if f.m_isInitialized then f.m_useWithSubtypes
            else
                invalidOp "Attempting to consume formatter at construction time."

        member f.IsInitialized = f.m_isInitialized

        abstract member UntypedWrite : Writer -> obj -> unit
        abstract member UntypedRead : Reader -> obj

        abstract member ManagedWrite : Writer -> obj -> unit
        abstract member ManagedRead : Reader -> obj

        abstract member Cast<'S> : unit -> Formatter<'S>

        abstract member InitializeFrom : Formatter -> unit
        default f.InitializeFrom(f' : Formatter) : unit =
            if f.m_isInitialized then
                invalidOp "Formatter has already been initialized."
            elif not f'.m_isInitialized then 
                invalidOp "Attempting to consume formatter at construction time."
            elif f.Type <> f'.Type && not (f'.Type.IsAssignableFrom(f.Type) && f'.UseWithSubtypes) then
                raise <| new InvalidCastException(sprintf "Cannot cast formatter from %O to %O." f'.Type f.Type)
            else
                f.m_reflected_type <- f'.m_reflected_type
                f.m_typeHash <- f'.m_typeHash
                f.m_typeInfo <- f'.m_typeInfo
                f.m_formatterInfo <- f'.m_formatterInfo
                f.m_cacheObj <- f'.m_cacheObj
                f.m_useWithSubtypes <- f'.m_useWithSubtypes
                f.m_isInitialized <- true

    and [<Sealed>] Formatter<'T> =
        inherit Formatter
        
        val mutable private m_writer : Writer -> 'T -> unit
        val mutable private m_reader : Reader -> 'T

        internal new (reader, writer, formatterInfo, cacheObj, useWithSubtypes) = 
            { 
                inherit Formatter(typeof<'T>, formatterInfo, cacheObj, useWithSubtypes) ;
                m_writer = writer ;
                m_reader = reader ;
            }

        private new (t, reader, writer, formatterInfo, cacheObj, useWithSubtypes) = 
            { 
                inherit Formatter(t, formatterInfo, cacheObj, useWithSubtypes) ;
                m_writer = writer ;
                m_reader = reader ;
            }

        internal new () = 
            {
                inherit Formatter(typeof<'T>) ;
                m_writer = fun _ _ -> invalidOp "Attempting to consume formatter at construction time." ;
                m_reader = fun _ -> invalidOp "Attempting to consume formatter at construction time." ;
            }

        override f.UntypedWrite (w : Writer) (o : obj) = f.m_writer w (fastUnbox<'T> o)
        override f.UntypedRead (r : Reader) = f.m_reader r :> obj
        override f.ManagedWrite (w : Writer) (o : obj) = w.Write(f, fastUnbox<'T> o)
        override f.ManagedRead (r : Reader) = r.Read f :> obj

        override f.Cast<'S> () =
            if typeof<'T> = typeof<'S> then f |> fastUnbox<Formatter<'S>>
            elif typeof<'T>.IsAssignableFrom typeof<'S> && f.UseWithSubtypes then
                let writer = let wf = f.m_writer in fun w x -> wf w (fastUnbox<'T> x)
                let reader = let rf = f.m_reader in fun r -> rf r |> fastUnbox<'S>
                new Formatter<'S>(typeof<'T>, reader, writer, f.FormatterInfo, f.CacheObj, f.UseWithSubtypes)
            else
                raise <| new InvalidCastException(sprintf "Cannot cast formatter of type '%O' to type '%O'." typeof<'T> typeof<'S>)
                

        override f.InitializeFrom(f' : Formatter) : unit =
            let f' = f'.Cast<'T> ()
            base.InitializeFrom f'
            f.m_writer <- f'.m_writer
            f.m_reader <- f'.m_reader
            

        member f.Write = f.m_writer
        member f.Read = f.m_reader


    and IFormatterResolver =
        abstract Resolve<'T> : unit -> Formatter<'T>
        abstract Resolve : Type -> Formatter

    and [<AutoSerializable(false)>] 
        Writer internal (stream : Stream, resolver : IFormatterResolver, sc : StreamingContext, ?leaveOpen, ?encoding) as self =

        // using UTF8 gives an observed performance improvement ~200%
        let encoding = defaultArg encoding Encoding.UTF8

        let bw = new BinaryWriter(stream, encoding, defaultArg leaveOpen true)
        let idGen = new ObjectIDGenerator()
        let objStack = new Stack<int64> ()

        let tyFormatter = resolver.Resolve<Type> ()

        /// BinaryWriter to the underlying stream.
        member internal w.BW = bw

        /// Access the current streaming context.
        member w.StreamingContext = sc

        /// <summary>Precomputes formatter for the given type at runtime.</summary>
        member w.ResolveFormatter<'T> () = resolver.Resolve<'T> ()
        //Formatter<'T>(resolver typeof<'T>)

        /// <summary>
        ///     Write object to stream using given formatter rules. Unsafe method.
        ///     Has to be deserialized with the dual method Reader.ReadObj : Formatter -> obj.
        /// </summary>
        /// <param name="fmt">Formatter used in serialization. Needs to be compatible with input object.</param>
        /// <param name="o">The input object.</param>
        member w.Write<'T> (fmt : Formatter<'T>, x : 'T) =

            let inline writeHeader (flags : byte) =
                bw.Write(ObjHeader.create fmt.TypeHash flags)

            let inline writeType (t : Type) =
                let mutable firstOccurence = false
                let id = idGen.GetId(t, &firstOccurence)
                bw.Write firstOccurence
                if firstOccurence then tyFormatter.Write self t
                else
                    bw.Write id

            if fmt.TypeInfo <= TypeInfo.Value then 
                writeHeader ObjHeader.empty
                fmt.Write w x
            elif obj.ReferenceEquals(x, null) then writeHeader ObjHeader.isNull else

            do RuntimeHelpers.EnsureSufficientExecutionStack()

            if fmt.CacheObj then
                let mutable firstOccurence = false
                let id = idGen.GetId(x, &firstOccurence)
                if firstOccurence then

                    let inline write (fmt : Formatter) (writeOp : unit -> unit) =
                        if fmt.FormatterInfo = FormatterInfo.ReflectionDerived then
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
                    raise <| new SerializationException(sprintf "Unexpected cyclic object graph '%s'." fmt.Type.FullName)
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

        /// <summary>
        ///     Writes given object to the underlying stream.
        ///     Serialization rules are resolved at runtime based on the type argument.
        ///     Object has to be read with the dual Reader.Read&lt;'T&gt; method.
        /// </summary>
        /// <param name="t">The input value.</param>
        member w.Write<'T>(t : 'T) = let f = resolver.Resolve<'T> () in w.Write(f, t)

        member internal w.WriteObj(t : Type, o : obj) =
            let f = resolver.Resolve t in f.ManagedWrite w o

        interface IDisposable with
            member __.Dispose () = bw.Dispose ()

    and [<AutoSerializable(false)>] 
        Reader internal (stream : Stream, resolver : IFormatterResolver, sc : StreamingContext, ?leaveOpen, ?encoding) as self =

        // using UTF8 gives an observed performance improvement ~200%
        let encoding = defaultArg encoding Encoding.UTF8

        let br = new BinaryReader(stream, encoding, defaultArg leaveOpen true)
        let objCache = new Dictionary<int64, obj> ()
        let mutable counter = 1L
        let mutable currentReflectedObjId = 0L

        let tyFormatter = resolver.Resolve<Type> ()

        // objects deserialized with reflection-based rules are registered to the cache
        // at the initialization stage to support cyclic object graphs.
        member internal r.EarlyRegisterObject (o : obj) =
            if currentReflectedObjId = 0L then
                raise <| new SerializationException("Unexpected reflected object binding.")
            else
                objCache.Add(currentReflectedObjId, o)
                currentReflectedObjId <- 0L

        /// BinaryReader to the underlying stream.
        member internal r.BR = br

        /// Access the current streaming context.
        member r.StreamingContext = sc

        /// <summary>Precomputes formatter for the given type at runtime.</summary>
        member w.ResolveFormatter<'T> () = resolver.Resolve<'T> ()


        /// <summary>
        ///     Read object from stream using given formatter rules.
        ///     Needs to have been serialized with the dual method Writer.WriteObj : Formatter * obj -> unit.
        /// </summary>
        /// <param name="fmt">Formatter used in deserialization. Needs to be compatible with input object.</param>
        /// <param name="o">The input object.</param>
        member r.Read(fmt : Formatter<'T>) : 'T =
            let flags = ObjHeader.read fmt.TypeHash (br.ReadUInt32())

            let inline readType () =
                if br.ReadBoolean () then
                    let t = tyFormatter.Read self
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

                let inline read (fmt : Formatter) (readOp : unit -> 'T) =
                    let inline checkState () =
                        if currentReflectedObjId <> 0L then
                            raise <| new SerializationException("Internal error: reader state is corrupt.")

                    if fmt.FormatterInfo = FormatterInfo.ReflectionDerived && fmt.TypeInfo > TypeInfo.Value then
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

        /// <summary>
        ///     Reads object of given type from the underlying stream.
        ///     Serialization rules are resolved at runtime based on the object header.
        ///     Needs to have been serialized with the dual Writer.Write&lt;'T&gt; method.
        /// </summary>
        member r.Read<'T> () : 'T = let f = resolver.Resolve<'T> () in r.Read f

        member internal r.ReadObj(t : Type) = let f = resolver.Resolve t in f.ManagedRead r

        interface IDisposable with
            member __.Dispose () = br.Dispose ()



    /// an ISerializable-like pattern for describing serialization rules in new types
    /// serialization is performed by providing direct access to the underlying stream
    /// through the Reader/Writer types.
    ///
    /// Types implementing this interface require a constructor 'new (reader : Reader)'
    type IFsCoreSerializable =
        abstract GetObjectData : Writer -> unit

    /// A factory pattern for generating formatters for generic types
    /// any type implementing this interface must declare a method
    ///
    ///     Create<'T1, ... , 'Tn | constraints> : IFormatterResolver -> Formatter
    ///
    /// Generic formatters are registered and resolved at runtime. 
    and IGenericFormatterFactory = interface end

    // a few guideline templates that inherit the above interface

    and IGenericFormatterFactory1 =
        inherit IGenericFormatterFactory
        abstract Create<'T> : IFormatterResolver -> Formatter

    and IGenericFormatterFactory2 =
        inherit IGenericFormatterFactory
        abstract Create<'T, 'S> : IFormatterResolver -> Formatter

    and IGenericFormatterFactory3 =
        inherit IGenericFormatterFactory
        abstract Create<'T, 'S, 'U> : IFormatterResolver -> Formatter

    and IGenericFormatterFactory4 =
        inherit IGenericFormatterFactory
        abstract Create<'T, 'S, 'U, 'V> : IFormatterResolver -> Formatter

    /// Raised by FsCoreSerializer whenever an unsupported type is encountered in the object graph.
    type NonSerializableTypeException(unsupportedType : Type, ?message : string) =
        inherit SerializationException(
            match message with
            | None -> sprintf "Serialization of type '%O' is not supported." unsupportedType
            | Some msg -> sprintf "Serialization of type '%O' is not supported: %s" unsupportedType msg)

        member __.UnsupportedType = unsupportedType
