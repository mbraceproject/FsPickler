namespace FsCoreSerializer

    open System
    open System.IO
    open System.Text
    open System.Runtime.CompilerServices
    open System.Collections.Generic
    open System.Runtime.Serialization

    type internal TypeInfo =
        | Primitive = 0
        | Enum = 1
        | Value = 2
        | Array = 3
        | Sealed = 4
        | NonSealed = 5
        | Abstract = 6

    type FormatterInfo =
        | Atomic = 0
        | ReflectionType = 1
        | ReflectionDerived = 2
        | ISerializable = 3
        | FSharpValue = 4 // tuples, records and DUs
        | IFsCoreSerializable = 5
        | Custom = 6

    module internal FooTools =

        // builds type info enumeration out of reflection info
        let internal getTypeInfo (t : Type) =
            if t.IsPrimitive then TypeInfo.Primitive
            elif t.IsEnum then TypeInfo.Enum
            elif t.IsValueType then TypeInfo.Value
            elif t.IsArray then TypeInfo.Array
            elif t.IsSealed then TypeInfo.Sealed
            elif t.IsAbstract then TypeInfo.Abstract
            else TypeInfo.NonSealed

        let inline mkUntypedWriter (f : 'a -> 'b -> 'c) = fun x (y:obj) -> f x (y :?> _)
        let inline mkUntypedReader (f : 'a -> 'b) = fun x -> f x :> obj
        let inline mkTypedWriter (f : 'a -> obj -> 'c) = fun x (y : 'b) -> f x (y :> obj)
        let inline mkTypedReader (f : 'a -> obj) = fun x -> f x :?> 'b


    open FooTools

    [<AutoSerializable(false)>]
    [<AbstractClass>]
    type Formatter =

        val mutable private m_type : Type
        val mutable private m_typeInfo : TypeInfo
        val mutable private m_typeHash : uint16

        val mutable private m_writer_untyped : Writer -> obj -> unit
        val mutable private m_reader_untyped : Reader -> obj
        
        val mutable private m_isInitialized : bool

        val mutable private m_formatterInfo : FormatterInfo
        val mutable private m_cacheObj : bool
        val mutable private m_useWithSubtypes : bool

        internal new (t : Type) =
            { 
                m_isInitialized = false ;

                m_type = t ; m_typeInfo = FooTools.getTypeInfo t ; m_typeHash = ObjHeader.computeHash t ;
                
                m_writer_untyped = fun _ _ -> invalidOp "Attempting to consume formatter at construction time."
                m_reader_untyped = fun _ -> invalidOp "Attempting to consume formatter at construction time."

                m_formatterInfo = Unchecked.defaultof<_> ; 
                m_cacheObj = false ; 
                m_useWithSubtypes = false ;
            }

        internal new (t : Type, reader, writer, formatterInfo, cacheObj, useWithSubtypes) =
            {
                m_isInitialized = true ;

                m_type = t ; m_typeInfo = FooTools.getTypeInfo t ; m_typeHash = ObjHeader.computeHash t ;
                
                m_writer_untyped = writer ;
                m_reader_untyped = reader ;
                
                m_formatterInfo = formatterInfo ;
                m_cacheObj = cacheObj ;
                m_useWithSubtypes = useWithSubtypes ;
            }

        member f.Type = f.m_type
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

        member f.UntypedWrite = f.m_writer_untyped
        member f.UntypedRead = f.m_reader_untyped

        abstract member ManagedWrite : Writer -> obj -> unit
        abstract member ManagedRead : Reader -> obj

        abstract member InitializeFrom : Formatter -> unit
        default f.InitializeFrom(f' : Formatter) : unit =
            if f.m_isInitialized then
                invalidOp "Formatter has already been initialized."
            elif not f'.m_isInitialized then 
                invalidOp "Attempting to consume formatter at construction time."
            elif f.Type <> f'.Type && not (f'.m_type.IsAssignableFrom(f.m_type) && f'.UseWithSubtypes) then
                raise <| new InvalidCastException(sprintf "Cannot cast formatter from %O to %O." f'.Type f.Type)
            else
                f.m_type <- f'.m_type
                f.m_typeHash <- f'.m_typeHash
                f.m_typeInfo <- f'.m_typeInfo
                f.m_formatterInfo <- f'.m_formatterInfo
                f.m_cacheObj <- f'.m_cacheObj
                f.m_useWithSubtypes <- f'.m_useWithSubtypes
                f.m_reader_untyped <- f'.m_reader_untyped
                f.m_writer_untyped <- f'.m_writer_untyped
                f.m_isInitialized <- true

    and [<Sealed>] Formatter<'T> =
        inherit Formatter
        
        val mutable private m_writer : Writer -> 'T -> unit
        val mutable private m_reader : Reader -> 'T

        internal new (reader, writer, formatterInfo, cacheObj, useWithSubtypes) = 
            { 
                inherit Formatter(typeof<'T>, mkUntypedReader reader, mkUntypedWriter writer, formatterInfo, cacheObj, useWithSubtypes) ;
                m_writer = writer ;
                m_reader = reader ;
            }

//        internal new (reader, writer, ureader, uwriter, formatterInfo, cacheObj, useWithSubtypes, isTyped) =
//            {
//                inherit Formatter(typeof<'T>, ureader, uwriter, formatterInfo, cacheObj, useWithSubtypes)
////                m_isTyped = isTyped ;
//                m_writer = writer ;
//                m_reader = reader ;
//            }

//        internal new (reader : System.Delegate, writer : System.Delegate, formatterInfo, cacheObj, useWithSubtypes) =
//            let reader = reader :?> Func<Reader, 'T>
//            let writer = writer :?> Func<Writer, 'T, unit>
//            let ru = (fun r -> reader.Invoke r :> obj)
//            let wu = (fun w (o:obj) -> writer.Invoke(w, o :?> 'T))
//            { 
//                inherit Formatter(typeof<'T>, ru, wu, formatterInfo, cacheObj, useWithSubtypes) 
//                m_writer = (fun w t -> writer.Invoke(w,t)) ; 
//                m_reader = reader.Invoke 
//            }

//        internal new (t : Type, reader, writer, formatterInfo, cacheObj, useWithSubtypes) =
//            { 
//                inherit Formatter(t, reader, writer, formatterInfo, cacheObj, useWithSubtypes) ;
//                m_writer = mkTypedWriter writer ;
//                m_reader = mkTypedReader reader ;
//            }

        internal new () = 
            {
                inherit Formatter(typeof<'T>) ;
                m_writer = fun _ _ -> invalidOp "Attempting to consume formatter at construction time." ;
                m_reader = fun _ -> invalidOp "Attempting to consume formatter at construction time." ;
            }

        override f.ManagedWrite (w : Writer) (o : obj) = w.Write(f, o :?> 'T)
        override f.ManagedRead (r : Reader) = r.Read f :> obj

//        override f.Cast<'S> () =
//            if typeof<'T> = typeof<'S> then f :> obj :?> Formatter<'S>
//            elif not <| typeof<'T>.IsAssignableFrom typeof<'S> then
//                raise <| new InvalidCastException(sprintf "Cannot cast formatter from %O to %O." typeof<'T> typeof<'S>)
//            elif not f.UseWithSubtypes then
//                raise <| new InvalidCastException(sprintf "Cannot cast formatter from %O to %O." typeof<'T> typeof<'S>)
//            else
//                new Formatter<'S>(f.Type, f.UntypedRead, f.UntypedWrite, f.FormatterInfo, f.CacheObj, f.UseWithSubtypes)

        override f.InitializeFrom(f' : Formatter) : unit =
            base.InitializeFrom f'
            let writer = f'.UntypedWrite
            let reader = f'.UntypedRead
            f.m_writer <- fun w t -> writer w t
            f.m_reader <- fun r -> reader r :?> 'T
//            match f' with
//            | :? Formatter<'T> as f' ->
//                base.InitializeFrom f'
//                f.m_writer <- f'.m_writer
//                f.m_reader <- f'.m_reader
//            | _ -> 
//                invalidOp "Invalid formatter initialization operation"

        member f.Write = f.m_writer
        member f.Read = f.m_reader


    and IFormatterResolver =
        abstract Resolve<'T> : unit -> Formatter<'T>
        abstract Resolve : Type -> Formatter

    and Writer internal (stream : Stream, resolver : IFormatterResolver, sc : StreamingContext, ?leaveOpen, ?encoding) as self =

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

                    let inline write isProperSubtype (fmt' : Formatter) =
                        let inline write () = 
                            if isProperSubtype then fmt'.UntypedWrite w (x :> _)
                            else fmt.Write w x

                        if fmt'.FormatterInfo = FormatterInfo.ReflectionDerived then
                            write ()
                        else
                            objStack.Push id
                            write ()
                            objStack.Pop () |> ignore
                    
                    if fmt.TypeInfo <= TypeInfo.Sealed || fmt.UseWithSubtypes then
                        writeHeader ObjHeader.isNewInstance
                        write false fmt
                    else
                        // type is not sealed, do subtype resolution
                        let t0 = x.GetType()
                        if t0 <> fmt.Type then
                            let fmt' = resolver.Resolve t0
                            writeHeader (ObjHeader.isNewInstance ||| ObjHeader.isProperSubtype)
                            writeType t0
                            write true fmt'
                        else
                            writeHeader ObjHeader.isNewInstance
                            write false fmt

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
//        ///     Writes object of given type to the underlying stream. Unsafe method.
//        ///     Serialization rules are resolved at runtime based on the type argument.
//        ///     Has to be read with the dual Reader.ReadObj : Type -> obj    
//        /// </summary>
//        /// <param name="t">The reflected type of the serialized object.</param>
//        /// <param name="o">The object to be serialized.</param>
//        member internal w.WriteObj(t : Type, o : obj) =
//            let f = resolver.Resolve t in w.WriteObj(f, o)

        /// <summary>
        ///     Writes given object to the underlying stream.
        ///     Serialization rules are resolved at runtime based on the type argument.
        ///     Object has to be read with the dual Reader.Read&lt;'T&gt; method.
        /// </summary>
        /// <param name="t">The input value.</param>
        member w.Write<'T>(t : 'T) = let f = resolver.Resolve<'T> () in w.Write(f, t)

        member internal w.WriteObj(t : Type, o : obj) =
            let f = resolver.Resolve t in f.ManagedWrite w o

//        member w.Write<'T>(fmt : Formatter<'T>, value : 'T) = w.WriteObj(fmt.Value, value)

//        /// <summary>
//        ///     Writes given object to the stream.
//        ///     Serialization rules are resolved at runtime based on 
//        ///     the reflected type and recorded to the stream.
//        /// </summary>
//        /// <param name="o">The input object.</param>
//        member internal w.WriteObj(o : obj) =
//            if obj.ReferenceEquals(o, null) then bw.Write (ObjHeader.create 0us ObjHeader.isNull)
//            else
//                let t = o.GetType()
//                let f = resolver t
//                bw.Write (ObjHeader.create 0us ObjHeader.empty)
//                writeType t
//                w.WriteObj(f, o)


        interface IDisposable with
            member __.Dispose () = bw.Dispose ()

    and Reader internal (stream : Stream, resolver : IFormatterResolver, sc : StreamingContext, ?leaveOpen, ?encoding) as self =

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
                    objCache.[id] :?> Type

            if ObjHeader.hasFlag flags ObjHeader.isNull then Unchecked.defaultof<'T>
            elif fmt.TypeInfo <= TypeInfo.Value then fmt.Read r
            elif ObjHeader.hasFlag flags ObjHeader.isNewInstance then
                let id = counter
                counter <- counter + 1L

                let read isSubtypeResolved (fmt' : Formatter) =
                    let inline read () = 
                        if isSubtypeResolved then fmt'.UntypedRead r :?> 'T
                        else fmt.Read r

                    let inline checkState () =
                        if currentReflectedObjId <> 0L then
                            raise <| new SerializationException("Internal error: reader state is corrupt.")
                        
                    if fmt'.FormatterInfo = FormatterInfo.ReflectionDerived && fmt'.TypeInfo > TypeInfo.Value then
                        do checkState ()

                        currentReflectedObjId <- id
                        let x = read ()

                        do checkState ()
                        x
                    else
                        let x = read ()
                        objCache.Add(id, x) ; x
                
                if ObjHeader.hasFlag flags ObjHeader.isProperSubtype then
                    let t0 = readType ()
                    let fmt' = resolver.Resolve t0
                    read true fmt'
                else
                    read false fmt

            elif ObjHeader.hasFlag flags ObjHeader.isCachedInstance then
                let id = br.ReadInt64() in objCache.[id] :?> 'T

            elif ObjHeader.hasFlag flags ObjHeader.isProperSubtype then
                let t0 = readType ()
                let f0 = resolver.Resolve t0
                f0.UntypedRead r :?> 'T
            else
                fmt.Read r

        /// <summary>
        ///     Reads object of given type from the underlying stream.
        ///     Serialization rules are resolved at runtime based on the type argument.
        ///     Needs to have been serialized with the dual Write.WriteObj : Type * obj -> unit    
        /// </summary>
        /// <param name="t">The reflected type of the deserialized object.</param>
//        member internal r.ReadObj (t : Type) : obj = let f = resolver t in r.ReadObj f

        /// <summary>
        ///     Reads object of given type from the underlying stream.
        ///     Serialization rules are resolved at runtime based on the object header.
        ///     Needs to have been serialized with the dual Writer.Write&lt;'T&gt; method.
        /// </summary>
        member r.Read<'T> () : 'T = let f = resolver.Resolve<'T> () in r.Read f

        member internal r.ReadObj(t : Type) = let f = resolver.Resolve t in f.ManagedRead r
        
//        /// <summary>
//        ///     Reads object from the underlying stream.
//        ///     Serialization rules are resolved at runtime based on the object header.
//        ///     Needs to have been serialized with the dual Writer.WriteObj : obj -> unit
//        /// </summary>
//        member r.ReadObj () =
//            let flags = ObjHeader.read 0us (br.ReadUInt32())
//            if ObjHeader.hasFlag flags ObjHeader.isNull then null
//            else
//                let t = readType ()
//                let f = resolver t
//                r.ReadObj f

        interface IDisposable with
            member __.Dispose () = br.Dispose ()
