namespace FsCoreSerializer

    open System
    open System.IO
    open System.Text
    open System.Runtime.CompilerServices
    open System.Collections.Generic
    open System.Runtime.Serialization


    // stores precomputed information for the type being serialized
    type internal TypeInfo =
        | Primitive = 0
        | Enum = 1
        | Value = 2
        | Array = 3
        | Sealed = 4
        | NonSealed = 5
        | Abstract = 6

    type internal FormatterInfo =
        | Atomic = 0
        | ReflectionType = 1
        | ReflectionDerived = 2
        | ISerializable = 3
        | FSharpValue = 4 // tuples, records and DUs
        | IFsCoreSerializable = 5
        | Custom = 6

    type Formatter =
        internal {
            Type : Type

            Write : Writer -> obj -> unit
            Read : Reader -> obj

            TypeInfo : TypeInfo
            TypeHash : uint16
            FormatterInfo : FormatterInfo

            CacheObj : bool
            UseWithSubtypes : bool
        }
    with
        member __.SerializingType = __.Type

    and [<Sealed>] Writer internal (stream : Stream, resolver : Type -> Formatter, sc : StreamingContext, ?leaveOpen, ?encoding) =
        // using UTF8 gives an observed performance improvement ~200%
        let encoding = defaultArg encoding Encoding.UTF8

        let bw = new BinaryWriter(stream, encoding, defaultArg leaveOpen true)
        let idGen = new ObjectIDGenerator()
        let objStack = new Stack<int64> ()

        let writeType (t : Type) =
            let mutable firstOccurence = false
            let id = idGen.GetId(t, &firstOccurence)
            bw.Write firstOccurence
            if firstOccurence then TypeFormatter.Write bw t
            else
                bw.Write id

        /// BinaryWriter to the underlying stream.
        member w.BW = bw

        /// Access the current streaming context.
        member w.StreamingContext = sc

        /// <summary>Precomputes formatter for the given type at runtime.</summary>
        /// <param name="t">The input type.</param>
        member w.ResolveFormatter (t : Type) : Formatter = resolver t

        /// <summary>
        ///     Write object to stream using given formatter rules. Unsafe method.
        ///     Has to be deserialized with the dual method Reader.ReadObj : Formatter -> obj.
        /// </summary>
        /// <param name="fmt">Formatter used in serialization. Needs to be compatible with input object.</param>
        /// <param name="o">The input object.</param>
        member w.WriteObj (fmt : Formatter, o : obj) =
            
            do RuntimeHelpers.EnsureSufficientExecutionStack()

            let inline writeHeader (flags : byte) =
                bw.Write(ObjHeader.create fmt.TypeHash flags)

            if obj.ReferenceEquals(o, null) then writeHeader ObjHeader.isNull
            elif fmt.TypeInfo <= TypeInfo.Value then 
                writeHeader ObjHeader.empty
                fmt.Write w o
            elif fmt.CacheObj then
                let mutable firstOccurence = false
                let id = idGen.GetId(o, &firstOccurence)
                if firstOccurence then
                    let inline writeObj (fmt : Formatter) =
                        if fmt.FormatterInfo = FormatterInfo.ReflectionDerived then
                            fmt.Write w o
                        else
                            objStack.Push id
                            fmt.Write w o
                            objStack.Pop () |> ignore

                    if fmt.TypeInfo <= TypeInfo.Sealed || fmt.UseWithSubtypes then
                        writeHeader ObjHeader.isNewInstance
                        writeObj fmt
                    else
                        // type is not sealed, do subtype resolution
                        let t0 = o.GetType()
                        if t0 <> fmt.Type then
                            let fmt' = resolver t0
                            writeHeader (ObjHeader.isNewInstance ||| ObjHeader.isProperSubtype)
                            writeType t0
                            writeObj fmt'
                        else
                            writeHeader ObjHeader.isNewInstance
                            writeObj fmt

                elif objStack.Contains id then
                    raise <| new SerializationException(sprintf "Unexpected cyclic object graph [%s]." fmt.Type.FullName)
                else
                    writeHeader ObjHeader.isCachedInstance
                    bw.Write id
            else
                if fmt.TypeInfo <= TypeInfo.Sealed || fmt.UseWithSubtypes then
                    writeHeader ObjHeader.empty
                    fmt.Write w o
                else
                    // type is not sealed, do subtype resolution
                    let t0 = o.GetType ()
                    if t0 <> fmt.Type then
                        let f0 = resolver t0
                        writeHeader ObjHeader.isProperSubtype
                        f0.Write w o
                    else
                        writeHeader ObjHeader.empty
                        fmt.Write w o

        /// <summary>
        ///     Writes object of given type to the underlying stream. Unsafe method.
        ///     Serialization rules are resolved at runtime based on the type argument.
        ///     Has to be read with the dual Reader.ReadObj : Type -> obj    
        /// </summary>
        /// <param name="t">The reflected type of the serialized object.</param>
        /// <param name="o">The object to be serialized.</param>
        member w.WriteObj(t : Type, o : obj) = let f = resolver t in w.WriteObj(f, o)

        /// <summary>
        ///     Writes given object to the underlying stream.
        ///     Serialization rules are resolved at runtime based on the type argument.
        ///     Object has to be read with the dual Reader.Read&lt;'T&gt; method.
        /// </summary>
        /// <param name="t">The input value.</param>
        member w.Write<'T>(t : 'T) = let f = resolver typeof<'T> in w.WriteObj(f, t)

        /// <summary>
        ///     Writes given object to the stream.
        ///     Serialization rules are resolved at runtime based on 
        ///     the reflected type and recorded to the stream.
        /// </summary>
        /// <param name="o">The input object.</param>
        member w.WriteObj(o : obj) =
            if obj.ReferenceEquals(o, null) then bw.Write (ObjHeader.create 0us ObjHeader.isNull)
            else
                let t = o.GetType()
                let f = resolver t
                bw.Write (ObjHeader.create 0us ObjHeader.empty)
                writeType t
                w.WriteObj(f, o)


        interface IDisposable with
            member __.Dispose () = bw.Dispose ()

    and [<Sealed>] Reader internal (stream : Stream, resolver : Type -> Formatter, sc : StreamingContext, ?leaveOpen, ?encoding) =
        // using UTF8 gives an observed performance improvement ~200%
        let encoding = defaultArg encoding Encoding.UTF8

        let br = new BinaryReader(stream, encoding, defaultArg leaveOpen true)
        let objCache = new Dictionary<int64, obj> ()
        let mutable counter = 1L
        let mutable currentReflectedObjId = 0L

        let readType () =
            if br.ReadBoolean () then
                let t = TypeFormatter.Read br
                objCache.Add(counter, t)
                counter <- counter + 1L
                t
            else
                let id = br.ReadInt64()
                objCache.[id] :?> Type

        // objects deserialized with reflection-based rules are registered to the cache
        // at the initialization stage to support cyclic object graphs.
        member internal r.EarlyRegisterObject (o : obj) =
            if currentReflectedObjId = 0L then
                raise <| new SerializationException("Unexpected reflected object binding.")
            else
                objCache.Add(currentReflectedObjId, o)
                currentReflectedObjId <- 0L

        /// BinaryReader to the underlying stream.
        member r.BR = br

        /// Access the current streaming context.
        member r.StreamingContext = sc

        /// <summary>Precomputes formatter for the given type at runtime.</summary>
        /// <param name="t">The input type.</param>
        member r.ResolveFormatter (t : Type) = resolver t


        /// <summary>
        ///     Read object from stream using given formatter rules.
        ///     Needs to have been serialized with the dual method Writer.WriteObj : Formatter * obj -> unit.
        /// </summary>
        /// <param name="fmt">Formatter used in deserialization. Needs to be compatible with input object.</param>
        /// <param name="o">The input object.</param>
        member r.ReadObj(fmt : Formatter) =
            let flags = ObjHeader.read fmt.TypeHash (br.ReadUInt32())

            if ObjHeader.hasFlag flags ObjHeader.isNull then null
            elif fmt.TypeInfo <= TypeInfo.Value then fmt.Read r
            elif ObjHeader.hasFlag flags ObjHeader.isNewInstance then
                let id = counter
                counter <- counter + 1L

                let inline readObj (fmt : Formatter) =
                    let inline checkState () =
                        if currentReflectedObjId <> 0L then
                            raise <| new SerializationException("Internal error: reader state is corrupt.")
                        
                    if fmt.FormatterInfo = FormatterInfo.ReflectionDerived && fmt.TypeInfo > TypeInfo.Value then
                        do checkState ()

                        currentReflectedObjId <- id
                        let o = fmt.Read r

                        do checkState ()
                        o
                    else
                        let o = fmt.Read r
                        objCache.Add(id, o) ; o
                
                if ObjHeader.hasFlag flags ObjHeader.isProperSubtype then
                    let t0 = readType ()
                    let fmt' = resolver t0
                    readObj fmt'
                else
                    readObj fmt

            elif ObjHeader.hasFlag flags ObjHeader.isCachedInstance then
                let id = br.ReadInt64() in objCache.[id]

            elif ObjHeader.hasFlag flags ObjHeader.isProperSubtype then
                let t0 = readType ()
                let f0 = resolver t0
                f0.Read r
            else
                fmt.Read r

        /// <summary>
        ///     Reads object of given type from the underlying stream.
        ///     Serialization rules are resolved at runtime based on the type argument.
        ///     Needs to have been serialized with the dual Write.WriteObj : Type * obj -> unit    
        /// </summary>
        /// <param name="t">The reflected type of the deserialized object.</param>
        member r.ReadObj (t : Type) : obj = let f = resolver t in r.ReadObj f

        /// <summary>
        ///     Reads object of given type from the underlying stream.
        ///     Serialization rules are resolved at runtime based on the object header.
        ///     Needs to have been serialized with the dual Writer.Write&lt;'T&gt; method.
        /// </summary>
        member r.Read<'T> () : 'T = let f = resolver typeof<'T> in r.ReadObj f :?> 'T
        
        /// <summary>
        ///     Reads object from the underlying stream.
        ///     Serialization rules are resolved at runtime based on the object header.
        ///     Needs to have been serialized with the dual Writer.WriteObj : obj -> unit
        /// </summary>
        member r.ReadObj () =
            let flags = ObjHeader.read 0us (br.ReadUInt32())
            if ObjHeader.hasFlag flags ObjHeader.isNull then null
            else
                let t = readType ()
                let f = resolver t
                r.ReadObj f

        interface IDisposable with
            member __.Dispose () = br.Dispose ()
