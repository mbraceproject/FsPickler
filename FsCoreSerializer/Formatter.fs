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


    // TODO : signature file

    and [<Sealed>] Writer internal (stream : Stream, resolver : Type -> Formatter, sc : StreamingContext, ?leaveOpen) =

        let bw = new BinaryWriter(stream, Encoding.UTF8, defaultArg leaveOpen true)
        let idGen = new ObjectIDGenerator()
        let objStack = new Stack<int64> ()

        let writeType (t : Type) =
            let mutable firstOccurence = false
            let id = idGen.GetId(t, &firstOccurence)
            bw.Write firstOccurence
            if firstOccurence then TypeFormatter.Default.Write bw t
            else
                bw.Write id

        /// BinaryWriter to the underlying stream.
        member w.BW = bw
        /// Access the current streaming context.
        member w.StreamingContext = sc
        /// Resolves a formatter for the given type.
        member w.ResolveFormatter (t : Type) = resolver t
        /// Write object to stream using given formatter. Unsafe!
        member w.WriteObj (f : Formatter, o : obj) =
            do RuntimeHelpers.EnsureSufficientExecutionStack()

            let inline writeHeader (flags : byte) =
                bw.Write(ObjHeader.create f.TypeHash flags)

            if obj.ReferenceEquals(o, null) then writeHeader ObjHeader.isNull
            elif f.TypeInfo <= TypeInfo.Value then 
                writeHeader ObjHeader.empty
                f.Write w o
            elif f.CacheObj then
                let mutable firstOccurence = false
                let id = idGen.GetId(o, &firstOccurence)
                if firstOccurence then
                    let inline writeObj (f : Formatter) =
                        if f.FormatterInfo = FormatterInfo.ReflectionDerived then
                            f.Write w o
                        else
                            objStack.Push id
                            f.Write w o
                            objStack.Pop () |> ignore

                    if f.TypeInfo <= TypeInfo.Sealed || f.UseWithSubtypes then
                        writeHeader ObjHeader.isNewInstance
                        writeObj f
                    else
                        // type is not sealed, do subtype resolution
                        let t0 = o.GetType()
                        if t0 <> f.Type then
                            let f0 = resolver t0
                            writeHeader (ObjHeader.isNewInstance ||| ObjHeader.isProperSubtype)
                            writeType t0
                            writeObj f0
                        else
                            writeHeader ObjHeader.isNewInstance
                            writeObj f

                elif objStack.Contains id then
                    raise <| new SerializationException(sprintf "Unexpected cyclic object graph [%s]." f.Type.FullName)
                else
                    writeHeader ObjHeader.isCachedInstance
                    bw.Write id
            else
                if f.TypeInfo <= TypeInfo.Sealed || f.UseWithSubtypes then
                    writeHeader ObjHeader.empty
                    f.Write w o
                else
                    // type is not sealed, do subtype resolution
                    let t0 = o.GetType ()
                    if t0 <> f.Type then
                        let f0 = resolver t0
                        writeHeader ObjHeader.isProperSubtype
                        f0.Write w o
                    else
                        writeHeader ObjHeader.empty
                        f.Write w o

        /// Writes given object to the stream
        member w.Write<'T>(t : 'T) = let f = resolver typeof<'T> in w.WriteObj(f, t)
        /// Writes given object of type 't' to the stream. Unsafe!
        member w.WriteObj(t : Type, o : obj) = let f = resolver t in w.WriteObj(f, o)
        /// Writes given object to the stream.
        /// Formatter type is resolved through reflection and recorded to the stream.
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

    and [<Sealed>] Reader internal (stream : Stream, resolver : Type -> Formatter, sc : StreamingContext, ?leaveOpen) =

        let br = new BinaryReader(stream, Encoding.UTF8, defaultArg leaveOpen true)
        let objCache = new Dictionary<int64, obj> ()
        let mutable counter = 1L
        let mutable currentReflectedObjId = 0L

        let readType () =
            if br.ReadBoolean () then
                let t = TypeFormatter.Default.Read br
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
        /// Resolves a formatter for the given type.
        member r.ResolveFormatter (t : Type) = resolver t
        /// Read object from stream using given formatter. Unsafe!
        member r.ReadObj(f : Formatter) =
            let flags = ObjHeader.read f.TypeHash (br.ReadUInt32())

            if ObjHeader.hasFlag flags ObjHeader.isNull then null
            elif f.TypeInfo <= TypeInfo.Value then f.Read r
            elif ObjHeader.hasFlag flags ObjHeader.isNewInstance then
                let id = counter
                counter <- counter + 1L

                let inline readObj (f : Formatter) =
                    let inline checkState () =
                        if currentReflectedObjId <> 0L then
                            raise <| new SerializationException("Internal error: reader state is corrupt.")
                        
                    if f.FormatterInfo = FormatterInfo.ReflectionDerived && f.TypeInfo > TypeInfo.Value then
                        do checkState ()

                        currentReflectedObjId <- id
                        let o = f.Read r

                        do checkState ()
                        o
                    else
                        let o = f.Read r
                        objCache.Add(id, o) ; o
                
                if ObjHeader.hasFlag flags ObjHeader.isProperSubtype then
                    let t0 = readType ()
                    let f0 = resolver t0
                    readObj f0
                else
                    readObj f

            elif ObjHeader.hasFlag flags ObjHeader.isCachedInstance then
                let id = br.ReadInt64() in objCache.[id]

            elif ObjHeader.hasFlag flags ObjHeader.isProperSubtype then
                let t0 = readType ()
                let f0 = resolver t0
                f0.Read r
            else
                f.Read r
        
        /// Reads object of given type from the stream.
        member r.Read<'T> () = let f = resolver typeof<'T> in r.ReadObj f :?> 'T
        /// Reads object of given type from the stream.
        member r.ReadObj (t : Type) = let f = resolver t in r.ReadObj f
        /// Reads next object from the stream.
        /// Object type is read from the stream and formatter resolved on the fly.
        member r.ReadObj () =
            let flags = ObjHeader.read 0us (br.ReadUInt32())
            if ObjHeader.hasFlag flags ObjHeader.isNull then null
            else
                let t = readType ()
                let f = resolver t
                r.ReadObj f

        interface IDisposable with
            member __.Dispose () = br.Dispose ()
