namespace FsCoreSerializer

    open System
    open System.IO
    open System.IO.Compression
    open System.Text
    open System.Collections
    open System.Collections.Generic
    open System.Runtime.Serialization

    open FsCoreSerializer
    open FsCoreSerializer.Utils
    open FsCoreSerializer.FsCoreFormatterImpl
    open FsCoreSerializer.AdditionalFormatters


    type FsCoreSerializerRegistry private () =
        static let add (t : Type) (serializer : Formatter option) (m : Map<_,_>) =
            m.Add(t.AssemblyQualifiedName, serializer)

        static let formatters : Atom<Map<string, Formatter option>> =
            let initial = mkFormatterCache (primitives @ reflectionFormatters @ [exprFormatter])
            let initial = add typeof<obj> None initial
            Atom.atom initial

        static let genericFormatters : Atom<Map<string, GenericFormatterDescriptor>> =
            Atom.atom (mkGenericFormatterEntries (gExprFormatter :: genericFormatters))

        static member TryResolve(t : Type) =
            match formatters.Value.TryFind t.AssemblyQualifiedName with
            | None -> 
                let fmt = tryResolveFormatter memberInfoFormatter genericFormatters.Value formatters.Value CacheByRef t
                formatters.Swap(add t fmt); fmt
            | x -> Option.bind id x

        static member TryResolve<'T> () = FsCoreSerializerRegistry.TryResolve typeof<'T>

        static member Register(fmt : Formatter) = formatters.Swap(add fmt.Type (Some fmt))
        static member Register(writer : Writer -> 'T -> unit, reader : Reader -> 'T, 
                                        requiresExternalSerializer, ?applyToSubtypes, ?cacheMode) =
                let cacheMode = defaultArg cacheMode CacheByRef
                let applyToSubtypes = defaultArg applyToSubtypes false
                let fmt = mkFormatter cacheMode requiresExternalSerializer applyToSubtypes false reader writer
                FsCoreSerializerRegistry.Register(fmt)

        static member MarkForExternalSerialization (t : Type) = formatters.Swap(add t None)

        static member RegisterGenericFormatter(targetKind : Type, implementationKind : Type) =
            let descr = mkGenericFormatterDescr targetKind implementationKind
            genericFormatters.Swap(fun m -> m.Add(targetKind.AssemblyQualifiedName, descr))


    module private FsCoreSerializerImpl =

        let sfailwith inner msg = 
            match inner with 
            | None -> SerializationException(msg) 
            | Some e -> SerializationException(msg, e)
            |> raise

        let sfailwithf inner fmt = Printf.ksprintf (fun msg -> sfailwith inner msg) fmt

        let inline mkWriter (bw : BinaryWriter) (objWriter : obj -> unit) ctx =
            new Writer(bw, objWriter, typeFormatter, FsCoreSerializerRegistry.TryResolve, ?context = ctx)

        let inline mkReader (br : BinaryReader) (objReader : unit -> obj) ctx =
            new Reader(br, objReader, typeFormatter, FsCoreSerializerRegistry.TryResolve, ?context = ctx)

        let writeNoExternal (formatter : Formatter) (bw : BinaryWriter) (o : obj) ctx =
            let objWriter _ = invalidOp "Invalid serialization stream."
            let w = mkWriter bw objWriter ctx
            formatter.Writer w o

        let readNoExternal (formatter : Formatter) (br : BinaryReader) ctx =
            let objReader _ = invalidOp "Invalid serialization stream."
            let r = mkReader br objReader ctx
            formatter.Reader r

        let writeExtSequential (serializer : ISerializer) (formatter : Formatter) (bw : BinaryWriter) (o : obj) ctx =
            let objWriter (o : obj) = serializer.WriteObj(bw.BaseStream, o, ?context = ctx)
            let w = mkWriter bw objWriter ctx
            formatter.Writer w o

        let readExtSequential (serializer : ISerializer) (formatter : Formatter) (br : BinaryReader) ctx =
            let objReader () = serializer.ReadObj(br.BaseStream, ?context = ctx)
            let r = mkReader br objReader ctx
            formatter.Reader r

        let writeExtObjQueue encoding (serializer : ISerializer) (formatter : Formatter) (bw : BinaryWriter) (o : obj) ctx =
            let objQueue = new Queue<obj> ()
            let objWriter (o : obj) = objQueue.Enqueue o

            // need to perform serialization on temporary stream
            use mem = new MemoryStream()
            do
                use bw = new BinaryWriter(mem, encoding, true)
                let w = mkWriter bw objWriter ctx
                formatter.Writer w o

            // start writing to underlying stream
            match Seq.toArray objQueue with
            | [| |] -> bw.Write true
            | objs ->
                bw.Write false
                serializer.WriteObj(bw.BaseStream, objs, ?context = ctx)

            mem.WriteTo bw.BaseStream

        let readExtObjQueue (serializer : ISerializer) (formatter : Formatter) (br : BinaryReader) ctx =
            let objReader =
                if br.ReadBoolean() then
                    fun _ -> invalidOp "Invalid serialization stream."
                else
                    let objs = serializer.ReadObj(br.BaseStream, ?context = ctx) :?> obj []
                    let e = objs.GetEnumerator()
                    fun () -> if e.MoveNext() then e.Current else invalidOp "Invalid serialization stream."

            let r = mkReader br objReader ctx
            formatter.Reader r


        let write standalone objQueue encoding (serializer : ISerializer) (stream : Stream) (o : obj) ctx =
            use bw = new BinaryWriter(stream, encoding, true)

            if o = null then bw.Write 0uy
            else
                let t = o.GetType()
                match FsCoreSerializerRegistry.TryResolve t with
                | None -> 
                    bw.Write 1uy 
                    if standalone then serializer.Serialize(stream, o, ?context = ctx)
                    else serializer.WriteObj(stream, o, ?context = ctx)
                | Some fmt ->
                    bw.Write 2uy

                    try
                        // record serializer object type
                        do writeNoExternal typeFormatter bw t None
                        
                        if fmt.RequiresExternalSerializer then
                            if objQueue then bw.Write 0uy ; writeExtObjQueue encoding serializer fmt bw o ctx
                            else bw.Write 1uy ; writeExtSequential serializer fmt bw o ctx
                        else bw.Write 2uy ; writeNoExternal fmt bw o ctx
                    with 
                    | e -> sfailwithf (Some e) "error when serializing object of type %s." <| fmt.Type.ToString()

        let read standalone encoding (serializer : ISerializer) (stream : Stream) ctx =
            use br = new BinaryReader(stream, encoding, true)
            match br.ReadByte() with
            | 0uy -> null
            | 1uy -> 
                if standalone then serializer.Deserialize(stream, ?context = ctx)
                else serializer.ReadObj(stream, ?context = ctx)
            | 2uy ->
                    let t = readNoExternal typeFormatter br None :?> Type

                    match FsCoreSerializerRegistry.TryResolve t with
                    | None -> sfailwithf None "Could not resolve formatter of type %s." <| t.ToString()
                    | Some fmt ->
                        try
                            match br.ReadByte() with
                            | 0uy -> readExtObjQueue serializer fmt br ctx
                            | 1uy -> readExtSequential serializer fmt br ctx
                            | 2uy -> readNoExternal fmt br ctx
                            | _ -> invalidOp "Invalid serialization stream."
                        with e -> sfailwithf (Some e) "error when deserializing object of type %s." <| fmt.Type.ToString()
            | _ -> invalidOp "Invalid serialization stream."

    
    open FsCoreSerializerImpl

    type FsCoreSerializer(?externalSerializer, ?encoding, ?compress, ?optimizeForMemory) =
        let useObjQueue = not <| defaultArg optimizeForMemory true
        let encoding = defaultArg encoding Encoding.UTF8
        let compress = defaultArg compress false
        let extS = defaultArg externalSerializer (new NDCSerializer(compress = false) :> ISerializer)

        let serialize standalone (stream : Stream) (o : obj) ctx =
            try
                if compress then
                    if standalone then
                        use gZ = new GZipStream(stream, CompressionMode.Compress, true)
                        write true useObjQueue encoding extS gZ o ctx
                        gZ.Close()
                    else
                        use mem = new MemoryStream()
                        use gZ = new GZipStream(mem, CompressionMode.Compress, true)
                        write true useObjQueue encoding extS gZ o ctx
                        gZ.Close()

                        // write to underlying stream
                        writeInt stream (int mem.Length)
                        mem.WriteTo stream
                else
                    write standalone useObjQueue encoding extS stream o ctx
            with
            | :? SerializationException as e -> sfailwithf (Some e.InnerException) "FsCoreSerializer: %s" e.Message
            | e -> sfailwith (Some e) "FsCoreSerializer exception."

        let deserialize standalone (stream : Stream) ctx =
            try
                if compress then
                    if standalone then
                        use gZ = new GZipStream(stream, CompressionMode.Decompress, true)
                        read true encoding extS gZ ctx
                    else
                        let length = readInt stream
                        let buf = Array.zeroCreate length
                        stream.Read(buf, 0, length) |> ignore

                        use mem = new MemoryStream(buf)
                        use gZ = new GZipStream(mem, CompressionMode.Decompress, true)
                        read true encoding extS gZ ctx
                else
                    read standalone encoding extS stream ctx
            with
            | :? SerializationException as e -> sfailwithf (Some e.InnerException) "FsCoreSerializer: %s" e.Message
            | e -> sfailwith (Some e) "FsCoreSerializer exception."

        interface ISerializer with
            member __.Serialize(o : obj, ?context) =
                use mem = new MemoryStream()
                do serialize true mem o context
                mem.ToArray()

            member __.Deserialize(bytes : byte [], ?context) =
                use mem = new MemoryStream(bytes)
                deserialize true mem context

            member __.Serialize(stream : Stream, o : obj, ?context) = serialize true stream o context
            member __.Deserialize(stream : Stream, ?context) = deserialize true stream context

            member __.WriteObj (stream : Stream, o : obj, ?context : obj) = serialize false stream o context
            member __.ReadObj (stream : Stream, ?context : obj) = deserialize false stream context