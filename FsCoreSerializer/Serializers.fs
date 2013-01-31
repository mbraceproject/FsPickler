namespace FsCoreSerializer

    // a few standard .NET serializers implementing the ISerializer interface

    open System
    open System.IO
    open System.IO.Compression
    open System.Runtime.Serialization
    open System.Runtime.Serialization.Formatters.Binary

    open FsCoreSerializer.Utils

    type BinaryFormatterSerializer(?compress : bool) =
        let compress = defaultArg compress false
        let defaultFormatter = new BinaryFormatter()

        let serialize (stream : Stream) (graph : obj) (context : obj option) =
            try
                let formatter = 
                    match context with 
                    | Some ctx -> new BinaryFormatter(new SurrogateSelector(), StreamingContext(StreamingContextStates.All, ctx))
                    | None -> defaultFormatter

                if compress then
                    use gZ = new GZipStream(stream, CompressionMode.Compress, true)
                    formatter.Serialize(gZ, graph)
                    gZ.Close()
                else
                    formatter.Serialize(stream, graph)
            with 
            | :? SerializationException -> reraise ()
            | e -> raise <| new SerializationException("BinaryFormatter error.", e)

        let deserialize (stream : Stream) (context : obj option) : obj =
            try
                let formatter = 
                    match context with 
                    | Some ctx -> new BinaryFormatter(new SurrogateSelector(), StreamingContext(StreamingContextStates.All, ctx))
                    | None -> defaultFormatter

                if compress then
                    use gZ = new GZipStream(stream, CompressionMode.Decompress, true)
                    formatter.Deserialize(gZ)
                else
                    formatter.Deserialize(stream)
            with 
            | :? SerializationException -> reraise ()
            | e -> raise <| new SerializationException("BinaryFormatter error.", e)

        interface ISerializer with
            member __.Serialize (graph : obj, ?context : obj) : byte[] =
                use mem = new MemoryStream()
                serialize mem graph context
                mem.ToArray()

            member __.Deserialize (bytes : byte[], ?context : obj) : obj =
                use mem = new MemoryStream(bytes)
                deserialize mem context

            member __.Serialize (stream : Stream, graph : obj, ?context : obj) : unit = serialize stream graph context
            member __.Deserialize (stream : Stream, ?context : obj) : obj = deserialize stream context

            member __.WriteObj (stream : Stream, graph : obj, ?context : obj) : unit =
                try
                    if compress then
                        use mem = new MemoryStream()
                        do serialize mem graph context
                        writeInt stream (int mem.Length)
                        mem.WriteTo stream
                    else 
                        serialize stream graph context
                with
                | :? SerializationException -> reraise ()
                | e -> raise <| new SerializationException("BinaryFormatter error.", e)

            member __.ReadObj (stream : Stream, ?context : obj) : obj =
                try
                    if compress then
                        let length = readInt stream
                        let buf = Array.zeroCreate length
                        stream.Read(buf, 0, length) |> ignore
                        use mem = new MemoryStream(buf)
                        deserialize mem context
                    else
                        deserialize stream context
                with
                | :? SerializationException -> reraise ()
                | e -> raise <| new SerializationException("BinaryFormatter error.", e)


    type NDCSerializer(?compress : bool) =
        let compress = defaultArg compress false
        let defaultFormatter = new NetDataContractSerializer()
    
        let serialize (stream : Stream) (graph : obj) (context : obj option) =
            try
                let formatter = 
                    match context with 
                    | Some ctx -> new NetDataContractSerializer(StreamingContext(StreamingContextStates.All, ctx))
                    | None -> defaultFormatter

                if compress then
                    use gZ = new GZipStream(stream, CompressionMode.Compress, true)
                    formatter.Serialize(gZ, graph)
                    gZ.Close()
                else
                    formatter.Serialize(stream, graph)
            with
            | :? SerializationException -> reraise ()
            | e -> raise <| new SerializationException("NetDataContractSerializer error.", e)


        let deserialize (stream : Stream) (context : obj option) : obj =
            try
                let formatter = 
                    match context with 
                    | Some ctx -> new NetDataContractSerializer(StreamingContext(StreamingContextStates.All, ctx))
                    | None -> defaultFormatter

                if compress then
                    use gZ = new GZipStream(stream, CompressionMode.Decompress)
                    formatter.Deserialize(gZ)
                else
                    formatter.Deserialize(stream)
            with 
            | :? SerializationException as e -> reraise ()
            | e -> raise <| new SerializationException("NetDataContractSerializer error.", e)

        interface ISerializer with
            member __.Serialize (graph : obj, ?context : obj) : byte[] =
                use mem = new MemoryStream()
                do serialize mem graph context
                mem.ToArray()
            member __.Deserialize (bytes : byte[], ?context : obj) : obj =
                use mem = new MemoryStream(bytes)
                deserialize mem context

            member __.Serialize(stream : Stream, graph : obj, ?context : obj) : unit = serialize stream graph context
            member __.Deserialize (stream : Stream, ?context : obj) : obj = deserialize stream context

            member __.WriteObj (stream : Stream, graph : obj, ?context : obj) : unit =
                try
                    use mem = new MemoryStream()
                    do serialize mem graph context
                    writeInt stream (int mem.Length)
                    mem.WriteTo stream
                with
                | :? SerializationException -> reraise ()
                | e -> raise <| new SerializationException("NetDataContractSerializer error.", e)
            
            member __.ReadObj (stream : Stream, ?context : obj) : obj = 
                try
                    let length = readInt stream
                    let buf = Array.zeroCreate length
                    stream.Read(buf, 0, length) |> ignore
                    use mem = new MemoryStream(buf)
                    deserialize mem context
                with
                | :? SerializationException -> reraise ()
                | e -> raise <| new SerializationException("NetDataContractSerializer error.", e)