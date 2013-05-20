namespace FsCoreSerializer

    // a few standard .NET serializers implementing the ISerializer interface

    open System
    open System.IO
    open System.IO.Compression
    open System.Runtime.Serialization
    open System.Runtime.Serialization.Formatters.Binary

    open FsCoreSerializer.Utils

    type BinaryFormatterSerializer() =
        let defaultFormatter = new BinaryFormatter()

        let serialize (stream : Stream) (graph : obj) (context : obj option) =
            try
                let formatter = 
                    match context with 
                    | Some ctx -> new BinaryFormatter(new SurrogateSelector(), StreamingContext(StreamingContextStates.All, ctx))
                    | None -> defaultFormatter

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



    type NDCSerializer() =
        let defaultFormatter = new NetDataContractSerializer()
    
        let serialize (stream : Stream) (graph : obj) (context : obj option) =
            try
                let formatter = 
                    match context with 
                    | Some ctx -> new NetDataContractSerializer(StreamingContext(StreamingContextStates.All, ctx))
                    | None -> defaultFormatter

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