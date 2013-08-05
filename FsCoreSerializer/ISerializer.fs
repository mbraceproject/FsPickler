namespace FsCoreSerializer

    open System.IO

    type ISerializer =
        /// Name of the serializer implementation.
        abstract Name : string

        /// <summary>Serializes an object to a stream.</summary>
        /// <param name="stream">The target stream.</param>
        /// <param name="graph">The input object graph.</param>
        /// <param name="context">Object passed into the streaming context.</param>
        abstract Serialize : stream : Stream * graph : obj * ?context : obj -> unit

        /// <summary>Deserializes an object from a stream.</summary>
        /// <param name="stream">The source stream.</param>
        /// <param name="context">Object passed into the streaming context.</param>
        abstract Deserialize : stream : Stream * ?context : obj -> obj

        /// <summary>Serializes an object to a byte array.</summary>
        /// <param name="graph">The input object graph.</param>
        /// <param name="context">Object passed into the streaming context.</param>
        abstract Serialize : graph : obj * ?context : obj -> byte [] 

        /// <summary>Deserializes an object from a byte array.</summary>
        /// <param name="data">The input byte array.</param>
        /// <param name="context">Object passed into the streaming context.</param>
        abstract Deserialize : data : byte [] * ?context : obj -> obj