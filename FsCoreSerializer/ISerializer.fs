namespace FsCoreSerializer

    open System.IO

    type ISerializer =
        abstract Serialize : obj * ?context : obj -> byte [] 
        abstract Deserialize : byte [] * ?context : obj -> obj

        abstract Serialize : stream : Stream * graph : obj * ?context : obj -> unit
        abstract Deserialize : stream : Stream * ?context : obj -> obj

        // provision for serialization of multiple objects on the same stream
        abstract WriteObj : stream : Stream * graph : obj * ?context : obj -> unit
        abstract ReadObj : stream : Stream * ?context : obj -> obj