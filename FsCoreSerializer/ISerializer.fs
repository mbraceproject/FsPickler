namespace FsCoreSerializer

    open System.IO

    type ISerializer =
        abstract Name : string

        abstract Serialize : obj * ?context : obj -> byte [] 
        abstract Deserialize : byte [] * ?context : obj -> obj

        abstract Serialize : stream : Stream * graph : obj * ?context : obj -> unit
        abstract Deserialize : stream : Stream * ?context : obj -> obj