namespace FsCoreSerializer.Tests
    
    open System
    open System.IO
    open System.Runtime.Serialization
    open System.Runtime.Serialization.Formatters.Binary

    open FsCoreSerializer

    type ISerializer =
        abstract Name : string
        abstract Serialize : Stream * obj -> unit
        abstract Deserialize : Stream -> obj

    type TestFsCoreSerializer (?registry : FormatterRegistry) =
        let fsc = match registry with None -> new FsCoreSerializer() | Some r -> new FsCoreSerializer(r)

        member __.FSCS = fsc

        interface ISerializer with
            member __.Name = "FsCoreSerializer"
            member __.Serialize(stream : Stream, o : obj) = fsc.Serialize<obj>(stream, o)
            member __.Deserialize(stream : Stream) = fsc.Deserialize<obj> stream

    type TestBinaryFormatter () =
        let bfs = new BinaryFormatter()

        interface ISerializer with
            member __.Name = "BinaryFormatter"
            member __.Serialize(stream : Stream, o : obj) = bfs.Serialize(stream, o)
            member __.Deserialize(stream : Stream) = bfs.Deserialize stream

    type TestNetDataContractSerializer () =
        let ndc = new NetDataContractSerializer()

        interface ISerializer with
            member __.Name = "NetDataContractSerializer"
            member __.Serialize(stream : Stream, o : obj) = ndc.Serialize(stream, o)
            member __.Deserialize(stream : Stream) = ndc.Deserialize stream


    module Serializer =
        
        let write (s : ISerializer) (o : obj) =
            use m = new MemoryStream()
            s.Serialize(m, o)
            m.ToArray()

        let read (s : ISerializer) (bytes : byte []) =
            use m = new MemoryStream(bytes)
            s.Deserialize m

        let writeRead (s : ISerializer) (x : 'T) =
            use m = new MemoryStream()
            s.Serialize(m, x)
            m.Position <- 0L
            s.Deserialize m :?> 'T

        let loop (s : ISerializer) iters (x : 'T) =
            use m = new MemoryStream()

            for i = 1 to iters do
                m.Position <- 0L
                s.Serialize(m, x)
                m.Position <- 0L
                let _ = s.Deserialize m :?> 'T
                ()