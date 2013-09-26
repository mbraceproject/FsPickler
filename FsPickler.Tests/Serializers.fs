namespace FsPickler.Tests
    
    open System
    open System.IO
    open System.Runtime.Serialization
    open System.Runtime.Serialization.Formatters.Binary

    open FsPickler

    type ISerializer =
        abstract Name : string
        abstract Serialize : Stream * 'T -> unit
        abstract Deserialize : Stream -> 'T

    type TestFsPickler (?registry : FormatterRegistry) =
        let fsc = match registry with None -> new FsPickler() | Some r -> new FsPickler(r)

        member __.FSCS = fsc

        interface ISerializer with
            member __.Name = "FsPickler"
            member __.Serialize(stream : Stream, x : 'T) = fsc.Serialize(stream, x)
            member __.Deserialize(stream : Stream) = fsc.Deserialize<'T> stream

    type TestBinaryFormatter () =
        let bfs = new BinaryFormatter()

        interface ISerializer with
            member __.Name = "BinaryFormatter"
            member __.Serialize(stream : Stream, x : 'T) = bfs.Serialize(stream, x)
            member __.Deserialize(stream : Stream) = bfs.Deserialize stream :?> 'T

    type TestNetDataContractSerializer () =
        let ndc = new NetDataContractSerializer()

        interface ISerializer with
            member __.Name = "NetDataContractSerializer"
            member __.Serialize(stream : Stream, x : 'T) = ndc.Serialize(stream, x)
            member __.Deserialize(stream : Stream) = ndc.Deserialize stream :?> 'T


    module Serializer =
        
        let write (s : ISerializer) (x : 'T) =
            use m = new MemoryStream()
            s.Serialize(m, x)
            m.ToArray()

        let read (s : ISerializer) (bytes : byte []) =
            use m = new MemoryStream(bytes)
            s.Deserialize m : 'T

        let writeRead (s : ISerializer) (x : 'T) =
            use m = new MemoryStream()
            s.Serialize(m, x)
            m.Position <- 0L
            s.Deserialize<'T> m

        let loop (s : ISerializer) iters (x : 'T) =
            use m = new MemoryStream()

            for i = 1 to iters do
                m.Position <- 0L
                s.Serialize(m, x)
                m.Position <- 0L
                let _ = s.Deserialize<'T> m
                ()