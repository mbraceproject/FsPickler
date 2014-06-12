namespace Nessos.FsPickler.Tests
    
    open System
    open System.IO
    open System.Runtime.Serialization
    open System.Runtime.Serialization.Formatters.Binary

    open ServiceStack.Text
    open Newtonsoft.Json
    open ProtoBuf

    open Nessos.FsPickler
    open PerfUtil

    type ISerializer =
        inherit ITestable
        abstract Serialize : Stream * 'T -> unit
        abstract Deserialize : Stream -> 'T

    type FsPicklerSerializer (name : string, fsp : BasePickler) =
        member __.Pickler = fsp

        interface ISerializer with
            member __.Name = name
            member __.Serialize(stream : Stream, x : 'T) = fsp.Serialize(stream, x)
            member __.Deserialize(stream : Stream) = fsp.Deserialize<'T> stream

    and FsPicklerBinary () = inherit FsPicklerSerializer("FsPickler.Binary", FsPickler.CreateBinary())
    and FsPicklerXml () = inherit FsPicklerSerializer("FsPickler.Xml", FsPickler.CreateXml())
    and FsPicklerJson () = inherit FsPicklerSerializer("FsPickler.Json", FsPickler.CreateJson())

    type BinaryFormatterSerializer () =
        let bfs = new BinaryFormatter()

        interface ISerializer with
            member __.Name = "BinaryFormatter"
            member __.Serialize(stream : Stream, x : 'T) = bfs.Serialize(stream, x)
            member __.Deserialize(stream : Stream) = bfs.Deserialize stream :?> 'T

    type NetDataContractSerializer () =
        let ndc = new System.Runtime.Serialization.NetDataContractSerializer()

        interface ISerializer with
            member __.Name = "NetDataContractSerializer"
            member __.Serialize(stream : Stream, x : 'T) = ndc.Serialize(stream, x)
            member __.Deserialize(stream : Stream) = ndc.Deserialize stream :?> 'T

    type JsonDotNetSerializer () =
        let jdn = Newtonsoft.Json.JsonSerializer.Create()
        
        interface ISerializer with
            member __.Name = "Json.Net"
            member __.Serialize(stream : Stream, x : 'T) =
                use writer = new System.IO.StreamWriter(stream)
                jdn.Serialize(writer, x)
                writer.Flush()
            member __.Deserialize(stream : Stream) : 'T =
                use reader = new System.IO.StreamReader(stream)
                jdn.Deserialize(reader, typeof<'T>) :?> 'T

    type ServiceStackJsonSerializer () =
        
        interface ISerializer with
            member __.Name = "ServiceStack.JsonSerializer"
            member __.Serialize(stream : Stream, x : 'T) = JsonSerializer.SerializeToStream(x, stream)
            member __.Deserialize(stream : Stream) = JsonSerializer.DeserializeFromStream<'T>(stream)

    type ServiceStackTypeSerializer () =

        interface ISerializer with
            member __.Name = "ServiceStack.TypeSerializer"
            member __.Serialize(stream : Stream, x : 'T) = TypeSerializer.SerializeToStream(x, stream)
            member __.Deserialize(stream : Stream) = TypeSerializer.DeserializeFromStream<'T>(stream)


    type ProtoBufSerializer () =
        
        interface ISerializer with
            member __.Name = "ProtoBuf-Net"
            member __.Serialize(stream : Stream, x : 'T) = ProtoBuf.Serializer.Serialize(stream, x)
            member __.Deserialize(stream : Stream) = ProtoBuf.Serializer.Deserialize<'T>(stream)


    module Serializer =
        
        let write (s : ISerializer) (x : 'T) =
            use m = new MemoryStream()
            s.Serialize(m, x)
            m.ToArray()

        let read (s : ISerializer) (bytes : byte []) =
            use m = new MemoryStream(bytes)
            s.Deserialize m : 'T

        type ImmortalMemoryStream() =
            inherit MemoryStream()
            override __.Close() = ()

        let roundtrip (x : 'T) (s : ISerializer) =
            use m = new ImmortalMemoryStream()
            s.Serialize(m, x)
            m.Position <- 0L
            s.Deserialize<'T> m

        let roundtrips times (x : 'T) (s : ISerializer) =
            use m = new ImmortalMemoryStream()

            for i = 1 to times do
                m.Position <- 0L
                s.Serialize(m, x)
                m.Position <- 0L
                let _ = s.Deserialize<'T> m
                ()