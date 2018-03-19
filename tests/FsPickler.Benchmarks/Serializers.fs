namespace MBrace.FsPickler.Benchmarks

open System
open System.IO
open System.Threading

open BenchmarkDotNet.Attributes

type ISerializer =
    abstract Name : string
    abstract Serialize : Stream -> 'T -> unit
    abstract Deserialize : Stream -> 'T

type FsPicklerSerializer(serializer : MBrace.FsPickler.FsPicklerSerializer) =
    interface ISerializer with
        member __.Name = sprintf "FsPickler - %s" serializer.PickleFormat
        member __.Serialize stream value = serializer.Serialize(stream, value)
        member __.Deserialize stream = serializer.Deserialize(stream)

type FsPicklerBinarySerializer() =
    inherit FsPicklerSerializer(MBrace.FsPickler.FsPickler.CreateBinarySerializer())

type FsPicklerXmlSerializer() =
    inherit FsPicklerSerializer(MBrace.FsPickler.FsPickler.CreateXmlSerializer())

type FsPicklerJsonSerializer() =
    inherit FsPicklerSerializer(MBrace.FsPickler.Json.FsPickler.CreateJsonSerializer())

type JsonDotNetSerializer () =
    let jdn = Newtonsoft.Json.JsonSerializer.Create()
        
    interface ISerializer with
        member __.Name = "Json.Net"
        member __.Serialize (stream : Stream) (x : 'T) =
            use writer = new StreamWriter(stream)
            jdn.Serialize(writer, x)
            writer.Flush()

        member __.Deserialize (stream : Stream) : 'T =
            use reader = new StreamReader(stream)
            jdn.Deserialize(reader, typeof<'T>) :?> 'T

type JsonDotNetBsonSerializer () =
    let jdn = Newtonsoft.Json.JsonSerializer.Create()
        
    interface ISerializer with
        override __.Name = "Json.Net (Bson)"
        override __.Serialize(stream : Stream) (x : 'T) =
            use writer = new Newtonsoft.Json.Bson.BsonWriter(stream)
            jdn.Serialize(writer, x)
            writer.Flush()

        override __.Deserialize(stream : Stream) : 'T =
            use reader = new Newtonsoft.Json.Bson.BsonReader(stream)
            jdn.Deserialize(reader, typeof<'T>) :?> 'T

type ProtoBufSerializer () =
    interface ISerializer with
        override __.Name = "ProtoBuf-Net"
        override __.Serialize(stream : Stream) (x : 'T) = ProtoBuf.Serializer.Serialize(stream, x)
        override __.Deserialize(stream : Stream) = ProtoBuf.Serializer.Deserialize<'T>(stream)

type WireSerializer () =
    let wire = new Wire.Serializer()

    interface ISerializer with
        override __.Name = "Wire"
        override __.Serialize(stream : Stream) (x : 'T) = wire.Serialize(x, stream)
        override __.Deserialize(stream : Stream) = wire.Deserialize<'T>(stream)

module Serializer =
    
    type ImmortalMemoryStream() =
        inherit MemoryStream()
        override __.Close() = ()

    let memoryStream = 
        new ThreadLocal<_>(fun () -> 
            new ImmortalMemoryStream() :> MemoryStream)

    let roundTrip (serializer : ISerializer) (value : 'T) =
        let m = memoryStream.Value
        m.Position <- 0L
        serializer.Serialize m value
        m.Position <- 0L
        let _ = serializer.Deserialize<'T> m
        ()


[<AbstractClass>]
type RoundtripBenchmark<'T>(value : 'T) =
    let fsb = new FsPicklerBinarySerializer()
    let fsx = new FsPicklerXmlSerializer()
    let fsj = new FsPicklerJsonSerializer()
    let nsj = new JsonDotNetSerializer()
    let nsb = new JsonDotNetBsonSerializer()
    let pbf = new ProtoBufSerializer()
    let wir = new WireSerializer()

    [<Benchmark(Description = "FsPickler.Binary")>]
    member __.FsPicklerBinary() = Serializer.roundTrip fsb value
    [<Benchmark(Description = "FsPickler.Xml")>]
    member __.FsPicklerXml() = Serializer.roundTrip fsx value
    [<Benchmark(Description = "FsPickler.Json")>]
    member __.FsPicklerJson() = Serializer.roundTrip fsj value
    [<Benchmark(Description = "Newtonsoft.Json")>]
    member __.NewtonsoftJson() = Serializer.roundTrip nsj value
    [<Benchmark(Description = "Newtonsoft.Bson")>]
    member __.NewtonsoftBson() = Serializer.roundTrip nsb value
    [<Benchmark(Description = "Protobuf-Net")>]
    member __.Protobuf() = Serializer.roundTrip pbf value
    [<Benchmark(Description = "Wire")>]
    member __.Wire() = Serializer.roundTrip wir value