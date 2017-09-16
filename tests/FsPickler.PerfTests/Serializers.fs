namespace MBrace.FsPickler.Tests
    
open System
open System.IO
open System.Runtime.Serialization
open System.Runtime.Serialization.Formatters.Binary

open Newtonsoft.Json
open Newtonsoft.Json.Bson
open ProtoBuf
open Wire

open MBrace.FsPickler
open MBrace.FsPickler.Json

open PerfUtil

/// A MemoryStream that never closes
[<AllowNullLiteral>]
type ImmortalMemoryStream() =
    inherit MemoryStream()
    override __.Close() = ()

/// A Serializer implementation abstraction for performance tests

[<AbstractClass>]
type Serializer () =
    // use the same stream for every repeat of a given test
    // this is to factor out the performance overhead introduced
    // by MemoryStream
    let mutable m : ImmortalMemoryStream = null

    member s.TestRoundTrip (t : 'T) =
        m.Position <- 0L
        s.Serialize (m, t)
        m.Position <- 0L
        s.Deserialize<'T> m

    abstract Name : string
    abstract Serialize : Stream * 'T -> unit
    abstract Deserialize : Stream -> 'T

    interface ITestable with
        member __.Name = __.Name
        member __.Init () = m <- new ImmortalMemoryStream()
        member __.Fini () = m <- null


module FsPickler =
    let wrap (fsp : FsPicklerSerializer) =
        {
            new Serializer() with
                member __.Name = sprintf "FsPickler.%s" fsp.PickleFormat
                member __.Serialize(stream : Stream, x : 'T) = fsp.Serialize(stream, x)
                member __.Deserialize<'T>(stream : Stream) = fsp.Deserialize<'T> stream
        }

    let initBinary() = wrap <| FsPickler.CreateBinarySerializer()
    let initXml() = wrap <| FsPickler.CreateXmlSerializer()
    let initJson() = wrap <| FsPickler.CreateJsonSerializer()
    let initBson() = wrap <| FsPickler.CreateBsonSerializer()

type BinaryFormatterSerializer () =
    inherit Serializer ()
    let bfs = new BinaryFormatter()

    override __.Name = "BinaryFormatter"
    override __.Serialize(stream : Stream, x : 'T) = bfs.Serialize(stream, x)
    override __.Deserialize(stream : Stream) = bfs.Deserialize stream :?> 'T

type NetDataContractSerializer () =
    inherit Serializer ()
    let ndc = new System.Runtime.Serialization.NetDataContractSerializer()

    override __.Name = "NetDataContractSerializer"
    override __.Serialize(stream : Stream, x : 'T) = ndc.Serialize(stream, x)
    override __.Deserialize(stream : Stream) = ndc.Deserialize stream :?> 'T

type JsonDotNetSerializer () =
    inherit Serializer ()
    let jdn = Newtonsoft.Json.JsonSerializer.Create()
        
    override __.Name = "Json.Net"
    override __.Serialize(stream : Stream, x : 'T) =
        use writer = new System.IO.StreamWriter(stream)
        jdn.Serialize(writer, x)
        writer.Flush()
    override __.Deserialize(stream : Stream) : 'T =
        use reader = new System.IO.StreamReader(stream)
        jdn.Deserialize(reader, typeof<'T>) :?> 'T

type JsonDotNetBsonSerializer () =
    inherit Serializer()
    let jdn = Newtonsoft.Json.JsonSerializer.Create()
        
    override __.Name = "Json.Net (Bson)"
    override __.Serialize(stream : Stream, x : 'T) =
        use writer = new BsonWriter(stream)
        jdn.Serialize(writer, x)
        writer.Flush()

    override __.Deserialize(stream : Stream) : 'T =
        use reader = new BsonReader(stream)
        jdn.Deserialize(reader, typeof<'T>) :?> 'T

type ProtoBufSerializer () =
    inherit Serializer ()
        
    override __.Name = "ProtoBuf-Net"
    override __.Serialize(stream : Stream, x : 'T) = ProtoBuf.Serializer.Serialize(stream, x)
    override __.Deserialize(stream : Stream) = ProtoBuf.Serializer.Deserialize<'T>(stream)

type WireSerializer () =
    inherit Serializer ()
    let wire = new Wire.Serializer()

    override __.Name = "Wire"
    override __.Serialize(stream : Stream, x : 'T) = wire.Serialize(x, stream)
    override __.Deserialize(stream : Stream) = wire.Deserialize<'T>(stream)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Serializer =
        
    let inline write (s : Serializer) (x : 'T) =
        use m = new MemoryStream()
        s.Serialize(m, x)
        m.ToArray()

    let inline read (s : Serializer) (bytes : byte []) =
        use m = new MemoryStream(bytes)
        s.Deserialize m : 'T

    let inline roundtrip (x : 'T) (s : Serializer) = s.TestRoundTrip x