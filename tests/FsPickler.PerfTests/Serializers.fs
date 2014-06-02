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

    type FsPicklerSerializer (name : string, fsp : FsPickler) =
        member __.Pickler = fsp

        interface ISerializer with
            member __.Name = name
            member __.Serialize(stream : Stream, x : 'T) = fsp.Serialize(stream, x)
            member __.Deserialize(stream : Stream) = fsp.Deserialize<'T> stream

        static member Activate (name : string) =
            match name with
            | "FsPickler.Binary" -> new FsPicklerBinary () :> FsPicklerSerializer
            | "FsPickler.BclBinary" -> new FsPicklerBclBinary () :> FsPicklerSerializer
            | "FsPickler.Json" -> new FsPicklerJson () :> FsPicklerSerializer
            | "FsPickler.Xml" -> new FsPicklerXml () :> FsPicklerSerializer
            | _ -> invalidArg "name" <| sprintf "'%s' is not a valid pickler format." name

    and FsPicklerBinary () = inherit FsPicklerSerializer("FsPickler.Binary", FsPickler.CreateBinary())
    and FsPicklerBclBinary () = inherit FsPicklerSerializer("FsPickler.BclBinary", new BinaryPickler(new BclBinaryPickleFormatProvider()))
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

#if CREATE_FILE
        open System
        open System.IO

        let desktop = Environment.GetFolderPath Environment.SpecialFolder.Desktop
        let folder = Path.Combine(desktop, "fspickles")
        if not <| Directory.Exists(folder) then Directory.CreateDirectory folder |> ignore
        let pickleCount = ref 0
        let getFile<'T> () =
            let id = System.Threading.Interlocked.Increment pickleCount
            Path.Combine(folder, sprintf "%s-%d.txt" (typeof<'T>.FullName.Split('`').[0]) id)
#endif

        let roundtrip (x : 'T) (s : ISerializer) =
            use m = new ImmortalMemoryStream()
            s.Serialize(m, x)
#if CREATE_FILE
            m.Position <- 0L
            use fs = File.OpenWrite(getFile<'T> ())
            m.CopyTo(fs)
#endif
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