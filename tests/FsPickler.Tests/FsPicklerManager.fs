namespace Nessos.FsPickler.Tests

open System

open Nessos.FsPickler
open Nessos.FsPickler.Json

[<RequireQualifiedAccess>]
module PickleFormat =
    [<Literal>]
    let Binary = "FsPickler.Binary"

    [<Literal>]
    let Xml = "FsPickler.Xml"

    [<Literal>]
    let Json = "FsPickler.Json"

    [<Literal>]
    let Json_Headerless = "FsPickler.Json-headerless"

    [<Literal>]
    let Bson = "FsPickler.Bson"

type FsPicklerManager(pickleFormat : string) =

    let mkSerializer () =
        match pickleFormat with
        | PickleFormat.Binary -> FsPickler.CreateBinarySerializer() :> FsPicklerSerializer
        | PickleFormat.Xml -> FsPickler.CreateXmlSerializer(indent = true) :> FsPicklerSerializer
        | PickleFormat.Json -> FsPickler.CreateJsonSerializer(indent = true) :> FsPicklerSerializer
        | PickleFormat.Json_Headerless -> 
            let jsp = FsPickler.CreateJsonSerializer(omitHeader = true)
            jsp.UseCustomTopLevelSequenceSeparator <- true
            jsp.SequenceSeparator <- System.Environment.NewLine
            jsp :> FsPicklerSerializer

        | PickleFormat.Bson -> FsPickler.CreateBsonSerializer() :> FsPicklerSerializer

        | _ -> invalidArg "name" <| sprintf "unexpected pickler format '%s'." pickleFormat

    let serializer = mkSerializer()

    member __.Serializer = serializer
    member __.CreateSerializer() = mkSerializer()
    member __.GetRemoteSerializer() = new RemoteSerializationClient(pickleFormat)

    interface ISerializer with
        member __.Name = pickleFormat
        member __.Pickle (value : 'T) = serializer.Pickle(value)
        member __.UnPickle<'T> (data : byte[]) = serializer.UnPickle<'T>(data)



and RemoteSerializer (pickleFormat : string) =
    inherit MarshalByRefObject()

    let mgr = new FsPicklerManager(pickleFormat)
    let fp = FailoverSerializer.Create()

    member __.Pickle<'T>(data : byte []) : byte [] =
        let value = fp.UnPickle<'T>(data)
        mgr.Serializer.Pickle<'T>(value)

    member __.PickleF(data : byte []) : byte [] =
        let serializer = fp.UnPickle<FsPicklerSerializer -> byte[]>(data)
        serializer mgr.Serializer

and RemoteSerializationClient (pickleFormat : string) =
    let fp = FailoverSerializer.Create()
    let remote = AppDomainManager.Activate<RemoteSerializer>("remoteSerializationDomain", [| pickleFormat :> obj |])

    member __.Pickle<'T> (value : 'T) =
        let data = fp.Pickle<'T> value
        remote.Pickle<'T> data

    member __.PickleF (pickleF : FsPicklerSerializer -> byte []) =
        let data = fp.Pickle pickleF
        remote.PickleF data