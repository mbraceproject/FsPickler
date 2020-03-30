namespace MBrace.FsPickler.Tests

open System
open NUnit.Framework
open MBrace.FsPickler

#if !NETCOREAPP
/// Fixture that uses .NET remoting to run serialization and deserialization in distinct AppDomains
type RemotingFixture<'Serializer when 'Serializer :> FsPicklerSerializer>(factory : unit -> 'Serializer) =
    let localInstance = factory()
    // remote domain initialization
    let remoteDomain = AppDomain.Create()
    let remoteProxy = remoteDomain.Activate<RemoteSerializeProxy<'Serializer>>()
    do remoteProxy.InstallInstance(factory)

    interface ISerializerFixture with
        member __.IsRemotedFixture = true
        member __.Serializer = localInstance :> FsPicklerSerializer
        member __.NewSerializer() = factory() :> FsPicklerSerializer
        member __.Pickle value =
            match FailoverPickler.TryPickle value with
            | Some p -> remoteProxy.PickleValue p
            | None ->
                // fall back to in-domain initialization 
                // if remoting cannot support marshalling of the value
                localInstance.Pickle value

        member __.UnPickle pickle = localInstance.UnPickle pickle
        member __.PickleF (pickler : FsPicklerSerializer -> byte[]) = 
            match FailoverPickler.TryPickle pickler with
            | Some p -> remoteProxy.PickleFunc p
            | None ->
                // fall back to in-domain initialization 
                // if remoting cannot support marshalling of the value
                pickler localInstance

and RemoteSerializeProxy<'Serializer when 'Serializer :> FsPicklerSerializer>() =
    inherit MarshalByRefObject()
    let mutable instance = None
    let getInstance() = Option.get instance

    override __.InitializeLifetimeService () = null

    member __.InstallInstance(factory : unit -> 'Serializer) : unit =
        instance <- Some(factory())

    member __.PickleValue<'T> (pickle : FailoverPickle<'T>) : byte[] =
        let value = FailoverPickler.UnPickle pickle
        getInstance().Pickle value

    member __.PickleFunc (pickle : FailoverPickle<'Serializer -> byte[]>) : byte[] =
        let pickler = FailoverPickler.UnPickle pickle
        getInstance() |> pickler
#endif

#if DEBUG && !NETCOREAPP
[<TestFixture>]
type ``Remote Serialization Tests`` () =
    inherit SerializationTests(RemotingFixture(fun () -> FsPickler.CreateBinarySerializer()))
#endif