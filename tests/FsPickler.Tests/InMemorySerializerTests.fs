namespace MBrace.FsPickler.Tests

open System
open NUnit.Framework
open MBrace.FsPickler
open MBrace.FsPickler.Json

/// In-memory Serialization fixture
type LocalFixture<'Serializer when 'Serializer :> FsPicklerSerializer>(factory : unit -> 'Serializer) =
    let instance = factory()

    member __.Serializer = instance

    interface ISerializerFixture with
        member __.IsRemotedFixture = false
        member __.NewSerializer() = factory() :> FsPicklerSerializer
        member __.Serializer = instance :> FsPicklerSerializer
        member __.Pickle value = instance.Pickle value
        member __.UnPickle pickle = instance.UnPickle pickle
        member __.PickleF pickler = pickler instance

[<TestFixture>]
type ``Binary Serialization Tests`` () =
    inherit SerializationTests(LocalFixture(fun () -> FsPickler.CreateBinarySerializer()))

[<TestFixture>]
type ``XML Serialization Tests`` () =
    inherit SerializationTests(LocalFixture(fun () -> FsPickler.CreateXmlSerializer()))

[<TestFixture>]
type ``JSON Serialization Tests`` () =
    inherit SerializationTests(LocalFixture(fun () -> FsPickler.CreateJsonSerializer()))