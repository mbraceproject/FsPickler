namespace MBrace.FsPickler.Tests

open System
open System.IO

open NUnit.Framework

open MBrace.FsPickler

#if !NETCOREAPP2_0

[<TestFixture(PickleFormat.Binary)>]
type ``AppDomain Tests`` (pickleFormat : string) as self =
    inherit ``FsPickler Serializer Tests`` (pickleFormat)

    let remotePickler = lazy(self.PicklerManager.GetRemoteSerializer())

    override __.IsRemotedTest = true
    override __.Pickle (value : 'T) = 
        try remotePickler.Value.Pickle<'T> value
        with :? FailoverSerializerException -> self.PicklerManager.Serializer.Pickle value

    override __.PickleF (pickleF : FsPicklerSerializer -> byte []) = 
        try remotePickler.Value.PickleF pickleF
        with :? FailoverSerializerException -> pickleF self.PicklerManager.Serializer
#endif