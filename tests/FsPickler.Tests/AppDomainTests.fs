namespace Nessos.FsPickler.Tests

    open System
    open System.IO

    open NUnit.Framework

    open Nessos.FsPickler

#if REMOTE_TESTS

    type ``AppDomain Tests`` (pickleFormat : string) as self =
        inherit ``FsPickler Tests`` (pickleFormat)

        let remotePickler = self.PicklerManager.GetRemoteSerializer()

        override __.IsRemotedTest = true
        override __.Pickle (value : 'T) = 
            try remotePickler.Pickle<'T> value
            with :? FailoverPicklerException -> self.PicklerManager.Pickler.Pickle value

        override __.PickleF (pickleF : FsPicklerSerializer -> byte []) = 
            try remotePickler.PickleF pickleF
            with :? FailoverPicklerException -> pickleF self.PicklerManager.Pickler
#endif