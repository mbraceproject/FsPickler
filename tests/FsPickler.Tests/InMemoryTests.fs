namespace MBrace.FsPickler.Tests

open System
open System.IO

open NUnit.Framework

open MBrace.FsPickler

type ``InMemory Tests`` (pickleFormat : string) =
    inherit ``FsPickler Serializer Tests`` (pickleFormat)

    override __.IsRemotedTest = false
    override __.Pickle (value : 'T) = __.PicklerManager.Serializer.Pickle value
    override __.PickleF (pickleF : FsPicklerSerializer -> byte []) = pickleF __.PicklerManager.Serializer