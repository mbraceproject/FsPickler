namespace MBrace.FsPickler.Tests

open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Reflection
open System.Runtime.Serialization
open System.Threading.Tasks

open MBrace.FsPickler
open MBrace.FsPickler.Hashing
open MBrace.FsPickler.Combinators
open MBrace.FsPickler.Json

open MBrace.FsPickler.Tests.TestTypes

open NUnit.Framework
open FsUnit
open FsCheck

[<TestFixture>]
module ``Interface Tests`` =
    open System.Threading
    
    type ITest =
        abstract member Int : int

    [<AutoSerializable(false)>]
    type NonSerializableWithInterface(i) =
        interface ITest with
            member x.Int = i
        member x.Int = i

    let mutable isPicklerFactoryRegistered = false
    let registerFactory() =
        if isPicklerFactoryRegistered then () else
        let factory (resolver:IPicklerResolver) =
            let intPickler = resolver.Resolve<int>()
            let reader state =
                let i = intPickler.Read state "i"
                NonSerializableWithInterface(i) :> ITest
            let writer state (value:ITest) =
                intPickler.Write state "i" value.Int
            Pickler.FromPrimitives(reader, writer, useWithSubtypes=true)
        FsPickler.RegisterPicklerFactory<ITest>(factory)
        isPicklerFactoryRegistered <- true



    [<Test; Category("Pickler tests")>]
    let ``interface pickler is used for implementation`` () =
        registerFactory()

        let serializer = FsPickler.CreateBinarySerializer()
        use ms = new MemoryStream()
        serializer.Serialize(ms, NonSerializableWithInterface(123456), leaveOpen=true)
        ms.Seek(0L, SeekOrigin.Begin) |> ignore
        let v : NonSerializableWithInterface = serializer.Deserialize(ms)
        Assert.AreEqual(123456, v.Int)
