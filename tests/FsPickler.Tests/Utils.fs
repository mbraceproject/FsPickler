namespace Nessos.FsPickler.Tests

    open System
    open System.Reflection
    open System.Runtime.Serialization.Formatters.Binary

    open NUnit.Framework

    [<AutoOpen>]
    module Utils =

        let runsOnMono = System.Type.GetType("Mono.Runtime") <> null

        let allFlags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static

        let shouldFailwith<'Exception when 'Exception :> exn>(f : unit -> unit) =
            let result =
                try f () ; Choice1Of3 ()
                with
                | :? 'Exception -> Choice2Of3 ()
                | e -> Choice3Of3 e

            match result with
            | Choice1Of3 () ->
                let msg = sprintf "Expected exception '%s', but was successful." typeof<'Exception>.Name
                raise <| new AssertionException(msg)
            | Choice2Of3 () -> ()
            | Choice3Of3 e ->
                let msg = sprintf "An unexpected exception type was thrown\nExpected: '%s'\n but was: '%s'." (e.GetType().Name) typeof<'Exception>.Name
                raise <| new AssertionException(msg)

        let private bf = new BinaryFormatter()
        let cloneObject(t : 'T) =
            use m = new System.IO.MemoryStream()
            bf.Serialize(m, t)
            m.Position <- 0L
            bf.Deserialize(m) :?> 'T