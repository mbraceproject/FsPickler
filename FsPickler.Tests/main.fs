namespace Nessos.FsPickler.Tests

    module Main =

        open System

        open Nessos.FsPickler

        [<EntryPoint>]
        let main (args : string []) =
//            let port = if args.Length = 0 then None else Some (Int32.Parse args.[0])
            let server = new SerializationServer(FsPicklerSerializer.Create args.[0], logF = Console.WriteLine)
            server.Start()
            do System.Console.Title <- "FsPickler Unit tester"
            printfn "Serialization server now running at %O" server.IPEndPoint

            let rec mainLoop () =
                do System.Threading.Thread.Sleep 1000
                mainLoop ()

            mainLoop ()