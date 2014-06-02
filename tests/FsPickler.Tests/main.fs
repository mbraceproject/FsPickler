namespace Nessos.FsPickler.Tests

    module Main =

        open System

        open Nessos.FsPickler

        [<EntryPoint>]
        let main (args : string []) =
            let serializer, port =
                try
                    let serializer = args.[0]
                    let port = Int32.Parse args.[1]
                    serializer, port
                with e -> eprintfn "Parse error: %s" e.Message ; exit 1

            let server = new SerializationServer(FsPicklerSerializer.Activate serializer, port = port, logF = Console.WriteLine)
            server.Start()
            do System.Console.Title <- "FsPickler Unit tester"
            printfn "Serialization server now running at %O" server.IPEndPoint

            let rec mainLoop () =
                do System.Threading.Thread.Sleep 1000
                mainLoop ()

            mainLoop ()