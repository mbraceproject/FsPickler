namespace FsCoreSerializer.Tests

    module Main =

        open System

        open FsCoreSerializer

        [<EntryPoint>]
        let main (args : string []) =
            do TestTypes.registerCustomSerializers ()
            
            let port = if args.Length = 0 then None else Some (Int32.Parse args.[0])
            let server = new SerializationServer(?port = port, logF = Console.WriteLine)
            server.Start()
            do System.Console.Title <- "FsCoreSerializer Unit tester"
            printfn "Deserialization server now running at %O" server.IPEndPoint

            let rec mainLoop () =
                do System.Threading.Thread.Sleep 1000
                mainLoop ()

            mainLoop ()