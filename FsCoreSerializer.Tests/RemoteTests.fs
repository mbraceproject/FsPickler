namespace FsCoreSerializer.Tests

    open System
    open System.Diagnostics
    open System.Net
    open System.Net.Sockets
    open System.IO
    open System.Threading
    open System.Threading.Tasks

    open FsCoreSerializer
    open FsCoreSerializer.Tests.Utils

    open NUnit.Framework

    module ServerDefaults =

        let ipAddr = "127.0.0.1"
        let port = 2323

        let defaultProtocolSerializer () =
            // NetDataContractSerializer not supported in mono
            if runsOnMono then new BinaryFormatterSerializer() :> ISerializer
            else new NDCSerializer() :> ISerializer
        let defaultTestedSerializer () = new FsCoreSerializer () :> ISerializer

        

    type Request = Serialize of Type * obj
    type Reply = Success of byte [] | Error of exn

    type State = Init | Started | Stopped
        
    type SerializationServer(?ipAddr : string, ?port : int, ?testedSerializer : ISerializer, 
                                            ?protocolSerializer : ISerializer, ?logF : string -> unit) =

        let ipAddr = defaultArg ipAddr ServerDefaults.ipAddr |> IPAddress.Parse
        let port = defaultArg port ServerDefaults.port
        let testedSerializer = defaultArg' testedSerializer ServerDefaults.defaultTestedSerializer
        let protocolSerializer = defaultArg' protocolSerializer ServerDefaults.defaultProtocolSerializer
        let logF = defaultArg logF ignore
        let listener = new TcpListener(ipAddr, port)

        let testSerializer (Serialize (_,o)) =
            try Success (testedSerializer.Serialize o)
            with e -> Error e

        let report (Serialize (t,o)) =
            function
            | Success _ -> sprintf "Successfully serialized %A : %s" o t.Name |> logF
            | Error e -> sprintf "Failed to serialize %A : %s with error:\n %O" o t.Name e |> logF

        let loop () =
            async {
                while true do
                    try
                        use! client = listener.AcceptTcpClientAsync()
                        let (_ : TcpClient) = client
                        use stream = client.GetStream()

                        try
                            let! (bytes : byte []) = stream.AsyncReadBytes()

                            let msg = protocolSerializer.Deserialize bytes :?> Request
                            let result = testSerializer msg
                            do report msg result

                            do! stream.AsyncWriteBytes (protocolSerializer.Serialize result)
                        with e ->
                            do! stream.AsyncWriteBytes (protocolSerializer.Serialize (Error e))
                    with e ->
                        logF <| sprintf "Protocol error: %O" e
            }

        let cts = new CancellationTokenSource()
        let mutable state = Init

        member __.IPEndPoint = new IPEndPoint(ipAddr, port)

        member __.Start() =
            lock state (fun () ->
                match state with
                | Started -> failwith "server is already running."
                | Stopped -> failwith "server has been disposed."
                | Init ->
                    listener.Start()
                    Async.Start(loop(), cts.Token)
                    state <- Started)

        member __.Stop() =
            lock state (fun () ->
                match state with
                | Init -> failwith "server has not been started."
                | Stopped -> failwith "server has been stopped."
                | Started -> cts.Cancel() ; listener.Stop() ; state <- Stopped)

        interface IDisposable with
            member __.Dispose() = 
                lock state (fun () -> cts.Cancel() ; listener.Stop() ; state <- Stopped)


    type SerializationClient(?ipAddr : string, ?port : int, ?testedSerializer : ISerializer, ?protocolSerializer : ISerializer) =
        let ipAddr = defaultArg ipAddr ServerDefaults.ipAddr
        let port = defaultArg port ServerDefaults.port
        let protocolSerializer = defaultArg' protocolSerializer ServerDefaults.defaultProtocolSerializer
        let testedSerializer = defaultArg' testedSerializer ServerDefaults.defaultTestedSerializer

        let sendSerializationRequest (msg : Request) =
            async {
                use client = new TcpClient(ipAddr, port)
                use stream = client.GetStream()

                let bytes = protocolSerializer.Serialize msg
                do! stream.AsyncWriteBytes bytes
                let! (reply : byte []) = stream.AsyncReadBytes()

                return
                    match protocolSerializer.Deserialize reply :?> Reply with
                    | Success bytes -> bytes
                    | Error e -> raise e
            } |> Async.RunSynchronously


        member __.Test(x : 'T) =
            let bytes = sendSerializationRequest(Serialize(typeof<'T>, x))
            testedSerializer.Deserialize bytes :?> 'T

        member __.EndPoint = new IPEndPoint(IPAddress.Parse ipAddr, port)
        member __.Serializer = testedSerializer

    type ServerManager(?port : int) =
        let port = defaultArg port ServerDefaults.port
        let mutable proc = None : Process option

        let isActive () = 
            match proc with
            | Some p when not p.HasExited -> true
            | _ -> false
        
        member __.Start() =
            if isActive () then failwith "server already running"

            let thisExe = System.Reflection.Assembly.GetExecutingAssembly().Location

            let psi = new ProcessStartInfo()

            if runsOnMono && System.IO.File.Exists "/usr/bin/env" then
                // http://www.youtube.com/watch?v=dFUlAQZB9Ng
                psi.FileName <- "/usr/bin/env"
                psi.Arguments <- sprintf "xterm -e /usr/bin/env mono \"%s\"" thisExe
            else
                psi.FileName <- thisExe
                
            psi.WorkingDirectory <- Path.GetDirectoryName thisExe

            let p = Process.Start psi
            let c = new SerializationClient(port = port)

            proc <- Some p

        member __.GetClient() =
            if isActive() then new SerializationClient(port = port)
            else
                failwith "server is not running"

        member __.Stop () =
            if isActive() then 
                proc.Value.Kill()
                proc <- None
            else
                failwith "server is not running"


    [<TestFixture>]
    type ``Remoted Corectness Tests`` () =
        inherit ``Serializer Correctness Tests`` ()

        let mutable state = None : (ServerManager * SerializationClient) option

        override __.TestSerializer(x : 'T) = 
            match state with
            | Some (_,client) -> client.Serializer.Serialize x
            | None -> failwith "remote server has not been set up."
            
        override __.TestDeserializer(bytes : byte []) =
            match state with
            | Some (_,client) -> client.Serializer.Deserialize bytes
            | None -> failwith "remote server has not been set up."

        override __.TestLoop(x : 'T) =
            match state with
            | Some (_,client) -> client.Test x
            | None -> failwith "remote server has not been set up."

        override __.Init () =
            match state with
            | Some _ -> failwith "remote server appears to be running."
            | None ->
                let mgr = new ServerManager()
                do mgr.Start()
                do System.Threading.Thread.Sleep 2000
                let client = mgr.GetClient()
                state <- Some(mgr, client)

        override __.Fini () =
            match state with
            | None -> failwith "no remote server appears to be running."
            | Some (mgr,_) -> mgr.Stop() ; state <- None