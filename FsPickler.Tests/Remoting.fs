namespace FsPickler.Tests

    open System
    open System.Diagnostics
    open System.Net
    open System.Net.Sockets
    open System.IO
    open System.Threading
    open System.Threading.Tasks

    open FsPickler

    open NUnit.Framework

    module ServerDefaults =

        let ipAddr = "127.0.0.1"
        let port = 2323

        let defaultProtocolSerializer () = new TestBinaryFormatter() :> ISerializer
    
    exception SerializationError of exn
    exception ProtocolError of exn    

    type Request = Serialize of Type * obj
    type Reply = Success of byte [] | Error of exn

    type State = Init | Started | Stopped
        
    type SerializationServer(testedSerializer : ISerializer, ?ipAddr : string, ?port : int,
                                            ?protocolSerializer : ISerializer, ?logF : string -> unit) =

        let ipAddr = defaultArg ipAddr ServerDefaults.ipAddr |> IPAddress.Parse
        let port = defaultArg port ServerDefaults.port
        let protocolSerializer = defaultArg' protocolSerializer ServerDefaults.defaultProtocolSerializer
        let logF = defaultArg logF ignore
        let listener = new TcpListener(ipAddr, port)

        let testSerializer (Serialize (t,o)) =
            try 
                let result = Success <| Serializer.write testedSerializer o
                sprintf "Successfully serialized %A : %s" o t.Name |> logF
                result
            with e -> 
                sprintf "Failed to serialize %A : %s with error:\n %O" o t.Name e |> logF
                Error (SerializationError e)

        let loop () =
            async {
                while true do
                    try
                        use! client = listener.AcceptTcpClientAsync()
                        let (_ : TcpClient) = client
                        use stream = client.GetStream()

                        try
                            let! (bytes : byte []) = stream.AsyncReadBytes()

                            let msg = Serializer.read protocolSerializer bytes
                            let result = testSerializer msg

                            do! stream.AsyncWriteBytes <| Serializer.write protocolSerializer result
                        with e ->
                            logF <| sprintf "Protocol error: %O" e
                            do! stream.AsyncWriteBytes <| Serializer.write protocolSerializer (Error (ProtocolError e))
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


    type SerializationClient(testedSerializer : ISerializer, ?ipAddr : string, ?port : int, ?protocolSerializer : ISerializer) =
        let ipAddr = defaultArg ipAddr ServerDefaults.ipAddr
        let port = defaultArg port ServerDefaults.port
        let protocolSerializer = defaultArg' protocolSerializer ServerDefaults.defaultProtocolSerializer

        let sendSerializationRequest (msg : Request) =
            async {
                try
                    use client = new TcpClient(ipAddr, port)
                    use stream = client.GetStream()

                    let bytes = Serializer.write protocolSerializer msg
                    do! stream.AsyncWriteBytes bytes
                    let! (reply : byte []) = stream.AsyncReadBytes()

                    return Serializer.read protocolSerializer reply
                with e ->
                    return Error (ProtocolError e)
            } |> Async.RunSynchronously


        member __.Test(x : 'T) : 'T =
            match sendSerializationRequest(Serialize(typeof<'T>, x)) with
            | Success bytes -> 
                let o = Serializer.read testedSerializer bytes : obj
                o :?> 'T
            | Error(SerializationError e) -> raise e
            | Error e -> raise e

        member __.EndPoint = new IPEndPoint(IPAddress.Parse ipAddr, port)
        member __.Serializer = testedSerializer

    type ServerManager(testedSerializer : ISerializer, ?port : int) =
        let port = defaultArg port ServerDefaults.port
        let mutable proc = None : Process option

        let isActive () = 
            match proc with
            | Some p when not p.HasExited -> true
            | _ -> false
        
        member __.Start() =
            if isActive () then failwith "server already running"

            let thisExe = System.IO.Path.GetFullPath("FsPickler.Tests.exe")

            let psi = new ProcessStartInfo()

            if runsOnMono && System.IO.File.Exists "/usr/bin/env" then
                // http://www.youtube.com/watch?v=dFUlAQZB9Ng
                psi.FileName <- "/usr/bin/env"
                psi.Arguments <- sprintf "xterm -e /usr/bin/env mono \"%s\"" thisExe
            else
                psi.FileName <- thisExe
                
            psi.WorkingDirectory <- Path.GetDirectoryName thisExe

            let p = Process.Start psi

            proc <- Some p

        member __.GetClient() =
            if isActive() then new SerializationClient(testedSerializer, port = port)
            else
                failwith "server is not running"

        member __.Stop () =
            if isActive() then 
                proc.Value.Kill()
                proc <- None
            else
                failwith "server is not running"