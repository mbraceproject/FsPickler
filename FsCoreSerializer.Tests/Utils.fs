namespace FsCoreSerializer.Tests

    module Utils =

        open System
        open System.IO
        open System.Net
        open System.Net.Sockets
        open System.Threading.Tasks

        let runsOnMono = System.Type.GetType("Mono.Runtime") <> null

        let runOnce (f : unit -> 'T) : unit -> 'T = let x = lazy (f ()) in fun () -> x.Value

        let defaultArg' (x : 'T option) (def : unit -> 'T) = match x with Some x -> x | None -> def ()

        type AsyncBuilder with
            member __.Bind(f : Task<'T>, g : 'T -> Async<'S>) = __.Bind(Async.AwaitTask f, g)
            member __.Bind(f : Task, g : unit -> Async<'S>) = __.Bind(f.ContinueWith ignore |> Async.AwaitTask, g)

        type Stream with
            member s.AsyncWriteBytes (bytes : byte []) =
                async {
                    do! s.WriteAsync(BitConverter.GetBytes bytes.Length, 0, 4)
                    do! s.WriteAsync(bytes, 0, bytes.Length)
                    do! s.FlushAsync()
                }

            member s.AsyncReadBytes(length : int) =
                let rec readSegment buf offset remaining =
                    async {
                        let! read = s.ReadAsync(buf, offset, remaining)
                        if read < remaining then
                            return! readSegment buf (offset + read) (remaining - read)
                        else
                            return ()
                    }

                async {
                    let bytes = Array.zeroCreate<byte> length
                    do! readSegment bytes 0 length
                    return bytes
                }

            member s.AsyncReadBytes() =
                async {
                    let! lengthArr = s.AsyncReadBytes 4
                    let length = BitConverter.ToInt32(lengthArr, 0)
                    return! s.AsyncReadBytes length
                }