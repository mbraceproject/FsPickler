namespace FsCoreSerializer

    #nowarn "1204"

    module internal Utils =
        
        open System
        open System.Collections.Generic
        open System.IO
        open System.Threading
        open System.Text
        open System.Runtime.Serialization

        let runsOnMono = System.Type.GetType("Mono.Runtime") <> null

        type Atom<'T when 'T : not struct>(value : 'T) =
            let refCell = ref value
    
            let rec swap f = 
                let currentValue = !refCell
                let result = Interlocked.CompareExchange<'T>(refCell, f currentValue, currentValue)
                if obj.ReferenceEquals(result, currentValue) then ()
                else Thread.SpinWait 20; swap f

            let transact f =
                let output = ref Unchecked.defaultof<'S>
                let f' x = let t,s = f x in output := s ; t
                swap f' ; !output
        
            member __.Value with get() : 'T = !refCell
            member __.Swap (f : 'T -> 'T) : unit = swap f
            member __.Set (v : 'T) : unit = swap (fun _ -> v)
            member __.Transact (f : 'T -> 'T * 'S) : 'S = transact f  

        [<RequireQualifiedAccess>]
        module Atom =
            let atom x = Atom<_>(x)
            let get (a : Atom<_>) = a.Value
            let swap (f : 'T -> 'T) (a : Atom<_>) = a.Swap f
            let transact (f : 'T -> 'T * 'R) (a : Atom<_>) = a.Transact f


        type IDictionary<'K,'V> with
            member d.TryFind (k : 'K) =
                let found, v = d.TryGetValue k
                if found then Some v else None

        type Map<'K,'V when 'K : comparison> with
            member m.AddNoOverwrite(key : 'K, value : 'V) =
                if m.ContainsKey key then invalidArg "key" "An item with the same key has already been added."
                else
                    m.Add(key, value)

        let inline denull x = if x = null then None else Some x

        let inline fastUnbox<'T> (x : obj) = 
            Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.UnboxFast<'T> x

        let (|InnerExn|_|) (e : #exn) = denull e.InnerException

        let inline mkUntypedWriter (f : 'a -> 'b -> 'c) = fun x (y:obj) -> f x (y :?> _)
        let inline mkUntypedReader (f : 'a -> 'b) = fun x -> f x :> obj
        let inline mkTypedWriter (f : 'a -> obj -> 'c) = fun x (y : 'b) -> f x (y :> obj)
        let inline mkTypedReader (f : 'a -> obj) = fun x -> f x :?> 'b


//        // produces a structural hashcode out of a byte array
//        let getByteHashCode (bs : byte []) =
//            let n = bs.Length
//            let mutable i = 0
//            let mutable acc = 0
//
//            let inline bytes2Int x y z w = 
//                int x + (int y <<< 8) + (int z <<< 16) + (int w <<< 24)
//
//            while i + 4 <= n do
//                acc <- acc ^^^ bytes2Int bs.[i] bs.[i + 1] bs.[i + 2] bs.[i + 3]
//                i <- i + 4
//
//            match n - i with
//            | 0 -> ()
//            | 1 -> acc <- acc ^^^ bytes2Int bs.[i] 0uy 0uy 0uy
//            | 2 -> acc <- acc ^^^ bytes2Int bs.[i] bs.[i+1] 0uy 0uy
//            | _ -> acc <- acc ^^^ bytes2Int bs.[i] bs.[i+1] bs.[i+2] 0uy
//
//            acc

        [<RequireQualifiedAccess>]
        module Stream =

            let internal bufferSize = 256    
            let internal buffer = new ThreadLocal<byte []>(fun () -> Array.zeroCreate<byte> bufferSize)

            let inline WriteInt (stream : Stream) (n : int) =
                let buf = buffer.Value
                buf.[0] <- byte n
                buf.[1] <- byte (n >>> 8)
                buf.[2] <- byte (n >>> 16)
                buf.[3] <- byte (n >>> 24)
                stream.Write(buf, 0, 4)

            let inline ReadInt (stream : Stream) = 
                let buf = buffer.Value
                if stream.Read(buf, 0, 4) < 4 then
                    raise <| new EndOfStreamException()

                (int buf.[0])
                    ||| (int buf.[1] <<< 8)
                    ||| (int buf.[2] <<< 16)
                    ||| (int buf.[3] <<< 24)
                

            /// block copy primitive array to stream
            let WriteArray (stream : Stream, array : Array) =
                do stream.Flush()

                let buf = buffer.Value
                let totalBytes = Buffer.ByteLength array

                let d = totalBytes / bufferSize
                let r = totalBytes % bufferSize

                for i = 0 to d - 1 do
                    Buffer.BlockCopy(array, i * bufferSize, buf, 0, bufferSize)
                    stream.Write(buf, 0, bufferSize)

                if r > 0 then
                    Buffer.BlockCopy(array, d * bufferSize, buf, 0, r)
                    stream.Write(buf, 0, r)

            /// copy stream contents to preallocated array
            let CopyToArray (stream : Stream, array : Array) =
                let buf = buffer.Value
                let inline readBytes (n : int) =
                    if stream.Read(buf, 0, n) < n then
                        // TODO: this is wrong
                        raise <| new EndOfStreamException()
        
                let totalBytes = Buffer.ByteLength array

                let d = totalBytes / bufferSize
                let r = totalBytes % bufferSize

                for i = 0 to d - 1 do
                    do readBytes bufferSize
                    Buffer.BlockCopy(buf, 0, array, i * bufferSize, bufferSize)

                if r > 0 then
                    do readBytes r
                    Buffer.BlockCopy(buf, 0, array, d * bufferSize, r)