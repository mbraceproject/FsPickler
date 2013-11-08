namespace FsPickler.Hashing
    
    open System
    open System.IO

    /// An immutable factory interface for HashStreams
    type IHashStreamFactory =
        abstract Create : unit -> HashStream

    /// An abstract byte sink used as a hash generating state machine
    and [<AbstractClass>] HashStream(?underlying : Stream) =
        inherit System.IO.Stream()

        static let notSupported () = raise <| new NotSupportedException()

        let underlying = defaultArg underlying null

        abstract ComputeHash : unit -> byte []

        override __.CanRead = false
        override __.CanSeek = false
        override __.CanTimeout = false
        override __.CanWrite = true
        override __.ReadTimeout = 0
        override __.WriteTimeout = 0
        override __.Seek(_,_) = notSupported ()
        override __.SetLength _ = notSupported ()
        override __.Read(_,_,_) = notSupported ()
        override __.Flush () = ()
        override __.Position with set _ = notSupported ()

    //
    // 64-bit Fowler-Noll-Vo hashing algorithm
    // adapted from http://home.comcast.net/~bretm/hash/6.html
    //

    /// 64-bit Fowler-Noll-Vo hashing algorithm
    type FNV1aStreamFactory (?bits : int) =
        interface IHashStreamFactory with
            member __.Create () = new FNV1aStream(?bits = bits) :> HashStream

    /// 64-bit Fowler-Noll-Vo hashing algorithm
    and FNV1aStream (?bits : int) =
        inherit HashStream()

        let shift = 64 - (defaultArg bits 64)
        let mask = (1UL <<< shift) - 1UL

        let mutable pos = 0L
        let mutable hash = 0xcbf29ce484222325UL

        override __.Length = pos
        override __.Position = pos

        override __.ComputeHash () =
            // perform XOR folding
            let hash =
                if shift = 0 then hash
                else
                    (hash ^^^ (hash >>> shift)) &&& mask

            BitConverter.GetBytes hash

        override __.WriteByte(b : byte) =
            hash <- (hash ^^^ uint64 b) * 0x100000001b3uL
            pos <- pos + 1L

        override __.Write(bytes : byte [], offset : int, count : int) =
            let mutable remaining = offset - count
            let mutable i = 0

            let inline append k = hash <- (hash ^^^ uint64 bytes.[k]) * 0x100000001b3uL

            while remaining > 8 do
                append i ; append (i+1) ; append (i+2) ; append (i+3)
                append (i+4) ; append (i+5) ; append (i+6) ; append (i+7)

                i <- i + 8
                remaining <- remaining - 8

            if remaining > 0 then
                for j = i to i + remaining - 1 do
                    append j

            pos <- pos + int64 count


    //
    //  128-bit MurMur-3 hashing algorithm
    //  adapted from http://blog.teamleadnet.com/2012/08/murmurhash3-ultra-fast-hash-algorithm.html
    //

    [<AutoOpen>]
    module MurMur3Utils =
        
        [<Literal>]
        let C1 = 0x87c37b91114253d5uL

        [<Literal>]
        let C2 = 0x4cf5ad432745937fuL

        let inline rotateLeft (original : uint64) (bits : int) = original <<< bits ||| original >>> (64 - bits)
        let inline rotateRight (original : uint64) (bits : int) = original >>> bits ||| original <<< (64 - bits)

        // 2x faster than BitConverter
        let inline bytesToUInt64 (bytes : byte []) (i : int) =
            let inline b2i j s = (uint64 bytes.[i + j]) <<< s

            uint64 (bytes.[i]) ||| b2i 1 8 ||| b2i 2 16 ||| b2i 3 24 |||
            b2i 4 32 ||| b2i 5 40 ||| b2i 6 48 ||| b2i 7 56

        let inline uint64ToBytes (buf : byte []) (i : int) (value : uint64) =
            buf.[i] <- byte value
            buf.[i+1] <- byte (value >>> 8)
            buf.[i+2] <- byte (value >>> 16)
            buf.[i+3] <- byte (value >>> 24)
            buf.[i+4] <- byte (value >>> 32)
            buf.[i+5] <- byte (value >>> 40)
            buf.[i+6] <- byte (value >>> 48)
            buf.[i+7] <- byte (value >>> 56)

        let inline mixkey1 (k : uint64) =
            let mutable k = k
            k <- k * C1
            k <- rotateLeft k 31
            k <- k * C2
            k

        let inline mixkey2 (k : uint64) =
            let mutable k = k
            k <- k * C2
            k <- rotateLeft k 33
            k <- k * C1
            k

        let inline mixBody (h1 : byref<uint64>) (h2 : byref<uint64>) (k1 : uint64) (k2 : uint64) =
            h1 <- h1 ^^^ mixkey1 k1

            h1 <- rotateLeft h1 27
            h1 <- h1 + h2
            h1 <- h1 * 5UL + 0x52dce729UL

            h2 <- h2 ^^^ mixkey2 k2

            h2 <- rotateLeft h2 31
            h2 <- h2 + h1
            h2 <- h2 * 5UL + 0x38495ab5UL

        let inline mixFinal (k : byref<uint64>) =
            k <- k ^^^ (k >>> 33)
            k <- k * 0xff51afd7ed558ccduL
            k <- k ^^^ (k >>> 33)
            k <- k * 0xc4ceb9fe1a85ec53uL
            k <- k ^^^ (k >>> 33)


    type MurMur3(?seed) =
        interface IHashStreamFactory with
            member __.Create () = new MurMur3Stream(?seed = seed) :> HashStream

    and MurMur3Stream(?seed : uint64) =
        inherit HashStream()

        let mutable h1 = defaultArg seed 0UL
        let mutable h2 = 0UL

        let mutable length = 0L

        let buf = Array.zeroCreate<byte> 16
        let mutable pos = 0 // write position on the buffer

        let computePartial (h1 : byref<uint64>) (h2 : byref<uint64>) =
            let k1 = bytesToUInt64 buf 0
            let k2 = bytesToUInt64 buf 8

            mixBody &h1 &h2 k1 k2

        override __.Length = length
        override __.Position = length

        override __.WriteByte(b : byte) =
            buf.[pos] <- b
            pos <- pos + 1
            length <- length + 1L

            if pos = 16 then
                computePartial &h1 &h2
                pos <- 0

        override __.Write(bytes : byte [], offset : int, count : int) =
            if pos + count < 16 then
                Buffer.BlockCopy(bytes, offset, buf, pos, count)
                pos <- pos + count
            else
                let mutable remaining = count
                let mutable i = offset

                if pos > 0 then
                    let copied = 16 - pos
                    Buffer.BlockCopy(bytes, i, buf, pos, copied)
                    computePartial &h1 &h2

                    remaining <- remaining - copied
                    i <- i + copied

                while remaining >= 16 do
                    let k1 = bytesToUInt64 buf i
                    let k2 = bytesToUInt64 buf (i+8)

                    do mixBody &h1 &h2 k1 k2

                    remaining <- remaining - 16
                    i <- i + 16

                if remaining > 0 then
                    Buffer.BlockCopy(bytes, i, buf, 0, remaining)
    
                pos <- remaining

            length <- length + int64 count


        override __.ComputeHash () =
            let mutable h1 = h1
            let mutable h2 = h2

            let length =
                if pos > 0 then 
                    computePartial &h1 &h2
                    uint64 length + 16UL - uint64 pos
                else
                    uint64 length

            h1 <- h1 ^^^ length
            h2 <- h2 ^^^ length

            h1 <- h1 + h2
            h2 <- h2 + h1

            mixFinal &h1
            mixFinal &h2

            h1 <- h1 + h2
            h2 <- h2 + h1

            let bytes = Array.zeroCreate<byte> 16

            uint64ToBytes bytes 0 h1
            uint64ToBytes bytes 8 h2

            bytes


    // just compute object size, discarding any data
    type internal LengthCounter () =
        inherit HashStream ()

        let mutable pos = 0L

        override __.ComputeHash () = raise <| new NotSupportedException()

        override __.Length = pos
        override __.Position = pos
        override __.WriteByte _ = pos <- pos + 1L
        override __.Write(_ : byte [], _ : int, count : int) = pos <- pos + int64 count