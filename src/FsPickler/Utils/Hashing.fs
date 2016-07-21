namespace MBrace.FsPickler.Hashing
    
open System
open System.IO

/// Hashed object result info
[<StructuralEquality>]
[<StructuralComparison>]
[<AutoSerializable(true)>]
type HashResult =
    {
        /// Hashing algorithm identifier
        Algorithm : string
        /// Type of hashed object
        Type : string
        /// Size of serialized object
        Length : int64
        /// Hash data
        Hash : byte []
    }

/// An immutable factory interface for HashStreams
type IHashStreamFactory =
    /// Create a hash streaming instance
    abstract Create : unit -> HashStream

/// An abstract byte sink used as a hash generating state machine
and [<AbstractClass>] HashStream() =
    inherit System.IO.Stream()

    /// Hash algorithm identifier
    abstract HashAlgorithm : string
    /// Generates hash based on accumulated stream data.
    abstract ComputeHash : unit -> byte []

    override __.CanRead = false
    override __.CanSeek = false
    override __.CanTimeout = false
    override __.CanWrite = true
    override __.ReadTimeout = 0
    override __.WriteTimeout = 0
    override __.Seek(_,_) = raise <| new NotSupportedException()
    override __.SetLength _ = raise <| new NotSupportedException()
    override __.Read(_,_,_) = raise <| new NotSupportedException()
    override __.Flush () = ()
    override __.Position with set _ = raise <| new NotSupportedException()

//
// 64-bit Fowler-Noll-Vo hashing algorithm
// adapted from http://home.comcast.net/~bretm/hash/6.html
//

/// 64-bit Fowler-Noll-Vo hashing algorithm
type FNV1aStreamFactory (?bits : int) =
    interface IHashStreamFactory with
        member __.Create () = new FNV1aStream(?bits = bits) :> HashStream

/// 64-bit Fowler-Noll-Vo hashing algorithm
and [<AutoSerializable(false)>]
  internal FNV1aStream (?bits : int) =
    inherit HashStream()

    let shift = 64 - (defaultArg bits 64)
    let mask = (1UL <<< shift) - 1UL

    let mutable pos = 0L
    let mutable hash = 0xcbf29ce484222325UL

    override __.HashAlgorithm = "FNV-1a"

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
        let mutable remaining = count
        let mutable i = offset

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
module internal MurMur3Utils =
        
    [<Literal>]
    let C1 = 0x87c37b91114253d5uL

    [<Literal>]
    let C2 = 0x4cf5ad432745937fuL

    let inline rotateLeft (original : uint64) (bits : int) = 
        (original <<< bits) ||| (original >>> (64 - bits))
    let inline rotateRight (original : uint64) (bits : int) = 
        (original >>> bits) ||| (original <<< (64 - bits))

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

    // assume k is all 0's where b is to be written
    let inline writeByte (k : uint64) (i : int) (b : byte) =
        k ||| (uint64 b <<< i * 8)

    let inline writeBytes (k : byref<uint64>) boffset koffset count (bs : byte []) =
        for i = 0 to count - 1 do
            k <- writeByte k (koffset + i) bs.[boffset + i]

/// MurMur3 128-bit hashing algorithm.
/// Optimized for 64-bit architectures
type MurMur3(?seed) =
    interface IHashStreamFactory with
        member __.Create () = new MurMur3Stream(?seed = seed) :> HashStream

and [<AutoSerializable(false)>]
  internal MurMur3Stream(?seed : uint64) =
    inherit HashStream()

    let mutable length = 0L

    // partial 128-bit hash store
    let mutable h1 = defaultArg seed 0UL
    let mutable h2 = 0UL

    // buffer
    let mutable k1 = 0UL
    let mutable k2 = 0UL

    // position parameters
    let mutable atK2 = false
    let mutable pos = 0

    override __.HashAlgorithm = "MurMur3"

    override __.Length = length
    override __.Position = length

    override __.WriteByte(b : byte) =
        if atK2 then
            k2 <- writeByte k2 pos b
        else
            k1 <- writeByte k1 pos b

        if pos = 7 then 
            if atK2 then
                // got 16 bytes, perform partial hash computation and reset state
                mixBody &h1 &h2 k1 k2
                k1 <- 0UL
                k2 <- 0UL
            pos <- 0
            atK2 <- not atK2
        else
            pos <- pos + 1
                
        length <- length + 1L

    override __.Write(bytes : byte [], offset : int, count : int) =
        if pos + count < 8 then
            // data insufficient to fill a block, just copy & increase count
            if atK2 then
                writeBytes &k2 offset pos count bytes
            else
                writeBytes &k1 offset pos count bytes

            pos <- pos + count
        else
            let mutable remaining = count
            let mutable i = offset
            let mutable latK2 = atK2

            if pos > 0 then
                // synchronize with previous partial state
                let copied = 8 - pos
                if latK2 then
                    writeBytes &k2 i pos copied bytes
                    // got 16 bytes, perform partial hash computation and reset state
                    mixBody &h1 &h2 k1 k2
                    k1 <- 0UL
                    k2 <- 0UL
                else
                    writeBytes &k1 i pos copied bytes

                latK2 <- not latK2
                remaining <- remaining - copied
                i <- i + copied

            // synced, now compute in blocks

            while remaining >= 8 do
                if latK2 then
                    k2 <- bytesToUInt64 bytes i
                    // got 16 bytes, perform partial hash computation and reset state
                    mixBody &h1 &h2 k1 k2
                    k1 <- 0UL
                    k2 <- 0UL
                else
                    k1 <- bytesToUInt64 bytes i
                    
                latK2 <- not latK2
                remaining <- remaining - 8
                i <- i + 8

            // copy any remaining data

            if remaining > 0 then
                if latK2 then
                    writeBytes &k2 i 0 remaining bytes
                else
                    writeBytes &k1 i 0 remaining bytes

            pos <- remaining
            atK2 <- latK2

        length <- length + int64 count


    override __.ComputeHash () =
        let length = uint64 length
        let mutable h1 = h1
        let mutable h2 = h2

        // have unprocessed bytes, perform a final hash operation & normalize length
        if pos > 0 || atK2 then mixBody &h1 &h2 k1 k2

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