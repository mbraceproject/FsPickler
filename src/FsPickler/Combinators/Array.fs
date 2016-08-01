namespace MBrace.FsPickler

open System
open System.Reflection

open MBrace.FsPickler.Reflection
open MBrace.FsPickler.SequenceUtils

[<AutoOpen>]
module private ArrayPicklerUtils =
        
    let inline isPrimitiveSerialized (fmt : ^Formatter) (ep : Pickler) =
        ep.Kind = Kind.Primitive && 
            (^Formatter : (member IsPrimitiveArraySerializationSupported : bool) fmt)

    let lengthTags = [|0..10|] |> Array.map (sprintf "length-%d")

    let inline writeMultiDimensionalArrayRanks (fmt : IPickleFormatWriter) rank (array : Array) =
        let lengths = Array.zeroCreate<int> rank
        for d = 0 to rank - 1 do 
            lengths.[d] <- array.GetLength d
            fmt.WriteInt32 (lengthTags.[d]) (lengths.[d])
        lengths

    let inline readMultiDimensionalArrayRanks (fmt : IPickleFormatReader) rank =
        let lengths = Array.zeroCreate<int> rank
        for d = 0 to rank - 1 do
            lengths.[d] <- fmt.ReadInt32 (lengthTags.[d])
        lengths


// Array pickler combinators

type internal ArrayPickler =

    static member CreateByteArrayPickler () : Pickler<byte []> =
        let writer (w : WriteState) (tag : string) (bytes : byte []) = w.Formatter.WriteBytes tag bytes
        let reader (r : ReadState) (tag : string) = r.Formatter.ReadBytes tag
        let cloner (c : CloneState) (bytes : byte []) = 
            let bytes' = bytes.Clone() |> fastUnbox<byte []>
            c.EarlyRegisterArray bytes'
            bytes'

        CompositePickler.Create(reader, writer, cloner, ignore2, PicklerInfo.Array, skipHeaderWrite = false)

    static member Create (ep : Pickler<'T>) : Pickler<'T []> =
            
        let writer (w : WriteState) (tag : string) (array : 'T []) =

            let formatter = w.Formatter

            if isPrimitiveSerialized formatter ep then

                formatter.BeginWriteObject tag ObjectFlags.None
                formatter.WriteInt32 "length" array.Length
                formatter.WritePrimitiveArray "array" array
                formatter.EndWriteObject ()

            elif ep.IsRecursiveType || formatter.PreferLengthPrefixInSequences then
                    
                formatter.BeginWriteObject tag ObjectFlags.None
                formatter.WriteInt32 "length" array.Length
                formatter.BeginWriteObject "array" ObjectFlags.IsSequenceHeader

                for i = 0 to array.Length - 1 do
                    ep.Write w elemTag array.[i]

                formatter.EndWriteObject ()
                formatter.EndWriteObject ()

            else
                formatter.BeginWriteObject tag ObjectFlags.IsSequenceHeader

                for i = 0 to array.Length - 1 do
                    formatter.WriteNextSequenceElement true
                    ep.Write w elemTag array.[i]

                formatter.WriteNextSequenceElement false
                formatter.EndWriteObject ()

        let reader (r : ReadState) (_ : string) =
            let formatter = r.Formatter

            if isPrimitiveSerialized formatter ep then

                let length = formatter.ReadInt32 "length"
                let array = Array.zeroCreate<'T> length
                r.EarlyRegisterArray array
                formatter.ReadPrimitiveArray "array" array
                array

            elif ep.IsRecursiveType || formatter.PreferLengthPrefixInSequences then

                let length = formatter.ReadInt32 "length"
                let array = Array.zeroCreate<'T> length
                r.EarlyRegisterArray array

                do beginReadSequence formatter "array"

                for i = 0 to length - 1 do
                    array.[i] <- ep.Read r elemTag

                formatter.EndReadObject()

                array

            else
                let ra = new ResizeArray<'T> ()

                while formatter.ReadNextSequenceElement() do
                    ra.Add <| ep.Read r elemTag

                ra.ToArray()

        let cloner (c : CloneState) (array : 'T []) =
            if ep.Kind <= Kind.Enum then
                let array' = array.Clone() |> fastUnbox<'T[]>
                c.EarlyRegisterArray array'
                array'
            else
                let array' = Array.zeroCreate<'T> array.Length
                c.EarlyRegisterArray array'
                for i = 0 to array.Length - 1 do
                    array'.[i] <- ep.Clone c array.[i]
                array'

        let accepter (v : VisitState) (array : 'T[]) =
            for i = 0 to array.Length - 1 do
                ep.Accept v array.[i]

        CompositePickler.Create(reader, writer, cloner, accepter, PicklerInfo.Array, skipHeaderWrite = true)

    static member Create2D<'T> (ep : Pickler<'T>) =
            
        let writer (w : WriteState) (_ : string) (array : 'T[,]) =
            let formatter = w.Formatter
            let lengths = writeMultiDimensionalArrayRanks formatter 2 array
                
            if isPrimitiveSerialized formatter ep then
                formatter.WritePrimitiveArray "array" array
            else
                formatter.BeginWriteObject "array" ObjectFlags.IsSequenceHeader
                for i = 0 to lengths.[0] - 1 do
                    for j = 0 to lengths.[1] - 1 do
                        ep.Write w elemTag array.[i,j]

                formatter.EndWriteObject ()

        let reader (r : ReadState) (_ : string) =
            let formatter = r.Formatter
            let lengths = readMultiDimensionalArrayRanks formatter 2
                
            let array = Array2D.zeroCreate<'T> lengths.[0] lengths.[1]

            // early register array to the deserialization cache
            r.EarlyRegisterArray array

            if isPrimitiveSerialized formatter ep then
                formatter.ReadPrimitiveArray "array" array
            else
                do beginReadSequence formatter "array"

                for i = 0 to lengths.[0] - 1 do
                    for j = 0 to lengths.[1] - 1 do
                        array.[i,j] <- ep.Read r elemTag

                formatter.EndReadObject ()

            array

        let cloner (c : CloneState) (array : 'T [,]) =
            if ep.Kind <= Kind.Enum then
                let array' = array.Clone() |> fastUnbox<'T[,]>
                c.EarlyRegisterArray array'
                array'
            else
                let l1 = array.GetLength 0
                let l2 = array.GetLength 1
                let array' = Array2D.zeroCreate<'T> l1 l2
                c.EarlyRegisterArray array'
                for i = 0 to l1 - 1 do
                    for j = 0 to l2 - 1 do
                        array'.[i,j] <- ep.Clone c array.[i,j]

                array'

        let accepter (v : VisitState) (array : 'T[,]) =
            for i = 0 to array.GetLength 0 - 1 do
                for j = 0 to array.GetLength 1 - 1 do
                    ep.Accept v array.[i,j]

        CompositePickler.Create(reader, writer, cloner, accepter, PicklerInfo.Array, skipHeaderWrite = false)


    static member Create3D<'T> (ep : Pickler<'T>) =
            
        let writer (w : WriteState) (_ : string) (array : 'T[,,]) =
            let formatter = w.Formatter
            let lengths = writeMultiDimensionalArrayRanks formatter 3 array
                
            if isPrimitiveSerialized formatter ep then
                formatter.WritePrimitiveArray "array" array
            else
                formatter.BeginWriteObject "array" ObjectFlags.IsSequenceHeader
                for i = 0 to lengths.[0] - 1 do
                    for j = 0 to lengths.[1] - 1 do
                        for k = 0 to lengths.[2] - 1 do
                            ep.Write w elemTag array.[i,j,k]

                formatter.EndWriteObject ()

        let reader (r : ReadState) (_ : string) =
            let formatter = r.Formatter
            let lengths = readMultiDimensionalArrayRanks formatter 3
                
            let array = Array3D.zeroCreate<'T> lengths.[0] lengths.[1] lengths.[2]

            // early register array to the deserialization cache
            r.EarlyRegisterArray array

            if isPrimitiveSerialized formatter ep then
                formatter.ReadPrimitiveArray "array" array
            else
                do beginReadSequence formatter "array"

                for i = 0 to lengths.[0] - 1 do
                    for j = 0 to lengths.[1] - 1 do
                        for k = 0 to lengths.[2] - 1 do
                            array.[i,j,k] <- ep.Read r elemTag

                formatter.EndReadObject ()

            array

        let cloner (c : CloneState) (array : 'T [,,]) =
            if ep.Kind <= Kind.Enum then
                let array' = array.Clone() |> fastUnbox<'T[,,]>
                c.EarlyRegisterArray array'
                array'
            else
                let l1 = array.GetLength 0
                let l2 = array.GetLength 1
                let l3 = array.GetLength 2
                let array' = Array3D.zeroCreate<'T> l1 l2 l3
                c.EarlyRegisterArray array'
                for i = 0 to l1 - 1 do
                    for j = 0 to l2 - 1 do
                        for k = 0 to l3 - 1 do
                            array'.[i,j,k] <- ep.Clone c array.[i,j,k]

                array'

        let accepter (v : VisitState) (array : 'T[,,]) =
            for i = 0 to array.GetLength 0 - 1 do
                for j = 0 to array.GetLength 1 - 1 do
                    for k = 0 to array.GetLength 2 - 1 do
                        ep.Accept v array.[i,j,k]

        CompositePickler.Create(reader, writer, cloner, accepter, PicklerInfo.Array, skipHeaderWrite = false)


    static member Create4D<'T> (ep : Pickler<'T>) =
            
        let writer (w : WriteState) (_ : string) (array : 'T[,,,]) =
            let formatter = w.Formatter
            let lengths = writeMultiDimensionalArrayRanks formatter 4 array
                
            if isPrimitiveSerialized formatter ep then
                formatter.WritePrimitiveArray "array" array
            else
                formatter.BeginWriteObject "array" ObjectFlags.IsSequenceHeader

                for i = 0 to lengths.[0] - 1 do
                    for j = 0 to lengths.[1] - 1 do
                        for k = 0 to lengths.[2] - 1 do
                            for l = 0 to lengths.[3] - 1 do
                                ep.Write w elemTag array.[i,j,k,l]

                formatter.EndWriteObject ()

        let reader (r : ReadState) (_ : string) =
            let formatter = r.Formatter
            let lengths = readMultiDimensionalArrayRanks formatter 4
                
            let array = Array4D.zeroCreate<'T> lengths.[0] lengths.[1] lengths.[2] lengths.[3]

            // early register array to the deserialization cache
            r.EarlyRegisterArray array

            if isPrimitiveSerialized formatter ep then
                formatter.ReadPrimitiveArray "array" array
            else
                do beginReadSequence formatter "array"

                for i = 0 to lengths.[0] - 1 do
                    for j = 0 to lengths.[1] - 1 do
                        for k = 0 to lengths.[2] - 1 do
                            for l = 0 to lengths.[3] - 1 do
                                array.[i,j,k,l] <- ep.Read r elemTag

                formatter.EndReadObject ()

            array

        let cloner (c : CloneState) (array : 'T [,,,]) =
            if ep.Kind <= Kind.Enum then
                let array' = array.Clone() |> fastUnbox<'T[,,,]>
                c.EarlyRegisterArray array'
                array'
            else
                let l1 = array.GetLength 0
                let l2 = array.GetLength 1
                let l3 = array.GetLength 2
                let l4 = array.GetLength 3
                let array' = Array4D.zeroCreate<'T> l1 l2 l3 l4
                c.EarlyRegisterArray array'
                for i = 0 to l1 - 1 do
                    for j = 0 to l2 - 1 do
                        for k = 0 to l3 - 1 do
                            for l = 0 to l4 - 1 do
                            array'.[i,j,k,l] <- ep.Clone c array.[i,j,k,l]

                array'

        let accepter (v : VisitState) (array : 'T[,,,]) =
            for i = 0 to array.GetLength 0 - 1 do
                for j = 0 to array.GetLength 1 - 1 do
                    for k = 0 to array.GetLength 2 - 1 do
                        for l = 0 to array.GetLength 3 - 1 do
                            ep.Accept v array.[i,j,k,l]

        CompositePickler.Create(reader, writer, cloner, accepter, PicklerInfo.Array, skipHeaderWrite = false)

    static member Create<'T> (resolver : IPicklerResolver) =
        let ep = resolver.Resolve<'T> ()
        ArrayPickler.Create<'T>(ep)

    static member Create2D<'T> (resolver : IPicklerResolver) =
        let ep = resolver.Resolve<'T> ()
        ArrayPickler.Create2D<'T>(ep)

    static member Create3D<'T> (resolver : IPicklerResolver) =
        let ep = resolver.Resolve<'T> ()
        ArrayPickler.Create3D<'T>(ep)

    static member Create4D<'T> (resolver : IPicklerResolver) =
        let ep = resolver.Resolve<'T> ()
        ArrayPickler.Create4D<'T>(ep)