module internal Nessos.FsPickler.ArrayPicklers

    open System
    open System.Reflection

    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection
    open Nessos.FsPickler.PicklerUtils

    module private ArrayPicklerUtils =
        
        let inline isPrimitiveSerialized (fmt : ^Formatter) (ep : Pickler) =
            ep.TypeKind = TypeKind.Primitive && 
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
        

    open ArrayPicklerUtils

    // Array pickler combinators

    type ArrayPickler =

        static member Create (ep : Pickler<'T>) : Pickler<'T []> =
            
            let writer (w : WriteState) (array : 'T []) =

                let formatter = w.Formatter
                
                if isPrimitiveSerialized formatter ep then
                    formatter.WriteInt32 "length" array.Length
                    formatter.WritePrimitiveArray "array" array

                else
                    formatter.BeginWriteBoundedSequence "array" array.Length

                    for i = 0 to array.Length - 1 do
                        ep.Write w "elem" array.[i]

                    formatter.EndWriteBoundedSequence ()

            let reader (r : ReadState) =
                let formatter = r.Formatter

                if isPrimitiveSerialized formatter ep then
                    let length = formatter.ReadInt32 "length"
                    let array = Array.zeroCreate<'T> length
                    r.RegisterUninitializedArray array
                    formatter.ReadPrimitiveArray "array" array
                    array

                else
                    let length = formatter.BeginReadBoundedSequence "array"
                    let array = Array.zeroCreate<'T> length
                    r.RegisterUninitializedArray array
                    for i = 0 to length - 1 do
                        array.[i] <- ep.Read r "elem"

                    formatter.EndReadBoundedSequence ()
                    array

            mkPickler PicklerInfo.Array false true reader writer

        static member Create2D<'T> (ep : Pickler<'T>) =
            
            let writer (w : WriteState) (array : 'T[,]) =
                let formatter = w.Formatter
                let lengths = writeMultiDimensionalArrayRanks formatter 2 array
                
                if isPrimitiveSerialized formatter ep then
                    formatter.WritePrimitiveArray "array" array
                else
                    formatter.BeginWriteBoundedSequence "array" array.Length
                    for i = 0 to lengths.[0] - 1 do
                        for j = 0 to lengths.[1] - 1 do
                            ep.Write w "elem" array.[i,j]

                    formatter.EndWriteBoundedSequence ()

            let reader (r : ReadState) =
                let formatter = r.Formatter
                let lengths = readMultiDimensionalArrayRanks formatter 2
                
                let array = Array2D.zeroCreate<'T> lengths.[0] lengths.[1]

                // early register array to the deserialization cache
                r.RegisterUninitializedArray array

                if isPrimitiveSerialized formatter ep then
                    formatter.ReadPrimitiveArray "array" array
                else
                    let _ = formatter.BeginReadBoundedSequence "array"

                    for i = 0 to lengths.[0] - 1 do
                        for j = 0 to lengths.[1] - 1 do
                            array.[i,j] <- ep.Read r "elem"

                    formatter.EndReadBoundedSequence ()

                array

            mkPickler PicklerInfo.Array false true reader writer


        static member Create3D<'T> (ep : Pickler<'T>) =
            
            let writer (w : WriteState) (array : 'T[,,]) =
                let formatter = w.Formatter
                let lengths = writeMultiDimensionalArrayRanks formatter 3 array
                
                if isPrimitiveSerialized formatter ep then
                    formatter.WritePrimitiveArray "array" array
                else
                    formatter.BeginWriteBoundedSequence "array" array.Length
                    for i = 0 to lengths.[0] - 1 do
                        for j = 0 to lengths.[1] - 1 do
                            for k = 0 to lengths.[2] - 1 do
                                ep.Write w "elem" array.[i,j,k]

                    formatter.EndWriteBoundedSequence ()

            let reader (r : ReadState) =
                let formatter = r.Formatter
                let lengths = readMultiDimensionalArrayRanks formatter 3
                
                let array = Array3D.zeroCreate<'T> lengths.[0] lengths.[1] lengths.[2]

                // early register array to the deserialization cache
                r.RegisterUninitializedArray array

                if isPrimitiveSerialized formatter ep then
                    formatter.ReadPrimitiveArray "array" array
                else
                    let _ = formatter.BeginReadBoundedSequence "array"

                    for i = 0 to lengths.[0] - 1 do
                        for j = 0 to lengths.[1] - 1 do
                            for k = 0 to lengths.[2] - 1 do
                                array.[i,j,k] <- ep.Read r "elem"

                    formatter.EndReadBoundedSequence ()

                array

            mkPickler PicklerInfo.Array false true reader writer


        static member Create4D<'T> (ep : Pickler<'T>) =
            
            let writer (w : WriteState) (array : 'T[,,,]) =
                let formatter = w.Formatter
                let lengths = writeMultiDimensionalArrayRanks formatter 4 array
                
                if isPrimitiveSerialized formatter ep then
                    formatter.WritePrimitiveArray "array" array
                else
                    formatter.BeginWriteBoundedSequence "array" array.Length
                    for i = 0 to lengths.[0] - 1 do
                        for j = 0 to lengths.[1] - 1 do
                            for k = 0 to lengths.[2] - 1 do
                                for l = 0 to lengths.[3] - 1 do
                                    ep.Write w "elem" array.[i,j,k,l]

                    formatter.EndWriteBoundedSequence ()

            let reader (r : ReadState) =
                let formatter = r.Formatter
                let lengths = readMultiDimensionalArrayRanks formatter 4
                
                let array = Array4D.zeroCreate<'T> lengths.[0] lengths.[1] lengths.[2] lengths.[3]

                // early register array to the deserialization cache
                r.RegisterUninitializedArray array

                if isPrimitiveSerialized formatter ep then
                    formatter.ReadPrimitiveArray "array" array
                else
                    let _ = formatter.BeginReadBoundedSequence "array"

                    for i = 0 to lengths.[0] - 1 do
                        for j = 0 to lengths.[1] - 1 do
                            for k = 0 to lengths.[2] - 1 do
                                for l = 0 to lengths.[2] - 1 do
                                    array.[i,j,k,l] <- ep.Read r "elem"

                    formatter.EndReadBoundedSequence ()

                array

            mkPickler PicklerInfo.Array false true reader writer

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