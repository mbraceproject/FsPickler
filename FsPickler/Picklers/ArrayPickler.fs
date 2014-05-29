module internal Nessos.FsPickler.ArrayPickler

    open System
    open System.Reflection

    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection
    open Nessos.FsPickler.PicklerUtils

    // Array pickler combinator

    type ArrayPickler =

        static member CreateUntyped(t : Type, resolver : IPicklerResolver) =
            let et = t.GetElementType()
            let ep = resolver.Resolve et
            let m =
                typeof<ArrayPickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| et ; t |]

            m.GuardedInvoke(null, [| ep :> obj |]) :?> Pickler

        static member Create<'T, 'Array when 'Array :> Array> (ep : Pickler<'T>) : Pickler<'Array> =
            assert(typeof<'T> = typeof<'Array>.GetElementType())
            let rank = typeof<'Array>.GetArrayRank()

            let writer (w : WriteState) (array : 'Array) =
                // check if array is to be serialized using formatter support
                let isPrimitiveSerialized = 
                    ep.TypeInfo = TypeInfo.Primitive && w.Formatter.IsPrimitiveArraySerializationSupported

                // write initial array data
                let lengths =
                    if rank = 1 then
                        if isPrimitiveSerialized then
                            w.Formatter.WriteInt32 "length" array.Length
                            null
                        else
                            w.Formatter.BeginWriteBoundedSequence "array" array.Length
                            null
                    else
                        let lengths = Array.zeroCreate<int> rank
                        for d = 0 to rank - 1 do 
                            lengths.[d] <- array.GetLength d
                            w.Formatter.WriteInt32 (sprintf "length-%d" d) (lengths.[d])

                        w.Formatter.BeginWriteBoundedSequence "array" array.Length
                        lengths

                if isPrimitiveSerialized then
                    w.Formatter.WritePrimitiveArray "data" array

                else
                    match rank with
                    | 1 ->
                        let array = fastUnbox<'T []> array
                        for i = 0 to array.Length - 1 do
                            ep.Write w "elem" array.[i]
                    | 2 -> 
                        let array = fastUnbox<'T [,]> array
                        for i = 0 to lengths.[0] - 1 do
                            for j = 0 to lengths.[1] - 1 do
                                ep.Write w "elem" array.[i,j]
                    | 3 ->
                        let array = fastUnbox<'T [,,]> array
                        for i = 0 to lengths.[0] - 1 do
                            for j = 0 to lengths.[1] - 1 do
                                for k = 0 to lengths.[2] - 1 do
                                    ep.Write w "elem" array.[i,j,k]
                    | 4 ->
                        let array = fastUnbox<'T [,,,]> array
                        for i = 0 to lengths.[0] - 1 do
                            for j = 0 to lengths.[1] - 1 do
                                for k = 0 to lengths.[2] - 1 do
                                    for l = 0 to lengths.[3] - 1 do
                                        ep.Write w "elem" array.[i,j,k,l]

                    | _ -> failwith "impossible array rank"

                    w.Formatter.EndWriteBoundedSequence ()

            let reader (r : ReadState) =
                // check if array is to be deserialized using formatter support
                let isPrimitiveDeserialized = 
                    ep.TypeInfo = TypeInfo.Primitive && r.Formatter.IsPrimitiveArraySerializationSupported
                
                // read initial array data
                let lengths =
                    if rank = 1 then
                        let length =
                            if isPrimitiveDeserialized then r.Formatter.ReadInt32 "length"
                            else
                                r.Formatter.BeginReadBoundedSequence "array"

                        [|length|]

                    else
                        let l = Array.zeroCreate<int> rank
                        for d = 0 to rank - 1 do l.[d] <- r.Formatter.ReadInt32 (sprintf "length-%d" d)
                        let _ = r.Formatter.BeginReadBoundedSequence "array"
                        l

                // initialize empty array
                let array =
                    match rank with
                    | 1 -> Array.zeroCreate<'T> lengths.[0] |> fastUnbox<'Array>
                    | 2 -> Array2D.zeroCreate<'T> lengths.[0] lengths.[1] |> fastUnbox<'Array>
                    | 3 -> Array3D.zeroCreate<'T> lengths.[0] lengths.[1] lengths.[2] |> fastUnbox<'Array>
                    | 4 -> Array4D.zeroCreate<'T> lengths.[0] lengths.[1] lengths.[2] lengths.[3] |> fastUnbox<'Array>
                    | _ -> failwith "impossible array rank"

                // register new array with deserializer cache
                r.RegisterUninitializedArray array

                if isPrimitiveDeserialized then
                    do r.Formatter.ReadPrimitiveArray "data" array
                else
                    match rank with
                    | 1 -> 
                        let arr = fastUnbox<'T []> array
                        for i = 0 to lengths.[0] - 1 do
                            arr.[i] <- ep.Read r "elem"

                    | 2 -> 
                        let arr = fastUnbox<'T [,]> array
                        for i = 0 to lengths.[0] - 1 do
                            for j = 0 to lengths.[1] - 1 do
                                arr.[i,j] <- ep.Read r "elem"

                    | 3 ->
                        let arr = fastUnbox<'T [,,]> array
                        for i = 0 to lengths.[0] - 1 do
                            for j = 0 to lengths.[1] - 1 do
                                for k = 0 to lengths.[2] - 1 do
                                    arr.[i,j,k] <- ep.Read r "elem"

                    | 4 ->
                        let arr = fastUnbox<'T [,,,]> array
                        for i = 0 to lengths.[0] - 1 do
                            for j = 0 to lengths.[1] - 1 do
                                for k = 0 to lengths.[2] - 1 do
                                    for l = 0 to lengths.[3] - 1 do
                                        arr.[i,j,k,l] <- ep.Read r "elem"

                    | _ -> failwith "impossible array rank"

                    r.Formatter.EndReadBoundedSequence ()

                array

            CompositePickler.Create<'Array>(reader, writer, PicklerInfo.Array, cacheByRef = true, useWithSubtypes = false)