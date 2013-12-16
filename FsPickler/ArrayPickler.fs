module internal FsPickler.ArrayPickler

    open System
    open System.Reflection

    open FsPickler
    open FsPickler.Utils
    open FsPickler.PicklerUtils

    // Array pickler combinator

    type ArrayPickler =

        static member CreateUntyped(t : Type, resolver : IPicklerResolver) =
            let et = t.GetElementType()
            let ef = resolver.Resolve et
            let m =
                typeof<ArrayPickler>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| et ; t |]

            m.GuardedInvoke(null, [| ef :> obj |]) :?> Pickler

        static member Create<'T, 'Array when 'Array :> Array> (ef : Pickler<'T>) : Pickler<'Array> =
            assert(typeof<'T> = typeof<'Array>.GetElementType())
            let rank = typeof<'Array>.GetArrayRank()

            let writer (w : Writer) (x : 'Array) =

                let lengths = Array.zeroCreate<int> rank
                for d = 0 to rank - 1 do
                    lengths.[d] <- x.GetLength d
                    w.BinaryWriter.Write(lengths.[d])

                if ef.TypeKind = TypeKind.Primitive && keepEndianness then
                    // block copy without bothering with endianness
                    Stream.ReadFromArray(w.BinaryWriter.BaseStream, x)
                else
                    let isValue = ef.TypeKind <= TypeKind.Value
                             
                    match rank with
                    | 1 ->
                        let x = fastUnbox<'T []> x
                        for i = 0 to x.Length - 1 do
                            write isValue w ef x.[i]
                    | 2 -> 
                        let x = fastUnbox<'T [,]> x
                        for i = 0 to lengths.[0] - 1 do
                            for j = 0 to lengths.[1] - 1 do
                                write isValue w ef x.[i,j]
                    | 3 ->
                        let x = fastUnbox<'T [,,]> x
                        for i = 0 to lengths.[0] - 1 do
                            for j = 0 to lengths.[1] - 1 do
                                for k = 0 to lengths.[2] - 1 do
                                    write isValue w ef x.[i,j,k]
                    | 4 ->
                        let x = fastUnbox<'T [,,,]> x
                        for i = 0 to lengths.[0] - 1 do
                            for j = 0 to lengths.[1] - 1 do
                                for k = 0 to lengths.[2] - 1 do
                                    for l = 0 to lengths.[3] - 1 do
                                        write isValue w ef x.[i,j,k,l]
                    | _ -> failwith "impossible array rank"

            let reader (r : Reader) =
                let l = Array.zeroCreate<int> rank
                for i = 0 to rank - 1 do l.[i] <- r.BinaryReader.ReadInt32()

                let array =
                    match rank with
                    | 1 -> Array.zeroCreate<'T> l.[0] |> fastUnbox<'Array>
                    | 2 -> Array2D.zeroCreate<'T> l.[0] l.[1] |> fastUnbox<'Array>
                    | 3 -> Array3D.zeroCreate<'T> l.[0] l.[1] l.[2] |> fastUnbox<'Array>
                    | 4 -> Array4D.zeroCreate<'T> l.[0] l.[1] l.[2] l.[3] |> fastUnbox<'Array>
                    | _ -> failwith "impossible array rank"

                // register new object with deserializer cache
                r.EarlyRegisterObject array

                if ef.TypeKind = TypeKind.Primitive && keepEndianness then
                    Stream.WriteToArray(r.BinaryReader.BaseStream, array)

                    array
                else
                    let isValue = ef.TypeKind <= TypeKind.Value

                    match rank with
                    | 1 -> 
                        let arr = fastUnbox<'T []> array
                        for i = 0 to l.[0] - 1 do
                            arr.[i] <- read isValue r ef

                        array
                    | 2 -> 
                        let arr = fastUnbox<'T [,]> array
                        for i = 0 to l.[0] - 1 do
                            for j = 0 to l.[1] - 1 do
                                arr.[i,j] <- read isValue r ef

                        array
                    | 3 ->
                        let arr = fastUnbox<'T [,,]> array
                        for i = 0 to l.[0] - 1 do
                            for j = 0 to l.[1] - 1 do
                                for k = 0 to l.[2] - 1 do
                                    arr.[i,j,k] <- read isValue r ef

                        array
                    | 4 ->
                        let arr = fastUnbox<'T [,,,]> array
                        for i = 0 to l.[0] - 1 do
                            for j = 0 to l.[1] - 1 do
                                for k = 0 to l.[2] - 1 do
                                    for l = 0 to l.[3] - 1 do
                                        arr.[i,j,k,l] <- read isValue r ef

                        array
                    | _ -> failwith "impossible array rank"

            new Pickler<'Array>(reader, writer, PicklerInfo.Array, cacheByRef = true, useWithSubtypes = false)