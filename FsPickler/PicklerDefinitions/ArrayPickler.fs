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

            let writer (w : WriteState) (x : 'Array) =

                let lengths = Array.zeroCreate<int> rank
                for d = 0 to rank - 1 do
                    lengths.[d] <- x.GetLength d
                    w.Formatter.WriteInt32 (string d) lengths.[d]

                if ep.TypeInfo = TypeInfo.Primitive && w.Formatter.IsPrimitiveArraySerializationSupported then
                    w.Formatter.WritePrimitiveArray "data" x

                else
                    match rank with
                    | 1 ->
                        let x = fastUnbox<'T []> x
                        for i = 0 to x.Length - 1 do
                            ep.Write w "item" x.[i]
                    | 2 -> 
                        let x = fastUnbox<'T [,]> x
                        for i = 0 to lengths.[0] - 1 do
                            for j = 0 to lengths.[1] - 1 do
                                ep.Write w "item" x.[i,j]
                    | 3 ->
                        let x = fastUnbox<'T [,,]> x
                        for i = 0 to lengths.[0] - 1 do
                            for j = 0 to lengths.[1] - 1 do
                                for k = 0 to lengths.[2] - 1 do
                                    ep.Write w "item" x.[i,j,k]
                    | 4 ->
                        let x = fastUnbox<'T [,,,]> x
                        for i = 0 to lengths.[0] - 1 do
                            for j = 0 to lengths.[1] - 1 do
                                for k = 0 to lengths.[2] - 1 do
                                    for l = 0 to lengths.[3] - 1 do
                                        ep.Write w "item" x.[i,j,k,l]

                    | _ -> failwith "impossible array rank"

            let reader (r : ReadState) =
                let l = Array.zeroCreate<int> rank
                for i = 0 to rank - 1 do l.[i] <- r.Formatter.ReadInt32 (string i)

                let array =
                    match rank with
                    | 1 -> Array.zeroCreate<'T> l.[0] |> fastUnbox<'Array>
                    | 2 -> Array2D.zeroCreate<'T> l.[0] l.[1] |> fastUnbox<'Array>
                    | 3 -> Array3D.zeroCreate<'T> l.[0] l.[1] l.[2] |> fastUnbox<'Array>
                    | 4 -> Array4D.zeroCreate<'T> l.[0] l.[1] l.[2] l.[3] |> fastUnbox<'Array>
                    | _ -> failwith "impossible array rank"

                // register new object with deserializer cache
                r.RegisterUninitializedArray array

                if ep.TypeInfo = TypeInfo.Primitive && r.Formatter.IsPrimitiveArraySerializationSupported then
                    do r.Formatter.ReadPrimitiveArray "data" array
                    array
                else

                    match rank with
                    | 1 -> 
                        let arr = fastUnbox<'T []> array
                        for i = 0 to l.[0] - 1 do
                            arr.[i] <- ep.Read r "item"

                        array
                    | 2 -> 
                        let arr = fastUnbox<'T [,]> array
                        for i = 0 to l.[0] - 1 do
                            for j = 0 to l.[1] - 1 do
                                arr.[i,j] <- ep.Read r "item"

                        array
                    | 3 ->
                        let arr = fastUnbox<'T [,,]> array
                        for i = 0 to l.[0] - 1 do
                            for j = 0 to l.[1] - 1 do
                                for k = 0 to l.[2] - 1 do
                                    arr.[i,j,k] <- ep.Read r "item"

                        array
                    | 4 ->
                        let arr = fastUnbox<'T [,,,]> array
                        for i = 0 to l.[0] - 1 do
                            for j = 0 to l.[1] - 1 do
                                for k = 0 to l.[2] - 1 do
                                    for l = 0 to l.[3] - 1 do
                                        arr.[i,j,k,l] <- ep.Read r "item"

                        array
                    | _ -> failwith "impossible array rank"

            CompositePickler.Create<'Array>(reader, writer, PicklerInfo.Array, cacheByRef = true, useWithSubtypes = false)