module internal FsCoreSerializer.ArrayFormatter

    open System
    open System.IO
    open System.Reflection
    open System.Threading

    open FsCoreSerializer
    open FsCoreSerializer.Utils
    open FsCoreSerializer.FormatterUtils
    open FsCoreSerializer.BaseFormatters

    type ArrayFormatter =

        static member Create<'T, 'Array when 'Array :> Array> (resolver : IFormatterResolver) =
            assert(typeof<'T> = typeof<'Array>.GetElementType())
            let ef = resolver.Resolve<'T> ()
            let rank = typeof<'Array>.GetArrayRank()

            let writer (w : Writer) (x : 'Array) =

                for d = 0 to rank - 1 do
                    w.BW.Write(x.GetLength d)

                if ef.TypeInfo = TypeInfo.Primitive then
                    Stream.WriteArray(w.BW.BaseStream, x)
                else
                    let isValue = ef.TypeInfo <= TypeInfo.Value
                             
                    match rank with
                    | 1 ->
                        let x = x :> obj :?> 'T []
                        for i = 0 to x.Length - 1 do
                            write isValue w ef x.[i]
                    | 2 -> 
                        let x = x :> obj :?> 'T [,]
                        for i = 0 to x.GetLength(0) - 1 do
                            for j = 0 to x.GetLength(1) - 1 do
                                write isValue w ef x.[i,j]
                    | 3 ->
                        let x = x :> obj :?> 'T [,,]
                        for i = 0 to x.GetLength(0) - 1 do
                            for j = 0 to x.GetLength(1) - 1 do
                                for k = 0 to x.GetLength(2) - 1 do
                                    write isValue w ef x.[i,j,k]
                    | 4 ->
                        let x = x :> obj :?> 'T [,,,]
                        for i = 0 to x.GetLength(0) - 1 do
                            for j = 0 to x.GetLength(1) - 1 do
                                for k = 0 to x.GetLength(2) - 1 do
                                    for l = 0 to x.GetLength(3) - 1 do
                                        write isValue w ef x.[i,j,k,l]
                    | _ -> failwith "impossible array rank"

            let reader (r : Reader) =
                let l = Array.zeroCreate<int> rank
                for i = 0 to rank - 1 do l.[i] <- r.BR.ReadInt32()

                if ef.TypeInfo = TypeInfo.Primitive then
                    let array =
                        match rank with
                        | 1 -> Array.zeroCreate<'T> l.[0] :> Array
                        | 2 -> Array2D.zeroCreate<'T> l.[0] l.[1] :> Array
                        | 3 -> Array3D.zeroCreate<'T> l.[0] l.[1] l.[2] :> Array
                        | 4 -> Array4D.zeroCreate<'T> l.[0] l.[1] l.[2] l.[3] :> Array
                        | _ -> failwith "impossible array rank"

                    r.EarlyRegisterObject array

                    Stream.CopyToArray(r.BR.BaseStream, array)

                    array :?> 'Array
                else
                    let isValue = ef.TypeInfo <= TypeInfo.Value

                    match rank with
                    | 1 -> 
                        let arr = Array.zeroCreate<'T> l.[0]
                        r.EarlyRegisterObject arr
                        for i = 0 to l.[0] - 1 do
                            arr.[i] <- read isValue r ef
                        arr :> obj :?> 'Array
                    | 2 -> 
                        let arr = Array2D.zeroCreate<'T> l.[0] l.[1]
                        r.EarlyRegisterObject arr
                        for i = 0 to l.[0] - 1 do
                            for j = 0 to l.[1] - 1 do
                                arr.[i,j] <- read isValue r ef
                        arr :> obj :?> 'Array
                    | 3 ->
                        let arr = Array3D.zeroCreate<'T> l.[0] l.[1] l.[2]
                        r.EarlyRegisterObject arr
                        for i = 0 to l.[0] - 1 do
                            for j = 0 to l.[1] - 1 do
                                for k = 0 to l.[2] - 1 do
                                    arr.[i,j,k] <- read isValue r ef
                        arr :> obj :?> 'Array
                    | 4 ->
                        let arr = Array4D.zeroCreate<'T> l.[0] l.[1] l.[2] l.[3]
                        r.EarlyRegisterObject arr
                        for i = 0 to l.[0] - 1 do
                            for j = 0 to l.[1] - 1 do
                                for k = 0 to l.[2] - 1 do
                                    for l = 0 to l.[3] - 1 do
                                        arr.[i,j,k,l] <- read isValue r ef
                        arr :> obj :?> 'Array
                    | _ -> failwith "impossible array rank"

            new Formatter<'Array>(reader, writer, FormatterInfo.ReflectionDerived, true, false)

        static member CreateUntyped(t : Type, resolver : IFormatterResolver) =
            let et = t.GetElementType()
            let m =
                typeof<ArrayFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| et ; t |]

            m.Invoke(null, [| resolver :> obj |]) :?> Formatter