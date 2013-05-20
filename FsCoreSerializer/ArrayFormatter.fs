module internal FsCoreSerializer.ArrayFormatter

    open System
    open System.IO
    open System.Threading

    open FsCoreSerializer
    open FsCoreSerializer.Utils
    open FsCoreSerializer.BaseFormatters
    open FsCoreSerializer.BaseFormatters.Utils


    type ArrayFormatter<'T> (t : Type) =

        interface IFormatterFactory with
            member __.Create (resolver : Type -> Lazy<Formatter>) =
                assert(typeof<'T> = t.GetElementType())
                let ef = resolver typeof<'T>
                let rank = t.GetArrayRank()

                let writer (w : Writer) (o : obj) =
                    let array = o :?> Array
                    for d = 0 to rank - 1 do
                        w.BW.Write(array.GetLength d)

                    let ef = ef.Value

                    if ef.TypeInfo = TypeInfo.Primitive then
                        Stream.WriteArray(w.BW.BaseStream, array)
                    else        
                        match rank with
                        | 1 ->
                            let arr = o :?> 'T []
                            for i = 0 to arr.Length - 1 do
                                write w ef arr.[i]
                        | 2 -> 
                            let arr = o :?> 'T [,]
                            for i = 0 to arr.GetLength(0) - 1 do
                                for j = 0 to arr.GetLength(1) - 1 do
                                    write w ef arr.[i,j]
                        | 3 ->
                            let arr = o :?> 'T [,,]
                            for i = 0 to arr.GetLength(0) - 1 do
                                for j = 0 to arr.GetLength(1) - 1 do
                                    for k = 0 to arr.GetLength(2) - 1 do
                                        write w ef arr.[i,j,k]
                        | 4 ->
                            let arr = o :?> 'T [,,,]
                            for i = 0 to arr.GetLength(0) - 1 do
                                for j = 0 to arr.GetLength(1) - 1 do
                                    for k = 0 to arr.GetLength(2) - 1 do
                                        for l = 0 to arr.GetLength(3) - 1 do
                                            write w ef arr.[i,j,k,l]
                        | _ -> failwith "impossible array rank"

                let reader (r : Reader) =
                    let l = Array.zeroCreate<int> rank
                    for i = 0 to rank - 1 do l.[i] <- r.BR.ReadInt32()
                    let ef = ef.Value

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

                        array :> obj
                    else
                        match rank with
                        | 1 -> 
                            let arr = Array.zeroCreate<'T> l.[0]
                            r.EarlyRegisterObject arr
                            for i = 0 to l.[0] - 1 do
                                arr.[i] <- read r ef :?> 'T
                            arr :> obj
                        | 2 -> 
                            let arr = Array2D.zeroCreate<'T> l.[0] l.[1]
                            r.EarlyRegisterObject arr
                            for i = 0 to l.[0] - 1 do
                                for j = 0 to l.[1] - 1 do
                                    arr.[i,j] <- read r ef :?> 'T
                            arr :> _
                        | 3 ->
                            let arr = Array3D.zeroCreate<'T> l.[0] l.[1] l.[2]
                            r.EarlyRegisterObject arr
                            for i = 0 to l.[0] - 1 do
                                for j = 0 to l.[1] - 1 do
                                    for k = 0 to l.[2] - 1 do
                                        arr.[i,j,k] <- read r ef :?> 'T
                            arr :> _
                        | 4 ->
                            let arr = Array4D.zeroCreate<'T> l.[0] l.[1] l.[2] l.[3]
                            r.EarlyRegisterObject arr
                            for i = 0 to l.[0] - 1 do
                                for j = 0 to l.[1] - 1 do
                                    for k = 0 to l.[2] - 1 do
                                        for l = 0 to l.[3] - 1 do
                                            arr.[i,j,k,l] <- read r ef :?> 'T
                            arr :> _
                        | _ -> failwith "impossible array rank"

                {
                    Type = t
                    TypeInfo = getTypeInfo t
                    TypeHash = ObjHeader.getTruncatedHash t

                    Write = writer
                    Read = reader

                    FormatterInfo = FormatterInfo.ReflectionDerived
                    UseWithSubtypes = false
                    CacheObj = true
                }

    let mkArrayFormatter (resolver : Type -> Lazy<Formatter>) (t : Type) =
        let formatterType = typedefof<ArrayFormatter<_>>.MakeGenericType [| t.GetElementType () |]
        let factory = Activator.CreateInstance(formatterType, [| t :> obj |]) :?> IFormatterFactory
        factory.Create resolver