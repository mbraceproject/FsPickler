namespace FsCoreSerializer

    //  F# core types serialization; largely inspired by Anton Tayanovskyy's work (http://fssnip.net/6u)

    open System
    open System.IO
    open System.Reflection
    open System.Collections.Generic
    open System.Runtime.Serialization
    open System.Runtime.CompilerServices

    open Microsoft.FSharp.Reflection

    open FsCoreSerializer.Utils
    open FsCoreSerializer.Reflection

    type Formatter =
        {
            Type : Type

            Writer : Writer -> obj -> unit
            Reader : Reader -> obj

            CacheMode : CacheMode
            RequiresExternalSerializer : bool
            ContainsRecTypes : bool
            ValidForSubtypes : bool
        }

    and CacheMode =
        | NoCaching
        | CacheByRef
        | CacheByEquality

    // used for implementing generic types
    and IFormatterFactory =
        abstract member Create : unit -> Formatter

    and internal GenericFormatterDescriptor =
        {
            TargetKind : Type
            ImplKind : Type
        }


    and Writer internal (binaryWriter : BinaryWriter, externalWriter : obj -> unit, typeFormatter : Formatter, 
                                                runtimeResolver : Type -> Formatter option, ?context : obj) =

        do assert(typeFormatter.Type = typeof<Type>)
        
        let refIdx = new ObjectIDGenerator()
        let valIdx = new Dictionary<obj, int64> ()
        let mutable valCount = 0L

        member __.BW = binaryWriter
        member __.StreamingContext = context

        member w.Write (formatter : Formatter, o : obj) : unit =
            do RuntimeHelpers.EnsureSufficientExecutionStack()

            match formatter.CacheMode with
            | NoCaching -> formatter.Writer w o
            | CacheByRef ->
                let first = ref false
                let id = refIdx.HasId(o, first)
                if first.Value then
                    binaryWriter.Write true
                    formatter.Writer w o
                    refIdx.GetId(o, first) |> ignore
                else
                    binaryWriter.Write false
                    binaryWriter.Write id
            | CacheByEquality ->
                let v = ref 0L
                if valIdx.TryGetValue(o, v) then
                    binaryWriter.Write true
                    binaryWriter.Write v.Value
                else
                    binaryWriter.Write false
                    formatter.Writer w o

                    valIdx.Add(o, valCount)
                    valCount <- valCount + 1L

        member w.WriteObj (o : obj, resolveFormatter, cacheMode) =
            if o = null then binaryWriter.Write 0uy
            else
                let dynfmt =
                    if resolveFormatter then runtimeResolver (o.GetType())
                    else None

                match dynfmt with
                | Some fmt ->
                    binaryWriter.Write 1uy
                    w.Write(typeFormatter, fmt.Type)
                    w.Write(fmt, o)
                | None ->
                    match cacheMode with
                    | CacheByRef ->
                        let first = ref false
                        let id = refIdx.HasId(o, first)
                        if first.Value then
                            binaryWriter.Write 2uy
                            externalWriter o
                            refIdx.GetId(o, first) |> ignore
                        else
                            binaryWriter.Write 3uy
                            binaryWriter.Write id
                    | NoCaching -> 
                        binaryWriter.Write 4uy
                        externalWriter o
                    | CacheByEquality ->
                        let v = ref 0L
                        if valIdx.TryGetValue(o, v) then
                            binaryWriter.Write 5uy
                            binaryWriter.Write v.Value
                        else 
                            binaryWriter.Write 6uy
                            externalWriter o

                            valIdx.Add(o, valCount)
                            valCount <- valCount + 1L


    and Reader internal (binaryReader : BinaryReader, externalReader : unit -> obj, 
                            typeFormatter : Formatter, runtimeResolver : Type -> Formatter option, ?context : obj) =
        do assert(typeFormatter.Type = typeof<Type>)
        
        let refIdx = new Dictionary<int64, obj>()
        let mutable refCount = 1L

        let valIdx = new Dictionary<int64, obj> ()
        let mutable valCount = 0L

        member __.BR = binaryReader
        member __.StreamingContext = context

        member r.Read (formatter : Formatter) =
            match formatter.CacheMode with
            | NoCaching -> formatter.Reader r
            | CacheByRef ->
                if binaryReader.ReadBoolean() then
                    let o = formatter.Reader r
                    refIdx.Add(refCount, o)
                    refCount <- refCount + 1L
                    o
                else
                    let id = binaryReader.ReadInt64()
                    refIdx.[id]
            | CacheByEquality ->
                if binaryReader.ReadBoolean() then
                    let id = binaryReader.ReadInt64()
                    valIdx.[id]
                else
                    let o = formatter.Reader r
                    valIdx.Add(valCount, o)
                    valCount <- valCount + 1L
                    o

        member r.ReadObj () =
            match binaryReader.ReadByte() with
            | 0uy -> null
            | 1uy ->
                let t = r.Read typeFormatter :?> Type
                match runtimeResolver t with
                | None -> invalidOp "Invalid serialization."
                | Some fmt -> r.Read fmt
            | 2uy ->
                // deserialize, ref caching
                let o = externalReader ()
                refIdx.Add(refCount, o)
                refCount <- refCount + 1L
                o
            | 3uy ->
                // fetch ref cached object
                let id = binaryReader.ReadInt64()
                refIdx.[id]
            | 4uy ->
                // serialization without caching
                externalReader ()
            | 5uy ->
                // fetch eq cached object
                let id = binaryReader.ReadInt64()
                valIdx.[id]
            | 6uy ->
                // deserialize, eq caching
                let o = externalReader ()
                valIdx.Add(valCount, o)
                valCount <- valCount + 1L
                o
            | _ -> invalidOp "Invalid serialization."


    module internal FsCoreFormatterImpl =

        let allFlags = (enum<BindingFlags> Int32.MaxValue) &&& (~~~ BindingFlags.IgnoreCase)

        // create serializer out of typed lambas
        let mkFormatter cache reqExt subtypes recTypes (reader : Reader -> 'T) (writer : Writer -> 'T -> unit) =
            {
                Type = typeof<'T>

                Writer = fun w o -> writer w (o :?> 'T)
                Reader = fun r -> reader r :> obj
                
                CacheMode = cache
                RequiresExternalSerializer = reqExt
                ContainsRecTypes = recTypes
                ValidForSubtypes = subtypes

            }

        // untyped version of the above
        let mkFormatter2 (t : Type) cache reqExt subtypes recTypes (reader : Reader -> obj) (writer : Writer -> obj -> unit) =
            {
                Type = t

                Writer = writer
                Reader = reader

                CacheMode = cache
                RequiresExternalSerializer = reqExt
                ContainsRecTypes = recTypes
                ValidForSubtypes = subtypes
                
            }

        let inline write (w : Writer) (fmt : Formatter option) (o : obj) = 
            match fmt with
            | Some fmt -> w.Write(fmt, o)
            | None -> w.WriteObj(o, true, CacheByRef)

        let inline read (r : Reader) (fmt : Formatter option) =
            match fmt with
            | Some fmt -> r.Read fmt
            | None -> r.ReadObj()

        /// seq length must be declared externally to avoid double evaluations
        let inline writeSeq (ef : Formatter option) (w : Writer) (len : int) (xs : 'T seq) =
            w.BW.Write len
            for x in xs do write w ef x
    
        /// use with caution, should only evaluate returned sequence once
        let inline readSeq<'T> (ef : Formatter option) (r : Reader) =
            let len = r.BR.ReadInt32()
            seq { for _ in 1 .. len -> read r ef :?> 'T }

        /// seq length must be declared externally to avoid double evaluations
        let inline writeKVPair (kf : Formatter option) (vf : Formatter option) (w : Writer) (len : int) (xs : ('K * 'V) seq) =
            w.BW.Write len
            for k,v in xs do
                write w kf k
                write w vf v

        /// use with caution, should only evaluate returned sequence once
        let inline readKVPair<'K,'V> (kf : Formatter option) (vf : Formatter option) (r : Reader) =
            seq { 
                for _ in 1 .. r.BR.ReadInt32() ->
                    let k = read r kf :?> 'K
                    let v = read r vf :?> 'V
                    k,v
            }


        /// assuming formatter.Length = objs.Length
        let inline zipWrite (formatters : Formatter option cell []) (w : Writer) (objs : obj []) : unit =
            for i = 0 to formatters.Length - 1 do
                match formatters.[i].Value with
                | Some fmt when fmt.ContainsRecTypes -> 
                    do RuntimeHelpers.EnsureSufficientExecutionStack()
                    fmt.Writer w objs.[i]
                | fmt -> write w fmt objs.[i]

        let inline zipRead (formatters : Formatter option cell []) (r : Reader) : obj [] =
            let objs = Array.zeroCreate formatters.Length
            for i = 0 to formatters.Length - 1 do
                match formatters.[i].Value with
                | Some fmt when fmt.ContainsRecTypes -> objs.[i] <- fmt.Reader r
                | fmt -> objs.[i] <- read r fmt
            objs

        // primitive serializer definitions

        let primitives =
            [
                mkFormatter NoCaching false false false (fun _ -> ()) (fun _ _ -> ()) // unit serializer
                mkFormatter NoCaching false false false (fun r -> r.BR.ReadBoolean()) (fun w n -> w.BW.Write n)
                mkFormatter NoCaching false false false (fun r -> r.BR.ReadByte()) (fun w n -> w.BW.Write n)
                mkFormatter NoCaching false false false (fun r -> r.BR.ReadSByte()) (fun w n -> w.BW.Write n)
                mkFormatter NoCaching false false false (fun r -> r.BR.ReadByte()) (fun w n -> w.BW.Write n)
                mkFormatter NoCaching false false false (fun r -> r.BR.ReadChar()) (fun w n -> w.BW.Write n)
                mkFormatter NoCaching false false false (fun r -> r.BR.ReadDecimal()) (fun w n -> w.BW.Write n)
                mkFormatter NoCaching false false false (fun r -> r.BR.ReadSingle()) (fun w n -> w.BW.Write n)
                mkFormatter NoCaching false false false (fun r -> r.BR.ReadDouble()) (fun w n -> w.BW.Write n)
                mkFormatter NoCaching false false false (fun r -> r.BR.ReadInt16()) (fun w n -> w.BW.Write n)
                mkFormatter NoCaching false false false (fun r -> r.BR.ReadInt32()) (fun w n -> w.BW.Write n)
                mkFormatter NoCaching false false false (fun r -> r.BR.ReadInt64()) (fun w n -> w.BW.Write n)
                mkFormatter NoCaching false false false (fun r -> r.BR.ReadUInt16()) (fun w n -> w.BW.Write n)
                mkFormatter NoCaching false false false (fun r -> r.BR.ReadUInt32()) (fun w n -> w.BW.Write n)
                mkFormatter NoCaching false false false (fun r -> r.BR.ReadUInt64()) (fun w n -> w.BW.Write n)
                // a few additional primitives
                mkFormatter NoCaching false false false (fun r -> r.BR.ReadString()) (fun w b -> w.BW.Write b)
                mkFormatter NoCaching false false false (fun r -> Guid(r.BR.ReadBytes(sizeof<Guid>))) (fun w g -> w.BW.Write(g.ToByteArray()))
                mkFormatter NoCaching false false false (fun r -> DateTime(r.BR.ReadInt64())) (fun w d -> w.BW.Write(d.Ticks))
                // primitive arrays
                mkFormatter CacheByRef false false false (fun r -> let n = r.BR.ReadInt32() in r.BR.ReadBytes(n)) 
                                                            (fun w bs -> w.BW.Write bs.Length ; w.BW.Write bs)
            ]


        //
        // F# core types
        //

        // interpret "None" as external serializer
        let inline requiresExtern (fmt : Formatter option cell) = 
            fmt.Value |> Option.forall (fun f -> f.RequiresExternalSerializer)

        let inline containsRectype (fmt : Formatter option cell) =
            fmt.Value |> Option.exists (fun f -> f.ContainsRecTypes)

        let mkUnionFormatter (resolver : Type -> Formatter option cell) cache (t : Type) =
            let union = FsUnion.Create(t, allFlags)

            let reqExt = ref false
            let recTyp = ref false

            let getBranchingFormatters (uci : UnionCaseInfo) =
                let formatters =
                    uci.GetFields() 
                    |> Array.map 
                        (fun f -> 
                            let fmt = resolver f.PropertyType
                            reqExt := reqExt.Value || requiresExtern fmt
                            recTyp := recTyp.Value || containsRectype fmt
                            fmt)

                uci.Tag, formatters
            
            let fmtMap = union.UCIs |> Seq.map getBranchingFormatters |> Map.ofSeq

            let writer (w : Writer) (o : obj) =
                let tag, fields = union.Decompose o
                w.BW.Write tag
                zipWrite fmtMap.[tag] w fields

            let reader (r : Reader) =
                let tag = r.BR.ReadInt32()
                let fields = zipRead fmtMap.[tag] r
                union.Compose(tag, fields)

            mkFormatter2 union.DeclaringType cache reqExt.Value true recTyp.Value reader writer

        let mkRecordFormatter (resolver : Type -> Formatter option cell) cache (t : Type) =
            let record = FsRecord.Create(t, allFlags)

            let reqExt = ref false
            let recTyp = ref false

            let formatters = 
                record.Fields |> Array.map (fun f -> 
                                                let fmt = resolver f.PropertyType
                                                reqExt := reqExt.Value || requiresExtern fmt
                                                recTyp := recTyp.Value || containsRectype fmt
                                                fmt)

            let writer (w : Writer) (o : obj) =
                let fields = record.Decompose o
                zipWrite formatters w fields

            let reader (r : Reader) =
                let fields = zipRead formatters r
                record.Compose fields

            mkFormatter2 record.DeclaringType cache reqExt.Value false recTyp.Value reader writer

        let mkTupleFormatter (resolver : Type -> Formatter option cell) cache (t : Type) =
            let tuple = FsTuple.Create t

            let reqExt = ref false
            let recTyp = ref false
            let formatters =
                tuple.Elements |> Array.map (fun t -> 
                                                let fmt = resolver t
                                                reqExt := reqExt.Value || requiresExtern fmt
                                                recTyp := recTyp.Value || containsRectype fmt
                                                fmt)

            let writer (w : Writer) (o : obj) =
                let fields = tuple.Decompose o
                zipWrite formatters w fields

            let reader (r : Reader) =
                let fields = zipRead formatters r
                tuple.Compose fields

            mkFormatter2 t cache reqExt.Value false recTyp.Value reader writer

        let mkExceptionFormatter (resolver : Type -> Formatter option cell) cache (t : Type) =
            let exn = FsException.Create(t, allFlags)

            let reqExt = ref false
            let recTyp = ref false
            let formatters =
                exn.Fields |> Array.map (fun f -> 
                                                let fmt = resolver f.PropertyType
                                                reqExt := reqExt.Value || requiresExtern fmt
                                                recTyp := recTyp.Value || containsRectype fmt
                                                fmt)

            let writer (w : Writer) (o : obj) =
                let fields = exn.Decompose o
                zipWrite formatters w fields

            let reader (r : Reader) =
                let fields = zipRead formatters r
                exn.Compose fields

            mkFormatter2 t cache reqExt.Value false recTyp.Value reader writer

        let mkFuncFormatter (ctorFormatter : Formatter) (t : Type) =
            let ctorInfo = t.GetConstructors(allFlags) |> Array.tryFind(fun ctor -> ctor.GetParameters().Length = 0)
            match t.IsAbstract, ctorInfo with
            | true, _ | _, None ->
                // abstract funcs need dynamic constructor resolution
                let writer (w : Writer) (o : obj) =
                    let ctorInfo = 
                        if o = null then None
                        else o.GetType().GetConstructors(allFlags) |> Array.tryFind(fun ctor -> ctor.GetParameters().Length = 0)

                    match ctorInfo with
                    | None -> w.BW.Write 0uy ; w.WriteObj(o, false, NoCaching)
                    | Some ctorInfo -> w.BW.Write 1uy; w.Write(ctorFormatter, ctorInfo)
                    
                let reader (r : Reader) =
                    match r.BR.ReadByte() with
                    | 0uy -> r.ReadObj()
                    | 1uy -> 
                        let ctorInfo = r.Read ctorFormatter :?> ConstructorInfo
                        ctorInfo.Invoke [||]
                    | _ -> failwith "failed to deserialize lambda."
                
                mkFormatter2 t CacheByRef false false false reader writer
            | false, Some ctorInfo ->
                let dynamic = FSharpValue.PreComputeConstructor ctorInfo
                
                mkFormatter2 t CacheByRef false false false (fun _ -> dynamic [||]) (fun _ _ -> ())

        //
        //  Generic Serializer helper funcs
        //

        let mkGenericFormatterDescr (targetKind : Type) (implKind : Type) =
            if not (implKind.IsGenericType && targetKind.IsGenericType) then
                invalidArg "Invalid generic formatter." implKind.Name
            elif implKind.GetGenericArguments().Length <> targetKind.GetGenericArguments().Length then
                invalidArg "Invalid implementation kind." implKind.Name
            elif not (implKind.GetInterfaces() |> Seq.exists ((=) typeof<IFormatterFactory>)) then
                invalidArg "Type does not implement IFormatterFactory." implKind.Name
            elif implKind.GetConstructor [| typeof<Type -> Formatter option cell> ; typeof<Type> |] = null then
                invalidArg "Type does contain required constructor signature." implKind.Name

            { TargetKind = targetKind; ImplKind = implKind }

        let callGenericFormatter (resolver : Type -> Formatter option cell) (implKind : Type) (t : Type) (tyParams : Type [] option) =
            let tyParams =
                match tyParams with
                | None -> t.GetGenericArguments()
                | Some tp -> tp

            let impl = implKind.MakeGenericType tyParams
            let formatter = System.Activator.CreateInstance(impl, [| resolver :> obj ; t :> obj |]) :?> IFormatterFactory

            formatter.Create ()

        //
        //  F# Core Generic type formatters
        //

        let bf = lazy(new System.Runtime.Serialization.Formatters.Binary.BinaryFormatter())

        type ArrayFormatter<'T>(resolver : Type -> Formatter option cell, t : Type) =
            do assert(t.IsArray && t.GetElementType() = typeof<'T> )

            static let copy (src : Array) (dst : Array) =
                Buffer.BlockCopy(src,0,dst,0,Buffer.ByteLength(src))

            interface IFormatterFactory with
                member __.Create () =
                    let et = t.GetElementType()
                    let ef = resolver et
                    let rank = t.GetArrayRank()

                    let writer (w : Writer) (o : obj) =
                        let arr = o :?> Array
                        let useBf = et.IsValueType && arr.Length > 100
                        do w.BW.Write useBf

                        if useBf && rank = 1 then bf.Value.Serialize(w.BW.BaseStream, o) else    
                        
                        for d in 0 .. rank - 1 do
                            w.BW.Write (arr.GetLength d)

                        if useBf then
                            let arr1D = Array.zeroCreate<'T> arr.Length
                            do copy arr arr1D
                            bf.Value.Serialize(w.BW.BaseStream, arr1D)
                        else
                            let ef = ef.Value
                            let inline write o = write w ef o
                            
                            match rank with
                            | 1 ->
                                let arr = o :?> 'T []
                                for i = 0 to arr.Length - 1 do
                                    write arr.[i]
                            | 2 -> 
                                let arr = o :?> 'T [,]
                                for i = 0 to arr.GetLength(0) - 1 do
                                    for j = 0 to arr.GetLength(1) - 1 do
                                        write arr.[i,j]
                            | 3 ->
                                let arr = o :?> 'T [,,]
                                for i = 0 to arr.GetLength(0) - 1 do
                                    for j = 0 to arr.GetLength(1) - 1 do
                                        for k = 0 to arr.GetLength(2) - 1 do
                                            write arr.[i,j,k]
                            | 4 ->
                                let arr = o :?> 'T [,,,]
                                for i = 0 to arr.GetLength(0) - 1 do
                                    for j = 0 to arr.GetLength(1) - 1 do
                                        for k = 0 to arr.GetLength(2) - 1 do
                                            for l = 0 to arr.GetLength(3) - 1 do
                                                write arr.[i,j,k,l]
                            | _ -> failwith "impossible array rank"

                    let reader (r : Reader) =
                        let useBf = r.BR.ReadBoolean()
                        if useBf && rank = 1 then bf.Value.Deserialize r.BR.BaseStream else

                        let l = [ for _ in 1 .. rank -> r.BR.ReadInt32 () ]

                        if useBf then
                            let src = bf.Value.Deserialize r.BR.BaseStream :?> Array
                            let dst =
                                match rank with
                                | 2 -> Array2D.zeroCreate<'T> l.[0] l.[1] :> Array
                                | 3 -> Array3D.zeroCreate<'T> l.[0] l.[1] l.[2] :> Array
                                | 4 -> Array4D.zeroCreate<'T> l.[0] l.[1] l.[2] l.[3] :> Array
                                | _ -> failwith "impossible array rank"

                            do copy src dst
                            dst :> obj
                        else
                            let ef = ef.Value
                            let inline reader () = read r ef :?> 'T

                            match rank with
                            | 1 -> 
                                let arr = Array.zeroCreate l.[0] : 'T []
                                for i = 0 to l.[0] - 1 do
                                    arr.[i] <- reader ()
                                arr :> obj
                            | 2 -> 
                                let arr = Array2D.zeroCreate l.[0] l.[1] : 'T [,]
                                for i = 0 to l.[0] - 1 do
                                    for j = 0 to l.[1] - 1 do
                                        arr.[i,j] <- reader ()
                                arr :> _
                            | 3 ->
                                let arr = Array3D.zeroCreate l.[0] l.[1] l.[2] : 'T [,,]
                                for i = 0 to l.[0] - 1 do
                                    for j = 0 to l.[1] - 1 do
                                        for k = 0 to l.[2] - 1 do
                                            arr.[i,j,k] <- reader ()
                                arr :> _
                            | 4 ->
                                let arr = Array4D.zeroCreate l.[0] l.[1] l.[2] l.[3] : 'T [,,,]
                                for i = 0 to l.[0] - 1 do
                                    for j = 0 to l.[1] - 1 do
                                        for k = 0 to l.[2] - 1 do
                                            for l = 0 to l.[3] - 1 do
                                                arr.[i,j,k,l] <- reader ()
                                arr :> _
                            | _ -> failwith "impossible array rank"

                    mkFormatter2 t CacheByRef (requiresExtern ef) false (containsRectype ef) reader writer

        type KeyValueArrayFormatter<'K, 'V> (resolver : Type -> Formatter option cell, t : Type) =
            do assert(t = typeof<('K * 'V) []>)

            interface IFormatterFactory with
                member __.Create () =
                    let et = t.GetElementType().GetGenericArguments()
                    let kf, vf = resolver et.[0], resolver et.[1]

                    let writer (w : Writer) (o : obj) =
                        let arr = o :?> ('K * 'V) []
                        writeKVPair kf.Value vf.Value w arr.Length arr

                    let reader (r : Reader) =
                        readKVPair<'K,'V> kf.Value vf.Value r |> Array.ofSeq :> obj

                    mkFormatter2 t CacheByRef (requiresExtern kf || requiresExtern vf) false (containsRectype kf || containsRectype vf) reader writer

        let mkArrayFormatter (resolver : Type -> Formatter option cell) (t : Type) =
            let et = t.GetElementType()
            let gt = if et.IsGenericType then Some <| et.GetGenericTypeDefinition() else None

            match gt with
            | Some gt when gt = typedefof<_ * _> ->
                let ga = et.GetGenericArguments()
                callGenericFormatter resolver typedefof<KeyValueArrayFormatter<_,_>> t (Some ga)
            | _ ->
                callGenericFormatter resolver typedefof<ArrayFormatter<_>> t (Some [|et|])



        type ListFormatter<'T> (resolver : Type -> Formatter option cell, t : Type) =
            do assert(t = typeof<'T list>)

            interface IFormatterFactory with
                member __.Create () =
                    let et = t.GetGenericArguments().[0]
                    let ef = resolver et

                    let writer (w : Writer) (o : obj) =
                        let l = o :?> 'T list
                        let n = l.Length
                        if et.IsValueType && n > 100 then
                            w.BW.Write true
                            bf.Value.Serialize(w.BW.BaseStream, Array.ofList l)
                        else
                            w.BW.Write false
                            writeSeq ef.Value w n l

                    let reader (r : Reader) =
                        if r.BR.ReadBoolean() then
                            bf.Value.Deserialize(r.BR.BaseStream) :?> 'T [] 
                            |> Array.toList :> obj
                        else
                            readSeq<'T> ef.Value r |> Seq.toList :> obj

                    mkFormatter2 t CacheByRef (requiresExtern ef) false (containsRectype ef) reader writer

        type KeyValueListFormatter<'K, 'V> (resolver : Type -> Formatter option cell, t : Type) =
            do assert(t = typeof<('K * 'V) list>)

            interface IFormatterFactory with
                member __.Create () =
                    let ga = t.GetGenericArguments().[0].GetGenericArguments()
                    let kf, vf = resolver ga.[0], resolver ga.[1]
                    
                    let writer (w : Writer) (o : obj) = 
                        let l = o :?> ('K * 'V) list 
                        writeKVPair kf.Value vf.Value w l.Length l

                    let reader (r : Reader) = readKVPair<'K, 'V> kf.Value vf.Value r |> Seq.toList :> obj

                    mkFormatter2 t CacheByRef (requiresExtern kf || requiresExtern vf) false (containsRectype kf || containsRectype vf) reader writer

        let mkListFormatter (resolver : Type -> Formatter option cell) (t : Type) =
            let et = t.GetGenericArguments().[0]
            let gt = if et.IsGenericType then Some <| et.GetGenericTypeDefinition() else None

            match gt with
            | Some gt when gt = typedefof<_ * _> ->
                let ga = et.GetGenericArguments()
                callGenericFormatter resolver typedefof<KeyValueListFormatter<_,_>> t (Some ga)
            | _ ->
                callGenericFormatter resolver typedefof<ListFormatter<_>> t (Some [|et|])
            
        //
        // F# core generic types
        //

        type OptionFormatter<'T> (resolver : Type -> Formatter option cell, t : Type) =
            do assert(t = typeof<'T option>)

            interface IFormatterFactory with
                member __.Create () =
                    let ef = resolver (t.GetGenericArguments().[0])

                    let writer (w : Writer) (o : obj) =
                        match o :?> 'T option with
                        | None -> w.BW.Write false
                        | Some x -> w.BW.Write true; write w ef.Value x

                    let reader (r : Reader) =
                        if r.BR.ReadBoolean() then
                            Some(read r ef.Value :?> 'T) :> obj
                        else None :> obj

                    mkFormatter2 t NoCaching (requiresExtern ef) false (containsRectype ef) reader writer

        let optionFormatter = mkGenericFormatterDescr typedefof<_ option> typedefof<OptionFormatter<_>>

        type SetFormatter<'T when 'T : comparison> (resolver : Type -> Formatter option cell, t : Type) =
            do assert(t = typeof<Set<'T>>)

            interface IFormatterFactory with
                member __.Create () =
                    let ef = resolver (t.GetGenericArguments().[0])

                    let writer (w : Writer) (o : obj) = 
                        let s = o :?> Set<'T>
                        writeSeq ef.Value w s.Count s

                    let reader (r : Reader) = readSeq<'T> ef.Value r |> Set.ofSeq |> box

                    mkFormatter2 t CacheByRef (requiresExtern ef) false (containsRectype ef) reader writer

        let setFormatter = mkGenericFormatterDescr typedefof<Set<_>> typedefof<SetFormatter<_>>

        type MapFormatter<'K, 'V when 'K : comparison> (resolver : Type -> Formatter option cell, t : Type) =
            do assert(t = typeof<Map<'K,'V>>)

            interface IFormatterFactory with
                member __.Create () =
                    let ga = t.GetGenericArguments()
                    let kf, vf = resolver ga.[0], resolver ga.[1]

                    let writer (w : Writer) (o : obj) =
                        let m = o :?> Map<'K,'V>
                        writeKVPair kf.Value vf.Value w m.Count (Map.toSeq m)

                    let reader (r : Reader) =
                        readKVPair<'K,'V> kf.Value vf.Value r |> Map.ofSeq :> obj

                    mkFormatter2 t CacheByRef (requiresExtern kf || requiresExtern vf) false (containsRectype kf || containsRectype vf) reader writer

        let mapFormatter = mkGenericFormatterDescr typedefof<Map<_,_>> typedefof<MapFormatter<_,_>>

        type DictionaryFormatter<'K, 'V when 'K : comparison> (resolver : Type -> Formatter option cell, t : Type) =
            do assert(t = typeof<Dictionary<'K,'V>>)

            interface IFormatterFactory with
                member __.Create () =
                    let ga = t.GetGenericArguments()
                    let kf, vf = resolver ga.[0], resolver ga.[1]

                    let writer (w : Writer) (o : obj) =
                        let d = o :?> Dictionary<'K, 'V>
                        let kvs = Seq.map (fun (KeyValue (k,v)) -> k,v) d
                        writeKVPair kf.Value vf.Value w d.Count kvs
                    let reader (r : Reader) =
                        let kvs = readKVPair<'K,'V> kf.Value vf.Value r
                        let d = new Dictionary<'K,'V>()
                        for k,v in kvs do d.Add(k,v)
                        d :> obj

                    mkFormatter2 t CacheByRef (requiresExtern kf || requiresExtern vf) false (containsRectype kf || containsRectype vf) reader writer

        let dictFormatter = mkGenericFormatterDescr typedefof<Dictionary<_,_>> typedefof<DictionaryFormatter<_,_>>

        let genericFormatters = [ optionFormatter ; setFormatter ; mapFormatter ; dictFormatter ]

        let mkGenericFormatterEntries (inp : GenericFormatterDescriptor seq) = 
            inp |> Seq.map (fun gfd  -> gfd.TargetKind.AssemblyQualifiedName, gfd) |> Map.ofSeq

        let mkFormatterCache (serializers : Formatter seq) =
            serializers |> Seq.map (fun s -> s.Type.AssemblyQualifiedName, Some s) |> Map.ofSeq

        //
        //  dynamic serializer resolution
        //

        let tryResolveFormatter memberInfoFormatter 
                                (genericCache : Map<string, GenericFormatterDescriptor>) 
                                (globalCache : Map<string, Formatter option>) cacheMode =

            // dummy initial formatter to found recursion upon
            let initial t =
                Some <|
                mkFormatter2 t NoCaching false false true (fun _ -> failwith "Invalid serializer resolution.")
                                                        (fun _ _ -> failwith "Invalid serializer resolution.")

            YParametric initial (fun (self : Type -> Formatter option cell) cont t ->
                // cache resolution
                match globalCache.TryFind t.AssemblyQualifiedName with
                | Some r -> cont r
                | None ->

                // formatter not in cache, go ahead with resolution
                // for starters, look for known generic types

                let result =
                    if t.IsGenericType then
                        let gt = t.GetGenericTypeDefinition()
                        if gt = typedefof<_ list> then
                            mkListFormatter self t |> Some
                        else
                            match genericCache.TryFind gt.AssemblyQualifiedName with
                            | None -> None
                            | Some gfd -> callGenericFormatter self gfd.ImplKind t None |> Some
                    else None

                // if that fails, resolve F# core types

                let result =
                    match result with
                    | Some _ -> result
                    | None ->
                        if t.IsArray then
                            mkArrayFormatter self t |> Some
                        elif FSharpType.IsTuple t then
                            mkTupleFormatter self cacheMode t |> Some
                        elif FSharpType.IsUnion(t, allFlags) then
                            mkUnionFormatter self cacheMode t |> Some
                        elif FSharpType.IsRecord(t, allFlags) then
                            mkRecordFormatter self cacheMode t |> Some
                        elif FSharpType.IsExceptionRepresentation(t, allFlags) then
                            mkExceptionFormatter self cacheMode t |> Some
                        elif FSharpType.IsFunction t then
                            mkFuncFormatter memberInfoFormatter t |> Some
                        else None

                // finally, look for formatters in subtypes

                let result =
                    match result with
                    | None when t.BaseType <> null ->
                        match (self t.BaseType).Value with
                        | Some fmt when fmt.ValidForSubtypes -> Some fmt
                        | _ -> None
                    | r -> r
                
                // commit result
                cont result)


    [<AutoOpen>]
    module FormatterExtensions =
        
        type Formatter with
            static member Create(writer : Writer -> 'T -> unit, reader : Reader -> 'T, 
                                                ?requiresExternalSerializer, ?useWithSubtypes, ?cacheMode) = 
                let requiresExternalSerializer = defaultArg requiresExternalSerializer false
                let cacheMode = defaultArg cacheMode CacheByRef
                let useWithSubtypes = defaultArg useWithSubtypes false
                FsCoreFormatterImpl.mkFormatter cacheMode requiresExternalSerializer useWithSubtypes false reader writer


        [<RequireQualifiedAccess>]
        module Writer =
            let inline writeSeq (w : Writer) (fmt : Formatter) (inputs : seq<'T>) =
                w.BW.Write (Seq.length inputs)
                for e in inputs do
                    w.Write(fmt, e)

            let inline zipWrite (w : Writer) (fmts : Formatter []) (objs : obj []) =
                for fmt, o in Seq.zip fmts objs do
                    w.Write(fmt, o)

        [<RequireQualifiedAccess>]
        module Reader =
            /// warning: make sure output sequence is evaluated only once!
            let inline readSeq<'T> (r : Reader) (fmt : Formatter) =
                seq { for _ in 1 .. r.BR.ReadInt32() -> r.Read fmt :?> 'T }

            let inline zipRead (r : Reader) (fmts : Formatter []) : obj [] =
                let n = fmts.Length
                let arr = Array.zeroCreate n
                for i = 0 to n - 1 do
                    arr.[i] <- r.Read fmts.[i]
                arr
