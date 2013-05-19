module internal FsCoreSerializer.BaseFormatters

    open System
    open System.Reflection
    open System.IO
    open System.Collections.Generic
    open System.Collections.Concurrent
    open System.Runtime.Serialization
    open System.Runtime.CompilerServices

    open FsCoreSerializer
    open FsCoreSerializer.Reflection
    open FsCoreSerializer.Utils

    module internal Utils =

        let containsAttr<'T when 'T :> Attribute> (m : MemberInfo) =
            m.GetCustomAttributes< ^T>() |> Seq.isEmpty |> not

        let fieldBindings = 
            BindingFlags.NonPublic ||| BindingFlags.Public ||| 
                BindingFlags.Instance ||| BindingFlags.FlattenHierarchy 

        let methodBindings =
            BindingFlags.NonPublic ||| BindingFlags.Public |||
                BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.FlattenHierarchy

        let ctorBindings = BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public

        let tryGetCtor (t : Type) (args : Type []) = denull <| t.GetConstructor(ctorBindings,null,args, [||]) 

        let allFlags = enum<BindingFlags> Int32.MaxValue &&& ~~~ BindingFlags.IgnoreCase

        let getTypeInfo (t : Type) =
            if t.IsPrimitive then TypeInfo.Primitive
            elif t.IsEnum then TypeInfo.Enum
            elif t.IsValueType then TypeInfo.Value
            elif t.IsArray then TypeInfo.Array
            elif t.IsSealed then TypeInfo.Sealed
            elif t.IsAbstract then TypeInfo.Abstract
            else TypeInfo.NonSealed

        let inline mkFormatter<'T> info useWithSubtypes cache (reader : Reader -> 'T) (writer : Writer -> 'T -> unit) =
            {
                Type = typeof<'T>
                Write = fun bw o -> writer bw (o :?> 'T)
                Read = fun br -> reader br :> obj

                TypeInfo = getTypeInfo typeof<'T>
                TypeHash = ObjHeader.getTruncatedHash typeof<'T>
                FormatterInfo = info

                CacheObj = cache
                UseWithSubtypes = useWithSubtypes
            }


        let inline write (w : Writer) (f : Formatter) (x : obj) =
            if f.TypeInfo <= TypeInfo.Value then f.Write w x
            elif not f.CacheObj then
                if obj.ReferenceEquals(x, null) then w.BW.Write true
                else
                    w.BW.Write false ; f.Write w x
            elif f.FormatterInfo = FormatterInfo.FSharpValue then
                if obj.ReferenceEquals(x, null) then w.BW.Write true
                else
                    w.BW.Write false
                    do RuntimeHelpers.EnsureSufficientExecutionStack()
                    f.Write w x
            else
                w.WriteObj(f, x)

        let inline read (r : Reader) (f : Formatter) =
            if f.TypeInfo <= TypeInfo.Value then f.Read r
            elif not f.CacheObj || f.FormatterInfo = FormatterInfo.FSharpValue then
                if r.BR.ReadBoolean() then null
                else f.Read r
            else
                r.ReadObj f

        let inline writeSeq (w : Writer) (ef : Formatter) (length : int) (xs : seq<'T>) =
            w.BW.Write length
            for x in xs do write w ef x

        let inline readSeq<'T> (r : Reader) (f : Formatter)  =
            let n = r.BR.ReadInt32()
            let a = Array.zeroCreate<'T> n
            for i = 0 to n - 1 do
                a.[i] <- read r f :?> 'T
            a

        /// length is fed as external argument to prevent unintented double evaluation of sequence
        let inline writeKVPair (w : Writer) (kf : Formatter) (vf : Formatter) (length : int) (xs : ('K * 'V) seq) =
            w.BW.Write length
            for k,v in xs do
                write w kf k
                write w vf v

        let inline readKVPair<'K,'V> (r : Reader) (kf : Formatter) (vf : Formatter) =
            let n = r.BR.ReadInt32()
            let a = Array.zeroCreate<'K * 'V> n
            for i = 0 to n - 1 do
                let k = read r kf :?> 'K
                let v = read r vf :?> 'V
                a.[i] <- (k,v)
            a

        let inline zipWrite (w : Writer) (formatters : Lazy<Formatter> []) (objs : obj []) : unit =
            for i = 0 to formatters.Length - 1 do
                write w formatters.[i].Value objs.[i]

        let inline zipRead (r : Reader) (formatters : Lazy<Formatter> []) : obj [] =
            let objs = Array.zeroCreate formatters.Length
            for i = 0 to formatters.Length - 1 do
                objs.[i] <- read r formatters.[i].Value

            objs


    open Utils

    let intPtrFmt, uIntPtrFmt =
        match sizeof<nativeint> with
        | 4 -> 
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadInt32() |> nativeint) (fun bw x -> bw.BW.Write(int32 x)),
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadUInt32() |> unativeint) (fun bw x -> bw.BW.Write(uint32 x))
        | 8 -> 
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadInt64() |> nativeint) (fun bw x -> bw.BW.Write(int64 x)),
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadUInt64() |> unativeint) (fun bw x -> bw.BW.Write(uint64 x))
        | _ -> failwith "w00t!"

    let primitiveFormatters =
        [   
            mkFormatter FormatterInfo.Atomic false false ignore (fun _ _ -> ())
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadByte()) (fun bw x -> bw.BW.Write x)
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadSByte()) (fun bw x -> bw.BW.Write x) 
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadChar()) (fun bw x -> bw.BW.Write x) 
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadBoolean()) (fun bw x -> bw.BW.Write x)
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadDecimal()) (fun bw x -> bw.BW.Write x)
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadSingle()) (fun bw x -> bw.BW.Write x) 
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadDouble()) (fun bw x -> bw.BW.Write x) 
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadInt16()) (fun bw x -> bw.BW.Write x) 
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadInt32()) (fun bw x -> bw.BW.Write x) 
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadInt64()) (fun bw x -> bw.BW.Write x) 
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadUInt16()) (fun bw x -> bw.BW.Write x) 
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadUInt32()) (fun bw x -> bw.BW.Write x) 
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadUInt64()) (fun bw x -> bw.BW.Write x) 
            intPtrFmt ; uIntPtrFmt
        ]


    let valueFormatters =
        [
            mkFormatter FormatterInfo.Atomic false false (fun br -> br.BR.ReadString()) (fun bw x -> bw.BW.Write x)
            mkFormatter FormatterInfo.Atomic false false (fun br -> Guid(br.BR.ReadBytes(16))) (fun bw x -> bw.BW.Write(x.ToByteArray()))
            mkFormatter FormatterInfo.Atomic false false (fun br -> TimeSpan(br.BR.ReadInt64())) (fun bw x -> bw.BW.Write(x.Ticks))
            mkFormatter FormatterInfo.Atomic false false (fun br -> DateTime(br.BR.ReadInt64())) (fun bw x -> bw.BW.Write(x.Ticks)) 
            mkFormatter FormatterInfo.Atomic false true (fun br -> br.BR.ReadBytes(br.BR.ReadInt32())) (fun bw x -> bw.BW.Write x.Length ; bw.BW.Write x) 
        ]


    //
    //  Reflection formatters
    //


    let typeFormatter =
        let writer (w : Writer) (t : Type) =
            if t.AssemblyQualifiedName = null then
                if t.IsGenericParameter then
                    w.BW.Write (TypeSerializer.Default.Write t.ReflectedType)
                    w.BW.Write true
                    w.BW.Write t.Name
                else
                    raise <| new SerializationException(sprintf "invalid type '%s'" t.Name)
            else
                w.BW.Write (TypeSerializer.Default.Write t)
                w.BW.Write false

        let reader (r : Reader) : Type =
            let t = TypeSerializer.Default.Read (r.BR.ReadString())
            if r.BR.ReadBoolean () then
                let name = r.BR.ReadString()
                try t.GetGenericArguments() |> Array.find(fun a -> a.Name = name)
                with :? KeyNotFoundException -> raise <| new SerializationException(sprintf "cannot deserialize type '%s'" t.Name)
            else t

        mkFormatter FormatterInfo.ReflectionType true true reader writer

    let assemblyFormatter =
        let writer (w : Writer) (a : Assembly) = w.BW.Write(a.FullName)
        let reader (r : Reader) = Assembly.Load(r.BR.ReadString())

        mkFormatter FormatterInfo.ReflectionType true true reader writer

    let memberFormatter =
        let writer (w : Writer) (m : MemberInfo) =
            write w typeFormatter m.DeclaringType
            match m with
            | :? MethodInfo as m when m.IsGenericMethod && not m.IsGenericMethodDefinition ->
                let gm = m.GetGenericMethodDefinition()
                let ga = m.GetGenericArguments()
                w.BW.Write (gm.ToString())

                w.BW.Write true
                w.BW.Write ga.Length
                for a in ga do write w typeFormatter a
            | _ ->
                w.BW.Write (m.ToString())
                w.BW.Write false

        let reader (r : Reader) =
            let t = read r typeFormatter :?> Type
            let mname = r.BR.ReadString()
            let m = t.GetMembers(allFlags) |> Array.find (fun m -> m.ToString() = mname)
            if r.BR.ReadBoolean() then
                let n = r.BR.ReadInt32()
                let ga = Array.zeroCreate<Type> n
                for i = 0 to n - 1 do ga.[i] <- read r typeFormatter :?> Type
                (m :?> MethodInfo).MakeGenericMethod ga :> MemberInfo
            else
                m

        mkFormatter FormatterInfo.ReflectionType true true reader writer

    let typeHandleFormatter =
        mkFormatter FormatterInfo.ReflectionType true true 
                (fun r -> let t = read r typeFormatter :?> Type in t.TypeHandle)
                (fun w th -> write w typeFormatter (Type.GetTypeFromHandle th))

    let fieldHandleFormatter =
        mkFormatter FormatterInfo.ReflectionType true true
                (fun r -> let f = read r memberFormatter :?> FieldInfo in f.FieldHandle)
                (fun w fh -> write w memberFormatter (FieldInfo.GetFieldFromHandle fh))

    let methodHandleFormatter =
        mkFormatter FormatterInfo.ReflectionType true true
                (fun r -> let m = read r memberFormatter :?> MethodInfo in m.MethodHandle)
                (fun w mh -> write w memberFormatter (MethodInfo.GetMethodFromHandle mh))

    let assemblyNameFormatter =
        mkFormatter FormatterInfo.ReflectionType true true
                (fun r -> AssemblyName(r.BR.ReadString()))
                (fun w a -> w.BW.Write a.FullName)

    let reflectionFormatters =
        [ 
            typeFormatter ; assemblyFormatter ; memberFormatter
            typeHandleFormatter ; fieldHandleFormatter ; methodHandleFormatter
            assemblyNameFormatter
        ]

    //
    //  .NET serialization handlers
    //

    let mkAbstractFormatter (t : Type) =
        {
            Type = t
            TypeInfo = getTypeInfo t
            TypeHash = ObjHeader.getTruncatedHash t

            Write = fun _ _ -> raise <| new NotSupportedException("cannot serialize an abstract type")
            Read = fun _ -> raise <| new NotSupportedException("cannot serialize an abstract type")

            FormatterInfo = FormatterInfo.ReflectionDerived
            UseWithSubtypes = false
            CacheObj = true
        }

    let tryMkISerializableFormatter (t : Type) =
        if typeof<ISerializable>.IsAssignableFrom t then
            match tryGetCtor t [| typeof<SerializationInfo> ; typeof<StreamingContext> |] with
            | None -> None
            | Some ctor ->
                let ctorEmit = FSharpValue.PreComputeConstructor ctor

                let allMethods = t.GetMethods(methodBindings) 
                let precompute (m : MethodInfo) = FSharpValue.PreComputeMethod(t, m)
                
                let onSerializing = allMethods |> Array.filter containsAttr<OnSerializingAttribute> |> Array.map precompute
                let onSerialized = allMethods |> Array.filter containsAttr<OnSerializedAttribute> |> Array.map precompute
                let onDeserialized = allMethods |> Array.filter containsAttr<OnDeserializedAttribute> |> Array.map precompute
                let isDeserializationCallback = typeof<IDeserializationCallback>.IsAssignableFrom t

                let inline run o (sc : StreamingContext) (fs : (obj * obj [] -> obj) []) =
                    for f in fs do f(o , [| sc :> obj |]) |> ignore

                let writer (w : Writer) (o : obj) =
                    run o w.StreamingContext onSerializing
                    let s = o :?> ISerializable
                    let sI = new SerializationInfo(t, new FormatterConverter())
                    s.GetObjectData(sI, w.StreamingContext)
                    w.BW.Write sI.MemberCount
                    let enum = sI.GetEnumerator()
                    while enum.MoveNext() do
                        w.BW.Write enum.Current.Name
                        w.WriteObj enum.Current.Value

                    run o w.StreamingContext onSerialized

                let reader (r : Reader) =
                    let sI = new SerializationInfo(t, new FormatterConverter())
                    let memberCount = r.BR.ReadInt32()
                    for i = 1 to memberCount do
                        let name = r.BR.ReadString()
                        let v = r.ReadObj ()
                        sI.AddValue(name, v)

                    let o = ctorEmit [| sI :> obj ; r.StreamingContext :> obj |]
                    run o r.StreamingContext onDeserialized
                    if isDeserializationCallback then (o :?> IDeserializationCallback).OnDeserialization null
                    o

                Some {
                    Type = t
                    TypeInfo = getTypeInfo t
                    TypeHash = ObjHeader.getTruncatedHash t

                    Write = writer
                    Read = reader

                    FormatterInfo = FormatterInfo.ISerializable
                    CacheObj = true
                    UseWithSubtypes = false
                }
        else None

    let tryMkIFsCoreSerializable (t : Type) =
        if typeof<IFsCoreSerializable>.IsAssignableFrom t then
            match tryGetCtor t [| typeof<Reader> |] with
            | None -> None
            | Some ctor ->
                let emittedCtor = FSharpValue.PreComputeConstructor ctor

                Some {
                    Type = t
                    TypeInfo = getTypeInfo t
                    TypeHash = ObjHeader.getTruncatedHash t

                    Write = fun (w : Writer) (o : obj) -> (o :?> IFsCoreSerializable).GetObjectData(w)
                    Read = fun (r : Reader) -> emittedCtor [| r :> obj |]

                    FormatterInfo = FormatterInfo.IFsCoreSerializable
                    UseWithSubtypes = false
                    CacheObj = true
                }
        else None

    let mkReflectionFormatter (resolver : Type -> Lazy<Formatter>) (t : Type) =
        if t.IsPrimitive then raise <| new SerializationException(sprintf "could not derive serialization rules for '%s'." t.Name)
        elif t.IsAbstract then mkAbstractFormatter t
        elif not t.IsSerializable then raise <| new SerializationException(sprintf "type '%s' is marked as nonserializable." t.Name) else
        
        let allMethods = t.GetMethods(methodBindings)
        let precompute (m : MethodInfo) = FSharpValue.PreComputeMethod(t, m)

        let onSerializing = allMethods |> Array.filter containsAttr<OnSerializingAttribute> |> Array.map precompute
        let onSerialized = allMethods |> Array.filter containsAttr<OnSerializedAttribute> |> Array.map precompute
        let onDeserializing = allMethods |> Array.filter containsAttr<OnDeserializingAttribute> |> Array.map precompute
        let onDeserialized = allMethods |> Array.filter containsAttr<OnDeserializedAttribute> |> Array.map precompute
        let isDeserializationCallback = typeof<IDeserializationCallback>.IsAssignableFrom t

        let inline run o (sc : StreamingContext) (fs : (obj * obj[] -> obj) []) =
            for f in fs do f (o, [|sc :> obj|]) |> ignore

        let fields = t.GetFields(fieldBindings) |> Array.filter (not << containsAttr<NonSerializedAttribute>)
        let formatters = fields |> Array.map (fun f -> resolver f.FieldType)
        let decomposer = FSharpValue.PreComputeFieldReader(t, fields)
        let composer = FSharpValue.PreComputeObjectInitializer(t, fields)

        let tyInfo = getTypeInfo t

        let writer (w : Writer) (o : obj) =
            run o w.StreamingContext onSerializing
            let values = decomposer o
            zipWrite w formatters values

            run o w.StreamingContext onSerialized

        let reader (r : Reader) =
            let o = FormatterServices.GetUninitializedObject(t)
            if tyInfo > TypeInfo.Value then r.EarlyRegisterObject o
            run o r.StreamingContext onDeserializing
            let values = zipRead r formatters

            do composer (o, values)

            run o r.StreamingContext onDeserialized
            if isDeserializationCallback then (o :?> IDeserializationCallback).OnDeserialization null
            o
        {
            Type = t
            TypeInfo = tyInfo
            TypeHash = ObjHeader.getTruncatedHash t

            Write = writer
            Read = reader

            FormatterInfo = FormatterInfo.ReflectionDerived
            UseWithSubtypes = false
            CacheObj = true
        }

    let mkEnumFormatter (resolver : Type -> Lazy<Formatter>) (t : Type) =
        let ut = Enum.GetUnderlyingType t
        let uf = (resolver ut).Value

        {
            Type = t
            TypeInfo = getTypeInfo t
            TypeHash = ObjHeader.getTruncatedHash t

            Write = uf.Write
            Read = uf.Read

            FormatterInfo = FormatterInfo.ReflectionDerived
            UseWithSubtypes = false
            CacheObj = false
        }