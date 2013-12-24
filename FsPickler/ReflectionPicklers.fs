module internal FsPickler.ReflectionPicklers

    open System
    open System.Globalization
    open System.IO
    open System.Text
    open System.Reflection
    open System.Runtime.Serialization
    open System.Collections.Generic
    open System.Collections.Concurrent

    open FsPickler
    open FsPickler.Utils
    open FsPickler.PicklerUtils


    /// Contains breakdown information for a MemberInfo instance
    /// This information can be memoized for performance and
    /// is sufficient to restructure the instance at deserialization.

    type CompositeMemberInfo =
        // System.Type breakdown
        | NamedType of string * AssemblyInfo
        | ArrayType of Type * (* rank *) int option
        | GenericType of Type * Type []
        | GenericTypeParam of Type * int
        | GenericMethodParam of MethodInfo * int
        // System.MethodInfo breakdown
        | Method of Type * (* name *) string * (* isStatic *) bool * Type []
        | GenericMethod of MethodInfo * Type []
        // in generic method definitions, encode as signature
        // this is to avoid recursive situations in method overload resolutions
        | GenericMethodDefinition of Type * (* name *) string * (* isStatic *) bool * (*signature*) string
        // misc MemberInfo
        | Constructor of Type * Type []
        | Property of Type * (* name *) string * (* isStatic *) bool
        | Field of Type * (* name *) string * (* isStatic *) bool
        | Event of Type * (* name *) string * (* isStatic *) bool
        | Unknown of Type * string


    //
    //  Assembly Loading Code
    //

    let getAssemblyInfo (a : Assembly) =
        let an = a.GetName()
        {
            Name = an.Name
            Version = an.Version.ToString()
            Culture = an.CultureInfo.ToString()
            PublicKeyToken = an.GetPublicKeyToken()
        }

    let loadAssembly useStrongNames (aI : AssemblyInfo) =
        let an = new AssemblyName()

        an.Name <- aI.Name

        if useStrongNames then

            match aI.Version with
            | null -> ()
            | version -> an.Version <- new Version(version)
                
            match aI.Culture with
            | null -> ()
            | culture -> an.CultureInfo <- new CultureInfo(culture)

            match aI.PublicKeyToken with
            | null -> an.SetPublicKeyToken [||]
            | pkt -> an.SetPublicKeyToken(pkt)

        try Assembly.Load(an)
        with :? FileNotFoundException | :? FileLoadException as e ->
            
            // Assembly.Load fails in certain cases where assemblies are loaded at runtime
            // attempt to resolve this by performing a direct query on the AppDomain 
                
            let isPartialMatch (an : AssemblyName) (a : Assembly) =
                let an' = a.GetName()
                if an.Name <> an'.Name then false
                elif an.Version <> null && an.Version <> an'.Version then false
                elif an.CultureInfo <> null && an.CultureInfo <> an'.CultureInfo then false
                else
                    match an.GetPublicKeyToken() with
                    | null | [||] -> true
                    | pkt -> pkt = an'.GetPublicKeyToken()

            let result =
                System.AppDomain.CurrentDomain.GetAssemblies()
                |> Array.tryFind (isPartialMatch an)

            match result with
            | None -> raise <| new SerializationException("FsPickler: Assembly load exception.", e)
            | Some a -> a


    //
    //  MemberInfo Loading Code
    //

    let getFlags isStatic =
        let allVisibility = BindingFlags.Public ||| BindingFlags.NonPublic
        allVisibility ||| (if isStatic then BindingFlags.Static else BindingFlags.Instance)

    // qualified name generator that emulates Type.ToString()
    let generateQualifiedName (getInfo : Type -> CompositeMemberInfo) (t : Type) =
        let rec generate (b : StringBuilder) (t : Type) =
            let inline append (x : string) = b.Append x |> ignore
            match getInfo t with
            | NamedType(name, _) -> append name
            | ArrayType(et, rk) ->
                generate b et
                match rk with
                | None -> append "[]"
                | Some 1 -> append "[*]"
                | Some r ->
                    append "["
                    for i = 1 to r - 1 do append ","
                    append "]"

            | GenericType(gt, tyParams) ->
                generate b gt
                append "["
                for i = 0 to tyParams.Length - 1 do
                    generate b tyParams.[i]
                    if i < tyParams.Length - 1 then append ","
                append "]"

            | GenericTypeParam _
            | GenericMethodParam _ -> append t.Name
            | _ -> failwith "not a type."

        let b = new StringBuilder()
        generate b t
        b.ToString()

    // print type signature for a generic method definition
    let getGenericMethodSignature (getInfo : Type -> CompositeMemberInfo) (m : MethodInfo) =
        let sb = new StringBuilder()
        let inline append (x : string) = sb.Append x |> ignore

        let rt = generateQualifiedName getInfo m.ReturnType
        let tyParams = m.GetGenericArguments() |> Array.map (fun p -> p.Name)
        let mParams = m.GetParameters () |> Array.map (fun p -> generateQualifiedName getInfo p.ParameterType)

        append rt
        append " "
        append m.Name
        
        append "["
        for i = 0 to tyParams.Length - 1 do
            append <| tyParams.[i]
            if i < tyParams.Length - 1 then
                append ","
        append "]"

        append "("
        for i = 0 to mParams.Length - 1 do
            append <| mParams.[i]
            if i < tyParams.Length - 1 then
                append ","
        append ")"

        sb.ToString()
        
    // converts a memberInfo instance to a CompositeMemberInfo structure
    let rec getMemberInfo (tyConv : ITypeNameConverter option) (getAssemblyInfo : Assembly -> AssemblyInfo) (m : MemberInfo) =
        match m with
        | :? Type as t ->
            if t.IsArray then
                let et = t.GetElementType()
                let rk = t.GetArrayRank()
                if rk = 1 && et.MakeArrayType() = t then
                    ArrayType(et, None)
                else
                    ArrayType (et, Some rk)
            elif t.IsGenericType && not t.IsGenericTypeDefinition then
                let gt = t.GetGenericTypeDefinition()
                let gas = t.GetGenericArguments()
                GenericType(gt, gas)
            elif t.IsGenericParameter then
                match t.DeclaringMethod with
                | null -> GenericTypeParam(t.DeclaringType, t.GenericParameterPosition)
                | dm -> GenericMethodParam(dm :?> MethodInfo, t.GenericParameterPosition)
            else
                let name = 
                    match t.FullName with
                    | null -> t.Name
                    | name -> name
                let aI = getAssemblyInfo t.Assembly
                match tyConv with
                | None -> NamedType(name, aI)
                | Some tc ->
                    let tI = tc.OfSerializedType <| aI.GetType(name)
                    NamedType(tI.Name, tI.Assembly)

        | :? MethodInfo as m ->
            if m.IsGenericMethod then
                if m.IsGenericMethodDefinition then
                    let signature = getGenericMethodSignature (getMemberInfo tyConv getAssemblyInfo) m
                    GenericMethodDefinition(m.DeclaringType, m.Name, m.IsStatic, signature)
                else
                    let gm = m.GetGenericMethodDefinition()
                    let tyArgs = m.GetGenericArguments()
                    GenericMethod(gm, tyArgs)
            else
                let mParams = m.GetParameters() |> Array.map (fun p -> p.ParameterType)
                Method(m.DeclaringType, m.Name, m.IsStatic, mParams)

        | :? ConstructorInfo as ctor ->
            let dt = ctor.DeclaringType 
            let ps = ctor.GetParameterTypes()
            Constructor(dt, ps)

        | :? PropertyInfo as p ->
            let dt = p.DeclaringType
            let isStatic = let m = p.GetGetMethod(true) in m.IsStatic
            Property(dt, p.Name, isStatic)

        | :? FieldInfo as f ->
            let dt = f.DeclaringType
            Field(dt, f.Name, f.IsStatic)

        | :? EventInfo as e ->
            let dt = e.DeclaringType
            let isStatic = let m = e.GetAddMethod() in m.IsStatic
            Event(dt, e.Name, isStatic)

        | _ -> Unknown (m.GetType(), m.ToString())



    let loadMemberInfo (tyConv : ITypeNameConverter option) (getAssemblyInfo : Assembly -> AssemblyInfo)
                                                        (loadAssembly : AssemblyInfo -> Assembly) 
                                                        (mI : CompositeMemberInfo) =
        match mI with
        | NamedType(name, aI) ->
            let name, aI = 
                match tyConv with
                | None -> name, aI
                | Some tc ->
                    let tI = tc.ToDeserializedType <| aI.GetType name
                    tI.Name, tI.Assembly

            let assembly = loadAssembly aI
            try assembly.GetType(name, throwOnError = true) |> fastUnbox<MemberInfo>
            with e ->
                raise <| new SerializationException("FsPickler: Type load exception.", e)

        | ArrayType(et, rk) -> 
            match rk with
            | None -> et.MakeArrayType() |> fastUnbox<MemberInfo>
            | Some r -> et.MakeArrayType(r) |> fastUnbox<MemberInfo>

        | GenericType(dt, tyArgs) -> dt.MakeGenericType tyArgs |> fastUnbox<MemberInfo>
        | GenericTypeParam(dt, idx) -> dt.GetGenericArguments().[idx] |> fastUnbox<MemberInfo>
        | GenericMethodParam(dm, idx) -> dm.GetGenericArguments().[idx] |> fastUnbox<MemberInfo>
        | Method(dt, name, isStatic, mParams) ->  dt.GetMethod(name, getFlags isStatic, null, mParams, [||]) |> fastUnbox<MemberInfo>
        | GenericMethod(gm, tyParams) -> gm.MakeGenericMethod tyParams |> fastUnbox<MemberInfo>
        | GenericMethodDefinition(dt, name, isStatic, signature) ->
            let isMatchingGenericMethod (m : MethodInfo) =
                if m.Name = name && m.IsGenericMethodDefinition then
                    let signature' = getGenericMethodSignature (getMemberInfo tyConv getAssemblyInfo) m
                    signature = signature'
                else false

            try dt.GetMethods(getFlags isStatic) |> Array.find isMatchingGenericMethod |> fastUnbox<MemberInfo>
            with :? KeyNotFoundException -> 
                raise <| new SerializationException(sprintf "Cloud not load method '%s' from type '%O'." signature dt)

        | Constructor (dt, cParams) -> dt.GetConstructor(getFlags false, null, cParams, [||]) |> fastUnbox<MemberInfo>
        | Property (dt, name, isStatic) -> dt.GetProperty(name, getFlags isStatic) |> fastUnbox<MemberInfo>
        | Field(dt, name, isStatic) -> dt.GetField(name, getFlags isStatic) |> fastUnbox<MemberInfo>
        | Event(dt, name, isStatic) -> dt.GetEvent(name, getFlags isStatic) |> fastUnbox<MemberInfo>

        | Unknown (t, name) -> invalidOp <| sprintf "Cannot load '%s' of type %O." name t



    type ReflectionCache (useStrongNames, tyConv : ITypeNameConverter option) =

        let assemblyCache = new BiMemoizer<_,_>(getAssemblyInfo, loadAssembly useStrongNames)
        let memberInfoCache = new BiMemoizer<_,_>(getMemberInfo tyConv assemblyCache.F, loadMemberInfo tyConv assemblyCache.F assemblyCache.G)

        let qualifiedNameCache = memoize (generateQualifiedName memberInfoCache.F)

        member __.GetAssemblyInfo(a : Assembly) = assemblyCache.F a
        member __.LoadAssembly(a : AssemblyInfo) = assemblyCache.G a
        member __.GetMemberInfo(m : MemberInfo) = memberInfoCache.F m
        member __.LoadMemberInfo(m : CompositeMemberInfo) = memberInfoCache.G m
        member __.GetQualifiedName(t : Type) = qualifiedNameCache t


    type ReflectionManager (useStrongNames, tyConv : ITypeNameConverter option) =

        let cache = new ReflectionCache(useStrongNames, tyConv)

        let assemblyInfoPickler =
            let writer (w : Writer) (aI : AssemblyInfo) =
                let bw = w.BinaryWriter
                writeStringSafe bw aI.Name
                writeStringSafe bw aI.Version
                writeStringSafe bw aI.Culture
            
                match aI.PublicKeyToken with
                | null | [||] -> bw.Write true
                | bytes ->
                    bw.Write false
                    bw.Write bytes

            let reader (r : Reader) =
                let br = r.BinaryReader
                let name = readStringSafe br
                let version = readStringSafe br
                let culture = readStringSafe br

                let pkt =
                    if br.ReadBoolean() then [||]
                    else
                        br.ReadBytes(8)

                {
                    Name = name
                    Version = version
                    Culture = culture
                    PublicKeyToken = pkt
                }

            mkPickler PicklerInfo.ReflectionType true true reader writer

        let rec memberInfoWriter (w : Writer) (m : MemberInfo) =
            match cache.GetMemberInfo m with
            | NamedType (name, aI) ->
                try
                    w.BinaryWriter.Write 0uy
                    w.BinaryWriter.Write name
                    w.Write(assemblyInfoPickler, aI)
                with e ->
                    printfn "%A" e

            | ArrayType (et, rk) ->
                w.BinaryWriter.Write 1uy
                w.Write(typePickler, et)
                match rk with
                | None ->
                    w.BinaryWriter.Write true
                | Some r ->
                    w.BinaryWriter.Write false
                    w.BinaryWriter.Write r

            | GenericType(dt, tyParams) ->
                w.BinaryWriter.Write 2uy
                w.Write(typePickler, dt)
                writeArray w typePickler tyParams

            | GenericTypeParam(dt, idx) ->
                w.BinaryWriter.Write 3uy
                w.Write(typePickler, dt)
                w.BinaryWriter.Write idx

            | GenericMethodParam(dm, idx) ->
                w.BinaryWriter.Write 4uy
                w.Write(methodInfoPickler, dm)
                w.BinaryWriter.Write idx

            | Method(dt, name, isStatic, mParams) ->
                w.BinaryWriter.Write 5uy
                w.Write(typePickler, dt)
                w.BinaryWriter.Write name
                w.BinaryWriter.Write isStatic
                writeArray w typePickler mParams

            | GenericMethod(gm, mParams) ->
                w.BinaryWriter.Write 6uy
                w.Write(methodInfoPickler, gm)
                writeArray w typePickler mParams

            | GenericMethodDefinition(dt, name, isStatic, signature) ->
                w.BinaryWriter.Write 7uy
                w.Write(typePickler, dt)
                w.BinaryWriter.Write name
                w.BinaryWriter.Write isStatic
                w.BinaryWriter.Write signature

            | Constructor(dt, cParams) ->
                w.BinaryWriter.Write 8uy
                w.Write (typePickler, dt)
                writeArray w typePickler cParams

            | Property(dt, name, isStatic) ->
                w.BinaryWriter.Write 9uy
                w.Write (typePickler, dt)
                w.BinaryWriter.Write name
                w.BinaryWriter.Write isStatic

            | Field(dt, name, isStatic) ->
                w.BinaryWriter.Write 10uy
                w.Write (typePickler, dt)
                w.BinaryWriter.Write name
                w.BinaryWriter.Write isStatic

            | Event(dt, name, isStatic) ->
                w.BinaryWriter.Write 11uy
                w.Write (typePickler, dt)
                w.BinaryWriter.Write name
                w.BinaryWriter.Write isStatic

            | Unknown(t, name) ->
                raise <| new NonSerializableTypeException(t, sprintf "could not serialize '%s'." name)

        and memberInfoReader (r : Reader) =
            let cMemberInfo =
                match r.BinaryReader.ReadByte() with
                | 0uy ->
                    let name = r.BinaryReader.ReadString()
                    let assembly = r.Read assemblyInfoPickler
                    NamedType(name, assembly)
                | 1uy ->
                    let et = r.Read typePickler
                    if r.BinaryReader.ReadBoolean() then
                        ArrayType(et, None)
                    else
                        let rk = r.BinaryReader.ReadInt32()
                        ArrayType(et, Some rk)
                | 2uy ->
                    let gt = r.Read typePickler
                    let tyParams = readArray r typePickler
                    GenericType(gt, tyParams)
                | 3uy ->
                    let dt = r.Read typePickler
                    let idx = r.BinaryReader.ReadInt32()
                    GenericTypeParam(dt, idx)
                | 4uy ->
                    let dm = r.Read methodInfoPickler
                    let idx = r.BinaryReader.ReadInt32()
                    GenericMethodParam(dm, idx)
                | 5uy ->
                    let dm = r.Read typePickler
                    let name = r.BinaryReader.ReadString()
                    let isStatic = r.BinaryReader.ReadBoolean()
                    let tyParams = readArray r typePickler
                    Method(dm, name, isStatic, tyParams)
                | 6uy ->
                   let gm = r.Read methodInfoPickler
                   let tparams = readArray r typePickler 
                   GenericMethod(gm, tparams)
                | 7uy ->
                    let dt = r.Read typePickler
                    let name = r.BinaryReader.ReadString()
                    let isStatic = r.BinaryReader.ReadBoolean()
                    let signature = r.BinaryReader.ReadString()
                    GenericMethodDefinition(dt, name, isStatic, signature)
                | 8uy ->
                    let dt = r.Read typePickler
                    let cParams = readArray r typePickler
                    Constructor(dt, cParams)
                | 9uy ->
                    let dt = r.Read typePickler
                    let name = r.BinaryReader.ReadString()
                    let isStatic = r.BinaryReader.ReadBoolean()
                    Property(dt, name, isStatic)
                | 10uy ->
                    let dt = r.Read typePickler
                    let name = r.BinaryReader.ReadString()
                    let isStatic = r.BinaryReader.ReadBoolean()
                    Field(dt, name, isStatic)
                | 11uy ->
                    let dt = r.Read typePickler
                    let name = r.BinaryReader.ReadString()
                    let isStatic = r.BinaryReader.ReadBoolean()
                    Event(dt, name, isStatic)
                // 'Unknown' cases never get serialized, so treat as stream error.
                | _ ->
                    raise <| new SerializationException("Stream error.")

            cache.LoadMemberInfo cMemberInfo

        and memberInfoPickler = mkPickler PicklerInfo.ReflectionType true true memberInfoReader memberInfoWriter
        and typePickler = mkPickler PicklerInfo.ReflectionType true true (memberInfoReader >> fastUnbox<Type>) memberInfoWriter
        and methodInfoPickler = mkPickler PicklerInfo.ReflectionType true true (memberInfoReader >> fastUnbox<MethodInfo>) memberInfoWriter

        let assemblyPickler =
            mkPickler PicklerInfo.ReflectionType true true
                (fun r -> let aI = r.Read assemblyInfoPickler in cache.LoadAssembly aI)
                (fun w a -> let aI = cache.GetAssemblyInfo a in w.Write(assemblyInfoPickler, aI))

        member __.ReflectionPicklers =
            [|
                assemblyPickler :> Pickler
                methodInfoPickler :> _
                memberInfoPickler :> _
                typePickler :> _
            |]

        member __.GetQualifiedName(t : Type) = cache.GetQualifiedName t