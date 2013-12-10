module internal FsPickler.ReflectionPicklers

    open System
    open System.Globalization
    open System.IO
    open System.Reflection
    open System.Runtime.Serialization
    open System.Collections.Generic
    open System.Collections.Concurrent

    open FsPickler
    open FsPickler.Utils
    open FsPickler.PicklerUtils


    type CompositeTypeInfo =
        | NamedType of TypeInfo
        | GenericType of CompositeTypeInfo * CompositeTypeInfo []
        | TypeParam of CompositeTypeInfo * int
        | MethodParam of CompositeMethodInfo * int

    and CompositeMethodInfo =
        | NamedMethod of CompositeTypeInfo * string * bool * CompositeTypeInfo [] option
        | GenericMethod of CompositeMethodInfo * CompositeTypeInfo []

    and CompositeMemberInfo =
        | Type of CompositeTypeInfo
        | Method of CompositeMethodInfo
        | Constructor of CompositeTypeInfo * CompositeTypeInfo []
        | Other of CompositeTypeInfo * string * bool
        | Unknown


    type AssemblyCache () =

        let cache = new ConcurrentCache<Assembly, AssemblyInfo> ()
        let invCache = new ConcurrentCache<AssemblyInfo, Assembly> ()

        let getInfo (a : Assembly) =
            let an = a.GetName()
            {
                Name = an.Name
                Version = an.Version.ToString()
                Culture = an.CultureInfo.ToString()
                PublicKeyToken = an.GetPublicKeyToken()
            }

        let loadAssembly (aI : AssemblyInfo) =
            let an = new AssemblyName()

            an.Name <- aI.Name

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

        member __.GetAssemblyInfo(a : Assembly) = biMemoize cache invCache getInfo a
        member __.LoadAssembly(a : AssemblyInfo) = biMemoize invCache cache loadAssembly a
            

    type MemberInfoCache(assemblyCache : AssemblyCache, tyConv : ITypeNameConverter) =

        let getFlags isStatic =
            let allVisibility = BindingFlags.Public ||| BindingFlags.NonPublic
            allVisibility ||| (if isStatic then BindingFlags.Static else BindingFlags.Instance)

        let cache = new ConcurrentCache<MemberInfo, CompositeMemberInfo> ()
        let invCache = new ConcurrentCache<CompositeMemberInfo, MemberInfo> ()

        let getType (t : CompositeMemberInfo) =
            match t with
            | Type t -> t
            | _ -> invalidOp <| sprintf "%A is not a type." t

        let getMethod (m : CompositeMemberInfo) =
            match m with
            | Method m -> m
            | _ -> invalidOp <| sprintf "%A is not a method." m

        let getInfo (self : MemberInfo -> CompositeMemberInfo) (m : MemberInfo) =
            printfn "parsing memberInfo"
            match m with
            | :? Type as t ->
                let ty =
                    if t.IsGenericType && not t.IsGenericTypeDefinition then
                        let gt = t.GetGenericTypeDefinition() |> self |> getType
                        let gas = t.GetGenericArguments() |> Array.map (self >> getType)
                        GenericType(gt, gas)
                    elif t.IsGenericParameter then
                        match t.DeclaringType with
                        | null -> MethodParam(getMethod <| self t.DeclaringMethod, t.GenericParameterPosition)
                        | dt -> TypeParam(getType <| self dt, t.GenericParameterPosition)
                    else
                        let tI = { Name = t.FullName ; Assembly = assemblyCache.GetAssemblyInfo t.Assembly }
                        NamedType(tyConv.OfSerializedType tI)

                Type ty

            | :? MethodInfo as m ->
                let meth =
                    if m.IsGenericMethod && not m.IsGenericMethodDefinition then
                        let gm = m.GetGenericMethodDefinition() |> self |> getMethod
                        let tyArgs = m.GetGenericArguments() |> Array.map (self >> getType)
                        GenericMethod(gm, tyArgs)
                    else
                        let dt = m.DeclaringType |> self |> getType
                        let flags = getFlags m.IsStatic
                        let overloads = m.DeclaringType.GetMethods(flags) |> Array.filter (fun m' -> m'.Name = m.Name)
                        let overload =
                            if overloads.Length = 1 then None
                            else
                                Some(m.GetParameters() |> Array.map (fun p -> p.ParameterType |> self |> getType))

                        NamedMethod(dt, m.Name, m.IsStatic, overload)

                Method(meth)

            | :? ConstructorInfo as ctor ->
                let dt = ctor.DeclaringType |> self |> getType
                let ps = ctor.GetParameterTypes() |> Array.map (self >> getType)
                Constructor(dt, ps)

            | :? PropertyInfo as p ->
                let dt = p.DeclaringType |> self |> getType
                let isStatic = let m = p.GetGetMethod(true) in m.IsStatic
                Other(dt, p.Name, isStatic)

            | :? FieldInfo as f ->
                let dt = f.DeclaringType |> self |> getType
                Other(dt, f.Name, f.IsStatic)

            | :? EventInfo as e ->
                let dt = e.DeclaringType |> self |> getType
                let isStatic = let m = e.GetAddMethod() in m.IsStatic
                Other(dt, e.Name, isStatic)

            | _ -> Unknown
                

        let loadMemberInfo (self : CompositeMemberInfo -> MemberInfo) (mI : CompositeMemberInfo) =
            let loadType (tI : CompositeTypeInfo) = self (Type tI) |> fastUnbox<Type>
            let loadMethod (mI : CompositeMethodInfo) = self (Method mI) |> fastUnbox<MethodInfo>

            printfn "loading memberInfo"

            match mI with
            | Type tI ->
                match tI with
                | NamedType tI ->
                    let tI = tyConv.ToDeserializedType tI
                    let assembly = assemblyCache.LoadAssembly tI.Assembly
                    try assembly.GetType(tI.Name, throwOnError = true) :> MemberInfo
                    with e ->
                        raise <| new SerializationException("FsPickler: Type load exception.", e)

                | GenericType(dtI, tyArgI) ->
                    let dt = loadType dtI
                    let tyArgs = tyArgI |> Array.map loadType
                    dt.MakeGenericType tyArgs :> _
                | TypeParam(dtI, idx) ->
                    let dt = loadType dtI
                    dt.GetGenericArguments().[idx] :> _
                | MethodParam(dmI, idx) ->
                    let dm = loadMethod dmI
                    dm.GetGenericArguments().[idx] :> _

            | Method mI ->
                match mI with
                | NamedMethod(dtI, name, isStatic, overloads) ->
                    let dt = loadType dtI
                    let flags = getFlags isStatic
                    match overloads with
                    | None -> dt.GetMethod(name, flags) :> _
                    | Some paramInfo ->
                        let mParams = Array.map loadType paramInfo 
                        dt.GetMethods(flags)
                        |> Array.find(fun m -> m.Name = name && m.GetParameterTypes() = mParams) :> _
                | GenericMethod(gmI, gmParams) ->
                    let gm = loadMethod gmI
                    let mParams = gmParams |> Array.map loadType
                    gm.MakeGenericMethod mParams :> _

            | Constructor (dtI, paramInfo) ->
                let dt = loadType dtI
                let cParams = paramInfo |> Array.map loadType
                dt.GetConstructor(getFlags false, null, cParams, [||]) :> _

            | Other (dtI, name, isStatic) ->
                let dt = loadType dtI
                dt.GetMember(name, getFlags isStatic).[0]

            | Unknown -> invalidOp "Unknown memberInfo!"

        member __.GetMemberInfo (m : MemberInfo) = YBiMemoize cache invCache getInfo m
        member __.GetTypeInfo (t : Type) = __.GetMemberInfo t |> getType
        member __.LoadMemberInfo (mI : CompositeMemberInfo) = YBiMemoize invCache cache loadMemberInfo mI
        member __.LoadType (tI : CompositeTypeInfo) = __.LoadMemberInfo (Type tI) |> fastUnbox<Type>





    let mkReflectionPicklers (tyConv : ITypeNameConverter) =

        let assemblyCache = new AssemblyCache()
        let memberInfoCache = new MemberInfoCache(assemblyCache, tyConv)

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

        let tyInfoPickler =
            let writer (w : Writer) (tI : TypeInfo) =
                w.BinaryWriter.Write tI.Name
                w.Write(assemblyInfoPickler, tI.Assembly)

            let reader (r : Reader) =
                let name = r.BinaryReader.ReadString()
                let assembly = r.Read assemblyInfoPickler
                { Name = name ; Assembly = assembly }

            mkPickler PicklerInfo.ReflectionType true true reader writer

        let rec typeInfoWriter (w : Writer) (tI : CompositeTypeInfo) =
            match tI with
            | NamedType tI ->
                w.BinaryWriter.Write 0uy
                w.Write(tyInfoPickler, tI)
            | GenericType(dtI, paramI) ->
                w.BinaryWriter.Write 1uy
                w.Write(typeInfoPickler, dtI)
                w.BinaryWriter.Write paramI.Length
                for p in paramI do
                    w.Write(typeInfoPickler, p)
            | TypeParam(dtI, idx) ->
                w.BinaryWriter.Write 2uy
                w.Write(typeInfoPickler, dtI)
                w.BinaryWriter.Write idx
            | MethodParam(dmI, idx) ->
                w.BinaryWriter.Write 3uy
                w.Write(methodInfoPickler, dmI)
                w.BinaryWriter.Write idx

        and typeInfoReader (r : Reader) =
            match r.BinaryReader.ReadByte() with
            | 0uy -> NamedType(r.Read tyInfoPickler)
            | 1uy ->
                let dtI = r.Read typeInfoPickler
                let n = r.BinaryReader.ReadInt32()
                let paramI = Array.zeroCreate<CompositeTypeInfo> n
                for i = 0 to n - 1 do
                    paramI.[i] <- r.Read typeInfoPickler
                GenericType(dtI, paramI)
            | 2uy ->
                let dtI = r.Read typeInfoPickler
                let idx = r.BinaryReader.ReadInt32()
                TypeParam(dtI, idx)
            | _ ->
                let dmI = r.Read methodInfoPickler
                let idx = r.BinaryReader.ReadInt32()
                MethodParam(dmI, idx)
        
        and methodInfoWriter (w : Writer) (mI : CompositeMethodInfo) =
            match mI with
            | NamedMethod(dtI, name, isStatic, overload) ->
                w.BinaryWriter.Write 0uy
                w.Write(typeInfoPickler, dtI)
                w.BinaryWriter.Write name
                w.BinaryWriter.Write isStatic
                match overload with
                | None -> w.BinaryWriter.Write true
                | Some overloads ->
                    w.BinaryWriter.Write false
                    writeArray w typeInfoPickler overloads

            | GenericMethod(gmI, tparams) ->
                w.BinaryWriter.Write 1uy
                w.Write(methodInfoPickler, gmI)
                writeArray w typeInfoPickler tparams        

        and methodInfoReader (r : Reader) = 
            match r.BinaryReader.ReadByte() with
            | 0uy ->
                let dtI = r.Read typeInfoPickler
                let name = r.BinaryReader.ReadString()
                let isStatic = r.BinaryReader.ReadBoolean()
                let overload =
                    if r.BinaryReader.ReadBoolean() then None
                    else
                        let overloads = readArray r typeInfoPickler
                        Some overloads

                NamedMethod(dtI,name,isStatic,overload)

            | _ ->
               let gmI = r.Read methodInfoPickler
               let tparams = readArray r typeInfoPickler 
               GenericMethod(gmI, tparams)

        and memberInfoWriter (w : Writer) (mI : CompositeMemberInfo) =
            match mI with
            | Type tI ->
                w.BinaryWriter.Write 0uy
                w.Write (typeInfoPickler, tI)
            | Method mI ->
                w.BinaryWriter.Write 1uy
                w.Write (methodInfoPickler, mI)
            | Constructor(dtI, cParams) ->
                w.BinaryWriter.Write 2uy
                w.Write (typeInfoPickler, dtI)
                writeArray w typeInfoPickler cParams
            | Other(dtI, name, isStatic) ->
                w.BinaryWriter.Write 3uy
                w.Write (typeInfoPickler, dtI)
                w.BinaryWriter.Write name
                w.BinaryWriter.Write isStatic
            | Unknown ->
                w.BinaryWriter.Write 4uy

        and memberInfoReader (r : Reader) =
            match r.BinaryReader.ReadByte() with
            | 0uy ->
                let tI = r.Read typeInfoPickler
                Type tI
            | 1uy ->
                let mI = r.Read methodInfoPickler
                Method mI
            | 2uy ->
                let dtI = r.Read typeInfoPickler
                let cParams = readArray r typeInfoPickler
                Constructor(dtI, cParams)
            | 3uy ->
                let dtI = r.Read typeInfoPickler
                let name = r.BinaryReader.ReadString()
                let isStatic = r.BinaryReader.ReadBoolean()
                Other(dtI, name, isStatic)
            | _ -> Unknown

        and typeInfoPickler = mkPickler PicklerInfo.ReflectionType true true typeInfoReader typeInfoWriter
        and methodInfoPickler = mkPickler PicklerInfo.ReflectionType true true methodInfoReader methodInfoWriter
        and memberInfoPickler = mkPickler PicklerInfo.ReflectionType true true memberInfoReader memberInfoWriter

        let typePickler =
            let writer (w : Writer) (t : Type) =
                let tI = memberInfoCache.GetTypeInfo t
                w.Write(typeInfoPickler, tI)

            let reader (r : Reader) =
                let tI = r.Read typeInfoPickler
                memberInfoCache.LoadType tI

            mkPickler PicklerInfo.ReflectionType true true reader writer

        let mInfoPickler =
            let writer (w : Writer) (m : MemberInfo) =
                let mI = memberInfoCache.GetMemberInfo m
                w.Write(memberInfoPickler, mI)

            let reader (r : Reader) =
                let mI = r.Read memberInfoPickler
                memberInfoCache.LoadMemberInfo mI

            mkPickler PicklerInfo.ReflectionType true true reader writer


        [|
            assemblyInfoPickler :> Pickler
            tyInfoPickler :> _
            typeInfoPickler :> _
            methodInfoPickler :> _
            memberInfoPickler :> _
            typePickler :> _
            mInfoPickler :> _
        |]



//            match r.BinaryReader.ReadByte () with
//            | 0uy ->
//                
            
            
//    let mkTypeInfoPickler () =
//            
//        let inline writer (w : Writer) (tI : TypeInfo) =
//            let bw = w.BinaryWriter

//

//
//        let inline reader (r : Reader) (tI : TypeInfo) =

//
//        match tyConv with
//        | None -> mkPickler PicklerInfo.FSharpValue 


//        let getMemberInfoPickler (tyConv : ITypeNameConverter option) (resolver : IPicklerResolver) =
//
//            let typeInfoPickler =
//                match tyConv with
//                | None -> resolver.Resolve<TypeInfo> ()
//                | Some tc ->
//                    let writer (w : Writer) (tI : TypeInfo) =
                    

        

//
//    // custom serialize/deserialize routines for TypeInfo records
//        
//    let writeTypeInfo (bw : BinaryWriter) (tI : TypeInfo) =
//        writeStringSafe bw tI.Name
//        writeStringSafe bw tI.AssemblyName
//        writeStringSafe bw tI.Version
//        writeStringSafe bw tI.Culture
//
//        match tI.PublicKeyToken with
//        | null | [||] -> bw.Write true
//        | bytes ->
//            bw.Write false
//            bw.Write bytes
//
//    let readTypeInfo (br : BinaryReader) =
//        let name = readstringsafe br
//        let assemblyname = readstringsafe br
//        let version = readstringsafe br
//        let culture = readstringsafe br
//
//        let pkt =
//            if br.readboolean() then [||]
//            else
//                br.readbytes(8)
//
//        {
//            name = name
//            assemblyname = assemblyname
//            version = version
//            culture = culture
//            publickeytoken = pkt
//        }
//
//
//    // resolve MemberInfo from a collection of members using given signature
//    let resolveBySignature<'T when 'T :> MemberInfo> (declaringType : Type) (signature : string) (members : 'T []) =
//        match members |> Array.tryFind(fun m -> m.ToString() = signature) with
//        | Some m -> fastUnbox<MemberInfo> m
//        | None ->
//            let msg = sprintf "FsPickler: could not locate '%s' in type '%O'." signature declaringType
//            raise <| new SerializationException(msg)
//
//
//    // System.Type & MemberInfo pickler definitions
//
//    let mkReflectionPicklers (tyConv : ITypeNameConverter) =
//
//        // memoize Assembly loaders
//
//        let getAssemblyInfo = memoize AssemblyInfo.OfAssembly
//        let loadAssembly = memoize AssemblyInfo.ToAssembly
//
//        //
//        // System.Type serialization logic
//        //
//
//        let rec writeType (w : Writer) (t : Type) =
//            if t.IsGenericType && not t.IsGenericTypeDefinition then
//                w.BinaryWriter.Write 0uy
//                w.Write(typePickler, t.GetGenericTypeDefinition())
//                let gas = t.GetGenericArguments()
//                w.BinaryWriter.Write gas.Length
//                for ga in gas do
//                    w.Write(typePickler, ga)
//
//            elif t.IsGenericParameter then
//                w.BinaryWriter.Write 1uy
//                w.BinaryWriter.Write t.GenericParameterPosition
//
//                match t.DeclaringMethod with
//                | null ->
//                    // is type-level generic parameter
//                    w.BinaryWriter.Write true
//                    w.Write(typePickler, t.DeclaringType)
//                | dm ->
//                    // is method-level generic parameter
//                    w.BinaryWriter.Write false
//                    w.Write(memberInfoPickler, fastUnbox<MemberInfo> dm)
//            else
//                w.BinaryWriter.Write 2uy
//                let aI = getAssemblyInfo t.Assembly
//                let typeInfo =
//                    {
//                        Name = t.FullName
//                        AssemblyName = aI.Name
//                        Culture = aI.Culture
//                        Version = aI.Version
//                        PublicKeyToken = aI.PublicKeyToken
//                    }
//                    
//                do writeTypeInfo w.BinaryWriter (tyConv.OfSerializedType typeInfo)
//
//        and readType (r : Reader) : Type =
//            match r.BinaryReader.ReadByte() with
//            | 0uy ->
//                let gt = r.Read typePickler
//                let n = r.BinaryReader.ReadInt32()
//                let gas = Array.zeroCreate<Type> n
//                for i = 0 to n - 1 do
//                    gas.[i] <- r.Read typePickler
//
//                gt.MakeGenericType gas
//            | 1uy ->
//                let idx = r.BinaryReader.ReadInt32()
//                if r.BinaryReader.ReadBoolean() then
//                    let dt = r.Read typePickler
//                    dt.GetGenericArguments().[idx]
//                else
//                    let dm = r.Read memberInfoPickler |> fastUnbox<MethodInfo>
//                    dm.GetGenericArguments().[idx]
//            | _ ->
//                let tI = tyConv.ToDeserializedType (readTypeInfo r.BinaryReader)
//                let aI =
//                    {
//                        Name = tI.AssemblyName
//                        Culture = tI.Culture
//                        Version = tI.Version
//                        PublicKeyToken = tI.PublicKeyToken
//                    }
//                let assembly = loadAssembly aI
//                
//                try assembly.GetType(tI.Name, throwOnError = true)
//                with e ->
//                    raise <| new SerializationException("FsPickler: Type load exception.", e)
//
//        //
//        // MemberInfo serialization logic
//        //
//
//        and writeMemberInfo (w : Writer) (m : MemberInfo) =
//            w.BinaryWriter.Write (byte m.MemberType)
//            match m.MemberType with
//            | MemberTypes.TypeInfo | MemberTypes.NestedType ->
//                typePickler.Write w (fastUnbox<Type> m)
//
//            | MemberTypes.Constructor ->
//                w.Write(typePickler, m.DeclaringType)
//                w.BinaryWriter.Write(m.ToString())
//
//            | MemberTypes.Method ->
//                let meth = fastUnbox<MethodInfo> m
//                if meth.IsGenericMethod && not meth.IsGenericMethodDefinition then
//                    w.BinaryWriter.Write true
//                    let gMeth = meth.GetGenericMethodDefinition()
//                    let tyArgs = meth.GetGenericArguments()
//
//                    w.BinaryWriter.Write tyArgs.Length
//                    for tyArg in tyArgs do w.Write(typePickler, tyArg)
//                    w.Write(memberInfoPickler, fastUnbox<MemberInfo> gMeth)
//                else
//                    w.BinaryWriter.Write false
//
//                    w.Write(typePickler, m.DeclaringType)
//
//                    w.BinaryWriter.Write meth.IsStatic
//                    w.BinaryWriter.Write meth.IsPublic
//                    w.BinaryWriter.Write(m.ToString())
//            | _ ->
//                w.Write(typePickler, m.DeclaringType)
//                w.BinaryWriter.Write m.Name
//
//        and readMemberInfo (r : Reader) =
//            let memberType = enum<MemberTypes> (int <| r.BinaryReader.ReadByte())
//            match memberType with
//            | MemberTypes.TypeInfo | MemberTypes.NestedType ->
//                readType r |> fastUnbox<MemberInfo>
//
//            | MemberTypes.Constructor ->
//                let dt = r.Read typePickler
//                let signature = r.BinaryReader.ReadString()
//                dt.GetConstructors(allConstructors)
//                |> resolveBySignature dt signature
//
//            | MemberTypes.Method ->
//                if r.BinaryReader.ReadBoolean() then
//                    let n = r.BinaryReader.ReadInt32()
//                    let tyArgs = Array.zeroCreate<Type> n
//                    for i = 0 to n - 1 do
//                        tyArgs.[i] <- r.Read typePickler
//
//                    let gMeth = r.Read memberInfoPickler |> fastUnbox<MethodInfo>
//                    gMeth.MakeGenericMethod tyArgs |> fastUnbox<MemberInfo>
//                else
//                    let dt = r.Read typePickler
//
//                    let bf1 =
//                        if r.BinaryReader.ReadBoolean() then BindingFlags.Static
//                        else BindingFlags.Instance
//
//                    let bf2 =
//                        if r.BinaryReader.ReadBoolean() then BindingFlags.Public
//                        else BindingFlags.NonPublic
//
//                    let signature = r.BinaryReader.ReadString()
//
//                    dt.GetMethods(bf1 ||| bf2)
//                    |> resolveBySignature dt signature
//
//            | _ ->
//                let dt = r.Read typePickler
//                let name = r.BinaryReader.ReadString()
//                match dt.GetMember(name, memberType, allMembers) with
//                | [| m |] -> m
//                | [| |] -> 
//                    let msg = sprintf "FsPickler: could not locate %O '%s' in type '%O'." memberType name dt
//                    raise <| new SerializationException(msg)
//                | _ ->
//                    let msg = sprintf "FsPickler: ambiguous matches for %O '%s' in type '%O'." memberType name dt
//                    raise <| new SerializationException(msg)
//
//        and typePickler : Pickler<Type> = 
//            mkPickler PicklerInfo.ReflectionType true true readType writeType
//
//        and memberInfoPickler : Pickler<MemberInfo> = 
//            mkPickler PicklerInfo.ReflectionType true true readMemberInfo writeMemberInfo
//
//        [| memberInfoPickler :> Pickler ; typePickler :> Pickler |]