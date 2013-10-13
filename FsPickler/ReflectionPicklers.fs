module internal FsPickler.ReflectionPicklers

    open System
    open System.Globalization
    open System.IO
    open System.Reflection
    open System.Runtime.Serialization
    open System.Collections.Generic

    open FsPickler
    open FsPickler.Utils
    open FsPickler.PicklerUtils


    /// replacement for the System.Reflection.AssemblyName type,
    /// which does not implement proper equality semantics
    type AssemblyInfo =
        {
            Name : string
            Version : string
            Culture : string
            PublicKeyToken : byte []
        }
    with
        static member OfAssembly (a : Assembly) =
            let an = a.GetName()
            {
                Name = an.Name
                Version = an.Version.ToString()
                Culture = an.CultureInfo.ToString()
                PublicKeyToken = an.GetPublicKeyToken()
            }

        static member ToAssembly (aI : AssemblyInfo) =
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
            with :? FileLoadException as e ->
            
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



    // custom serialize/deserialize routines for TypeInfo records
        
    let writeTypeInfo (bw : BinaryWriter) (tI : TypeInfo) =
        writeStringSafe bw tI.Name
        writeStringSafe bw tI.AssemblyName
        writeStringSafe bw tI.Version
        writeStringSafe bw tI.Culture

        match tI.PublicKeyToken with
        | null | [||] -> bw.Write true
        | bytes ->
            bw.Write false
            bw.Write bytes

    let readTypeInfo (br : BinaryReader) =
        let name = readStringSafe br
        let assemblyName = readStringSafe br
        let version = readStringSafe br
        let culture = readStringSafe br

        let pkt =
            if br.ReadBoolean() then [||]
            else
                br.ReadBytes(8)

        {
            Name = name
            AssemblyName = assemblyName
            Version = version
            Culture = culture
            PublicKeyToken = pkt
        }


    // resolve MemberInfo from a collection of members using given signature
    let resolveBySignature<'T when 'T :> MemberInfo> (declaringType : Type) (signature : string) (members : 'T []) =
        match members |> Array.tryFind(fun m -> m.ToString() = signature) with
        | Some m -> fastUnbox<MemberInfo> m
        | None ->
            let msg = sprintf "FsPickler: could not locate '%s' in type '%O'." signature declaringType
            raise <| new SerializationException(msg)


    // System.Type & MemberInfo pickler definitions

    let mkReflectionPicklers (tyConv : ITypeNameConverter) =

        // memoize Assembly loaders

        let getAssemblyInfo = memoize AssemblyInfo.OfAssembly
        let loadAssembly = memoize AssemblyInfo.ToAssembly

        //
        // System.Type serialization logic
        //

        let rec writeType (w : Writer) (t : Type) =
            if t.IsGenericType && not t.IsGenericTypeDefinition then
                w.BinaryWriter.Write 0uy
                w.Write(typePickler, t.GetGenericTypeDefinition())
                let gas = t.GetGenericArguments()
                w.BinaryWriter.Write gas.Length
                for ga in gas do
                    w.Write(typePickler, ga)

            elif t.IsGenericParameter then
                w.BinaryWriter.Write 1uy
                w.BinaryWriter.Write t.GenericParameterPosition

                match t.DeclaringMethod with
                | null ->
                    // is type-level generic parameter
                    w.BinaryWriter.Write true
                    w.Write(typePickler, t.DeclaringType)
                | dm ->
                    // is method-level generic parameter
                    w.BinaryWriter.Write false
                    w.Write(memberInfoPickler, fastUnbox<MemberInfo> dm)
            else
                w.BinaryWriter.Write 2uy
                let aI = getAssemblyInfo t.Assembly
                let typeInfo =
                    {
                        Name = t.FullName
                        AssemblyName = aI.Name
                        Culture = aI.Culture
                        Version = aI.Version
                        PublicKeyToken = aI.PublicKeyToken
                    }
                    
                do writeTypeInfo w.BinaryWriter (tyConv.OfSerializedType typeInfo)

        and readType (r : Reader) : Type =
            match r.BinaryReader.ReadByte() with
            | 0uy ->
                let gt = r.Read typePickler
                let n = r.BinaryReader.ReadInt32()
                let gas = Array.zeroCreate<Type> n
                for i = 0 to n - 1 do
                    gas.[i] <- r.Read typePickler

                gt.MakeGenericType gas
            | 1uy ->
                let idx = r.BinaryReader.ReadInt32()
                if r.BinaryReader.ReadBoolean() then
                    let dt = r.Read typePickler
                    dt.GetGenericArguments().[idx]
                else
                    let dm = r.Read memberInfoPickler |> fastUnbox<MethodInfo>
                    dm.GetGenericArguments().[idx]
            | _ ->
                let tI = tyConv.ToDeserializedType (readTypeInfo r.BinaryReader)
                let aI =
                    {
                        Name = tI.AssemblyName
                        Culture = tI.Culture
                        Version = tI.Version
                        PublicKeyToken = tI.PublicKeyToken
                    }
                let assembly = loadAssembly aI
                
                try assembly.GetType(tI.Name, throwOnError = true)
                with e ->
                    raise <| new SerializationException("FsPickler: Type load exception.", e)

        //
        // MemberInfo serialization logic
        //

        and writeMemberInfo (w : Writer) (m : MemberInfo) =
            w.BinaryWriter.Write (byte m.MemberType)
            match m.MemberType with
            | MemberTypes.TypeInfo | MemberTypes.NestedType ->
                typePickler.Write w (fastUnbox<Type> m)

            | MemberTypes.Constructor ->
                w.Write(typePickler, m.DeclaringType)
                w.BinaryWriter.Write(m.ToString())

            | MemberTypes.Method ->
                let meth = fastUnbox<MethodInfo> m
                if meth.IsGenericMethod && not meth.IsGenericMethodDefinition then
                    w.BinaryWriter.Write true
                    let gMeth = meth.GetGenericMethodDefinition()
                    let tyArgs = meth.GetGenericArguments()

                    w.BinaryWriter.Write tyArgs.Length
                    for tyArg in tyArgs do w.Write(typePickler, tyArg)
                    w.Write(memberInfoPickler, fastUnbox<MemberInfo> gMeth)
                else
                    w.BinaryWriter.Write false

                    w.Write(typePickler, m.DeclaringType)

                    w.BinaryWriter.Write meth.IsStatic
                    w.BinaryWriter.Write meth.IsPublic
                    w.BinaryWriter.Write(m.ToString())
            | _ ->
                w.Write(typePickler, m.DeclaringType)
                w.BinaryWriter.Write m.Name

        and readMemberInfo (r : Reader) =
            let memberType = enum<MemberTypes> (int <| r.BinaryReader.ReadByte())
            match memberType with
            | MemberTypes.TypeInfo | MemberTypes.NestedType ->
                readType r |> fastUnbox<MemberInfo>

            | MemberTypes.Constructor ->
                let dt = r.Read typePickler
                let signature = r.BinaryReader.ReadString()
                dt.GetConstructors(allConstructors)
                |> resolveBySignature dt signature

            | MemberTypes.Method ->
                if r.BinaryReader.ReadBoolean() then
                    let n = r.BinaryReader.ReadInt32()
                    let tyArgs = Array.zeroCreate<Type> n
                    for i = 0 to n - 1 do
                        tyArgs.[i] <- r.Read typePickler

                    let gMeth = r.Read memberInfoPickler |> fastUnbox<MethodInfo>
                    gMeth.MakeGenericMethod tyArgs |> fastUnbox<MemberInfo>
                else
                    let dt = r.Read typePickler

                    let bf1 =
                        if r.BinaryReader.ReadBoolean() then BindingFlags.Static
                        else BindingFlags.Instance

                    let bf2 =
                        if r.BinaryReader.ReadBoolean() then BindingFlags.Public
                        else BindingFlags.NonPublic

                    let signature = r.BinaryReader.ReadString()

                    dt.GetMethods(bf1 ||| bf2)
                    |> resolveBySignature dt signature

            | _ ->
                let dt = r.Read typePickler
                let name = r.BinaryReader.ReadString()
                match dt.GetMember(name, memberType, allMembers) with
                | [| m |] -> m
                | [| |] -> 
                    let msg = sprintf "FsPickler: could not locate %O '%s' in type '%O'." memberType name dt
                    raise <| new SerializationException(msg)
                | _ ->
                    let msg = sprintf "FsPickler: ambiguous matches for %O '%s' in type '%O'." memberType name dt
                    raise <| new SerializationException(msg)

        and typePickler : Pickler<Type> = 
            mkPickler PicklerInfo.ReflectionType true true readType writeType

        and memberInfoPickler : Pickler<MemberInfo> = 
            mkPickler PicklerInfo.ReflectionType true true readMemberInfo writeMemberInfo

        [| memberInfoPickler :> Pickler ; typePickler :> Pickler |]