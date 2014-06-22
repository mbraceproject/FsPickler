module internal Nessos.FsPickler.ReflectionCache

    open System
    open System.Globalization
    open System.IO
    open System.Text
    open System.Reflection
    open System.Runtime.Serialization
    open System.Collections.Generic
    open System.Collections.Concurrent

    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection

    /// Contains breakdown information for a MemberInfo instance
    /// This information can be memoized for performance and
    /// is sufficient to restructure the instance at deserialization.

    type CompositeMemberInfo =
        // System.Type breakdown
        | NamedType of string * AssemblyInfo
        | ArrayType of Type * (* rank *) int option
        | GenericTypeInstance of Type * Type []
        | GenericTypeParam of Type * int
        | GenericMethodParam of MethodInfo * int
        // System.MethodInfo breakdown
        | Method of Type * (*reflected type*) Type option * (* signature *) string * (* isStatic *) bool
        | GenericMethodInstance of MethodInfo * Type []
        // misc MemberInfo
        | Constructor of Type * (*isStatic*) bool * (* params *) Type []
        | Property of Type * (*reflected type*) Type option * (* name *) string * (* isStatic *) bool
        | Field of Type * (*reflected type*) Type option * (* name *) string * (* isStatic *) bool
        | Event of Type * (*reflected type*) Type option * (* name *) string * (* isStatic *) bool
        | Unknown of Type * string
    

    type AssemblyInfo with

        static member OfAssemblyName(an : AssemblyName) =
            {
                Name = an.Name
                Version = 
                    match an.Version with
                    | null -> null
                    | v -> v.ToString()

                Culture = 
                    if String.IsNullOrEmpty an.CultureInfo.Name then "neutral"
                    else an.CultureInfo.Name

                PublicKeyToken =
                    match an.GetPublicKeyToken () with
                    | null -> null
                    | [||] -> ""
                    | pkt -> Bytes.toBase16String pkt
            }

        static member OfAssembly(a : Assembly) =
            a.GetName() |> AssemblyInfo.OfAssemblyName

        member aI.ToAssemblyName () =
            let an = new AssemblyName()

            an.Name <- aI.Name

            match aI.Version with
            | null | "" -> ()
            | version -> an.Version <- new Version(version)
                
            match aI.Culture with
            | null -> ()
            | "neutral" -> an.CultureInfo <- new CultureInfo("")
            | culture -> an.CultureInfo <- new CultureInfo(culture)

            match aI.PublicKeyToken with
            | null -> ()
            | "" -> an.SetPublicKeyToken [||]
            | pkt -> an.SetPublicKeyToken(Bytes.ofBase16String pkt)

            an

    // Assembly Loader

    let loadAssembly (aI : AssemblyInfo) =
        let an = aI.ToAssemblyName()

        try Assembly.Load an
        with :? FileNotFoundException | :? FileLoadException as e ->

            // in certain cases, such as when assemblies are loaded through reflection
            // Assembly.Load may fail even if already found in AppDomain
            // Resolve this by performing a direct query on the AppDomain.

            let result =
                System.AppDomain.CurrentDomain.GetAssemblies()
                |> Array.tryFind (fun a -> a.FullName = an.FullName)

            match result with
            | None -> raise <| new FsPicklerException("FsPickler: Assembly load error.", e)
            | Some a -> a


    //
    //  MemberInfo Loading Code
    //
        
    // converts a memberInfo instance to a CompositeMemberInfo structure
    let getMemberInfo (tyConv : ITypeNameConverter option) 
                        (getAssemblyInfo : Assembly -> AssemblyInfo)
                        (getMethodSignature : MethodInfo -> string) (m : MemberInfo) =

        let getReflectedType (m : MemberInfo) =
            match m.ReflectedType with
            | rt when rt <> m.DeclaringType -> Some rt
            | _ -> None

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
                GenericTypeInstance(gt, gas)

            elif t.IsGenericParameter then
                match t.DeclaringMethod with
                | null -> GenericTypeParam(t.DeclaringType, t.GenericParameterPosition)
                | dm -> GenericMethodParam(dm :?> MethodInfo, t.GenericParameterPosition)
            else
                // is named type, no generic arguments
                let name =
                    match t.FullName with null -> t.Name | name -> name

                let tI = { Name = name ; AssemblyInfo = getAssemblyInfo t.Assembly }
                let tI' =
                    match tyConv with
                    | None -> tI
                    | Some tc -> tc.OfSerializedType tI

                NamedType(tI'.Name, tI'.AssemblyInfo)

        | :? MethodInfo as m ->
            if m.IsGenericMethod && not m.IsGenericMethodDefinition then
                let gm = m.GetGenericMethodDefinition()
                let mParams = m.GetParameterTypes()
                GenericMethodInstance(gm, mParams)
            else
                let signature = getMethodSignature m
                Method(m.DeclaringType, getReflectedType m, signature, m.IsStatic)

        | :? ConstructorInfo as ctor ->
            let dt = ctor.DeclaringType 
            let ps = ctor.GetParameterTypes()
            Constructor(dt, ctor.IsStatic, ps)

        | :? PropertyInfo as p ->
            let dt = p.ReflectedType
            let isStatic = let m = p.GetGetMethod(true) in m.IsStatic
            Property(dt, getReflectedType p, p.Name, isStatic)

        | :? FieldInfo as f ->
            let dt = f.DeclaringType
            Field(dt, getReflectedType f, f.Name, f.IsStatic)

        | :? EventInfo as e ->
            let dt = e.DeclaringType
            let isStatic = let m = e.GetAddMethod(true) in m.IsStatic
            Event(dt, getReflectedType e, e.Name, isStatic)

        | _ -> Unknown (m.GetType(), m.ToString())



    let loadMemberInfo (tyConv : ITypeNameConverter option) 
                        (loadAssembly : AssemblyInfo -> Assembly) 
                        (getMethodSignature : MethodInfo -> string) (mI : CompositeMemberInfo) =

        let inline getFlags isStatic =
            let allVisibility = BindingFlags.Public ||| BindingFlags.NonPublic
            allVisibility ||| (if isStatic then BindingFlags.Static else BindingFlags.Instance)

        match mI with
        | NamedType(name, aI) ->
            let tI = { Name = name ; AssemblyInfo = aI }
            let tI' =
                match tyConv with
                | None -> tI
                | Some tc -> tc.ToDeserializedType tI

            let assembly = loadAssembly tI'.AssemblyInfo
            try assembly.GetType(tI'.Name, throwOnError = true) |> fastUnbox<MemberInfo>
            with e ->
                raise <| new FsPicklerException("FsPickler: Type load error.", e)

        | ArrayType(et, rk) -> 
            match rk with
            | None -> et.MakeArrayType() |> fastUnbox<MemberInfo>
            | Some r -> et.MakeArrayType(r) |> fastUnbox<MemberInfo>

        | GenericTypeInstance(dt, tyArgs) -> dt.MakeGenericType tyArgs |> fastUnbox<MemberInfo>
        | GenericTypeParam(dt, idx) -> dt.GetGenericArguments().[idx] |> fastUnbox<MemberInfo>
        | GenericMethodParam(dm, idx) -> dm.GetGenericArguments().[idx] |> fastUnbox<MemberInfo>

        | Method(dt, reflectedType, signature, isStatic) ->
            let isMatchingMethod (m : MethodInfo) = 
                m.DeclaringType = dt && getMethodSignature m = signature

            let qt = match reflectedType with None -> dt | Some r -> r

            try qt.GetMethods(getFlags isStatic) |> Array.find isMatchingMethod |> fastUnbox<MemberInfo>
            with :? KeyNotFoundException -> 
                let msg = sprintf "Cloud not load method '%s' from type '%O'." signature dt
                raise <| new FsPicklerException(msg)

        | GenericMethodInstance(gm, mParams) -> gm.MakeGenericMethod mParams |> fastUnbox<MemberInfo>

        | Constructor (dt, isStatic, cParams) -> dt.GetConstructor(getFlags isStatic, null, cParams, [||]) |> fastUnbox<MemberInfo>
        | Property (dt, None, name, isStatic) -> dt.GetProperty(name, getFlags isStatic) |> fastUnbox<MemberInfo>
        | Field(dt, None, name, isStatic) -> dt.GetField(name, getFlags isStatic) |> fastUnbox<MemberInfo>
        | Event(dt, None, name, isStatic) -> dt.GetEvent(name, getFlags isStatic) |> fastUnbox<MemberInfo>

        | Property (dt, Some rt, name, isStatic) ->
            try 
                rt.GetProperties(getFlags isStatic) |> Array.find(fun p -> p.Name = name && p.DeclaringType = dt) |> fastUnbox<MemberInfo>
            with :? KeyNotFoundException ->
                let msg = sprintf "Cloud not load property '%s' from type '%O'." name dt
                raise <| new FsPicklerException(msg)

        | Field (dt, Some rt, name, isStatic) ->
            try 
                rt.GetFields(getFlags isStatic) |> Array.find(fun f -> f.Name = name && f.DeclaringType = dt) |> fastUnbox<MemberInfo>
            with :? KeyNotFoundException ->
                let msg = sprintf "Cloud not load field '%s' from type '%O'." name dt
                raise <| new FsPicklerException(msg)

        | Event (dt, Some rt, name, isStatic) ->
            try 
                rt.GetEvents(getFlags isStatic) |> Array.find(fun e -> e.Name = name && e.DeclaringType = dt) |> fastUnbox<MemberInfo>
            with :? KeyNotFoundException ->
                let msg = sprintf "Cloud not load event '%s' from type '%O'." name dt
                raise <| new FsPicklerException(msg)

        | Unknown (t, name) -> invalidOp <| sprintf "Cannot load '%s' of type %O." name t


    // type signature generator that emulates Type.ToString() but respects ITypeNameConverter rules
    let generateTypeSignature (getInfo : Type -> CompositeMemberInfo) (t : Type) =
        let rec generate (sb : StringBuilder) (t : Type) =
            let inline append (x : string) = sb.Append x |> ignore
            match getInfo t with
            | NamedType(name, _) -> append name
            | ArrayType(et, rk) ->
                generate sb et
                match rk with
                | None -> append "[]"
                | Some 1 -> append "[*]"
                | Some r ->
                    append "["
                    for i = 1 to r - 1 do append ","
                    append "]"

            | GenericTypeInstance(gt, tyParams) ->
                generate sb gt
                append "["
                for i = 0 to tyParams.Length - 1 do
                    generate sb tyParams.[i]
                    if i < tyParams.Length - 1 then append ","
                append "]"

            | GenericTypeParam _
            | GenericMethodParam _ -> append t.Name
            | _ -> invalidOp "not a type."

        let sb = new StringBuilder()
        generate sb t
        sb.ToString()

    // method signature generator that emulates Type.ToString() but respects ITypeNameConverter rules
    let generateMethodSignature (getTypeSig : Type -> string) (m : MethodInfo) =
        let sb = new StringBuilder ()
        let inline append (x : string) = sb.Append x |> ignore

        append <| getTypeSig m.ReturnType
        append " "
        append m.Name
        if m.IsGenericMethod then
            let gas = m.GetGenericArguments()

            append "["
            for i = 0 to gas.Length - 1 do
                append <| getTypeSig gas.[i]
                if i < gas.Length - 1 then append ","
            append "]"

        let ps = m.GetParameterTypes()

        append "("
        for i = 0 to ps.Length - 1 do
            append <| getTypeSig ps.[i]
            if i < ps.Length - 1 then append ","
        append ")"

        sb.ToString()


    #nowarn "40"

    type private CacheRef = ReferenceEqualityContainer<ITypeNameConverter option>
    
    [<AutoSerializable(false)>]
    type ReflectionCache private (?tyConv : ITypeNameConverter) =

        static let cacheCache = new ConcurrentDictionary<CacheRef, ReflectionCache>()

        let loadAssembly = memoize loadAssembly
        let getAssemblyInfo = memoize AssemblyInfo.OfAssembly

        let rec memberInfoCache = new BiMemoizer<_,_>(getMember, loadMember)
        and getMember m   = getMemberInfo tyConv getAssemblyInfo (generateMethodSignature getSignature) m
        and loadMember mI = loadMemberInfo tyConv loadAssembly (generateMethodSignature getSignature) mI
        and getSignature = memoize (fun t -> generateTypeSignature memberInfoCache.F t)

        member __.GetAssemblyInfo(a : Assembly) = getAssemblyInfo a
        member __.LoadAssembly(aI : AssemblyInfo) = loadAssembly aI
        member __.GetCompositeMemberInfo(m : MemberInfo) = memberInfoCache.F m
        member __.LoadMemberInfo(m : CompositeMemberInfo) = memberInfoCache.G m
        member __.GetTypeSignature(t : Type) = getSignature t

        static member Create(?tyConv : ITypeNameConverter) =
            let container = new ReferenceEqualityContainer<_>(tyConv)
            match cacheCache.TryFind container with
            | None -> cacheCache.GetOrAdd(container, fun _ -> new ReflectionCache(?tyConv = tyConv))
            | Some cache -> cache