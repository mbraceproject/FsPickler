module internal Nessos.FsPickler.TypeCache

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
        | GenericType of Type * Type []
        | GenericTypeParam of Type * int
        | GenericMethodParam of MethodInfo * int
        // System.MethodInfo breakdown
        | Method of Type * (* name *) string * (* isStatic *) bool * Type []
        | GenericMethod of MethodInfo * Type []
        // in generic method definitions, encode as signature
        // this is to avoid recursive situations in method overload resolutions
        | GenericMethodDefinition of Type * (* name *) string * (* isStatic *) bool * (*params*) string []
        // misc MemberInfo
        | Constructor of Type * Type []
        | Property of Type * (* name *) string * (* isStatic *) bool
        | Field of Type * (* name *) string * (* isStatic *) bool
        | Event of Type * (* name *) string * (* isStatic *) bool
        | Unknown of Type * string


    /// TypeInfo extension methods

    type AssemblyInfo with
        static member ToAssemblyName (aI : AssemblyInfo) =
            let an = new AssemblyName()

            an.Name <- aI.Name

            match aI.Version with
            | null | "" -> ()
            | version -> an.Version <- new Version(version)
                
            match aI.Culture with
            | null -> ()
            | culture -> an.CultureInfo <- new System.Globalization.CultureInfo(culture)

            match aI.PublicKeyToken with
            | null -> ()
            | pkt -> an.SetPublicKeyToken(pkt)

            an

    
    // Assembly Loading Code

    let loadAssembly (aI : AssemblyInfo) =
        let an = AssemblyInfo.ToAssemblyName aI

        try Assembly.Load an
        with :? FileNotFoundException | :? FileLoadException as e ->

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
                        (getQualifiedName : Type -> string) (m : MemberInfo) =

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
            if m.IsGenericMethod then
                if m.IsGenericMethodDefinition then
                    let mParams = m.GetParameterTypes() |> Array.map getQualifiedName
                    GenericMethodDefinition(m.ReflectedType, m.Name, m.IsStatic, mParams)
                else
                    let gm = m.GetGenericMethodDefinition()
                    let tyArgs = m.GetGenericArguments()
                    GenericMethod(gm, tyArgs)
            else
                let mParams = m.GetParameters() |> Array.map (fun p -> p.ParameterType)
                Method(m.ReflectedType, m.Name, m.IsStatic, mParams)

        | :? ConstructorInfo as ctor ->
            let dt = ctor.DeclaringType 
            let ps = ctor.GetParameterTypes()
            Constructor(dt, ps)

        | :? PropertyInfo as p ->
            let dt = p.ReflectedType
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



    let loadMemberInfo (tyConv : ITypeNameConverter option) 
                        (loadAssembly : AssemblyInfo -> Assembly) 
                        (getQualifiedName : Type -> string) (mI : CompositeMemberInfo) =

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

        | GenericType(dt, tyArgs) -> dt.MakeGenericType tyArgs |> fastUnbox<MemberInfo>
        | GenericTypeParam(dt, idx) -> dt.GetGenericArguments().[idx] |> fastUnbox<MemberInfo>
        | GenericMethodParam(dm, idx) -> dm.GetGenericArguments().[idx] |> fastUnbox<MemberInfo>
        | Method(dt, name, isStatic, mParams) ->  dt.GetMethod(name, getFlags isStatic, null, mParams, [||]) |> fastUnbox<MemberInfo>
        | GenericMethod(gm, tyParams) -> gm.MakeGenericMethod tyParams |> fastUnbox<MemberInfo>
        | GenericMethodDefinition(dt, name, isStatic, signature) ->
            let isMatchingGenericMethod (m : MethodInfo) =
                if m.Name = name && m.IsGenericMethodDefinition then
                    let signature' = m.GetParameterTypes() |> Array.map getQualifiedName
                    signature = signature'
                else false

            try dt.GetMethods(getFlags isStatic) |> Array.find isMatchingGenericMethod |> fastUnbox<MemberInfo>
            with :? KeyNotFoundException -> 
                let msg = sprintf "Cloud not load method '%s' from type '%O'." name dt
                raise <| new FsPicklerException(msg)

        | Constructor (dt, cParams) -> dt.GetConstructor(getFlags false, null, cParams, [||]) |> fastUnbox<MemberInfo>
        | Property (dt, name, isStatic) -> dt.GetProperty(name, getFlags isStatic) |> fastUnbox<MemberInfo>
        | Field(dt, name, isStatic) -> dt.GetField(name, getFlags isStatic) |> fastUnbox<MemberInfo>
        | Event(dt, name, isStatic) -> dt.GetEvent(name, getFlags isStatic) |> fastUnbox<MemberInfo>

        | Unknown (t, name) -> invalidOp <| sprintf "Cannot load '%s' of type %O." name t


    // qualified name generator that emulates Type.ToString() but respects ITypeConverter changes
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

    #nowarn "40"

    
    [<AutoSerializable(false)>]
    type ReflectionCache private (?tyConv : ITypeNameConverter) =

        static let defaultInstance = lazy (new ReflectionCache())

        let loadAssembly = memoize loadAssembly
        let getAssemblyInfo = memoize AssemblyInfo.OfAssembly

        let rec memberInfoCache = new BiMemoizer<_,_>(getMember, loadMember)
        and getMember m   = getMemberInfo tyConv getAssemblyInfo getQualifiedName m
        and loadMember mI = loadMemberInfo tyConv loadAssembly getQualifiedName mI
        and getQualifiedName = memoize (fun t -> generateQualifiedName memberInfoCache.F t)

        member __.GetAssemblyInfo(a : Assembly) = getAssemblyInfo a
        member __.LoadAssembly(aI : AssemblyInfo) = loadAssembly aI
        member __.GetCompositeMemberInfo(m : MemberInfo) = memberInfoCache.F m
        member __.LoadMemberInfo(m : CompositeMemberInfo) = memberInfoCache.G m
        member __.GetQualifiedName(t : Type) = getQualifiedName t

        static member Create(?tyConv : ITypeNameConverter) =
            match tyConv with
            | None -> defaultInstance.Value
            | Some tc -> new ReflectionCache(tc)