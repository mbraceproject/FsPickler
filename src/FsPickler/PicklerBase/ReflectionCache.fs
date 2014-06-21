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
        | NamedType of TypeInfo
        | ArrayType of Type * (* rank *) int option
        | GenericType of Type * Type []
        | GenericTypeParam of Type * int
        | GenericMethodParam of MethodInfo * int
        // System.MethodInfo breakdown
        | Method of Type * (*reflected type*) Type option * (* name *) string * (* isStatic *) bool * Type []
        | GenericMethod of MethodInfo * Type []
        // in generic method definitions, encode as signature
        // this is to avoid recursive situations in method overload resolutions
        | GenericMethodDefinition of Type * (*reflected type*) Type option * (* name *) string * (* isStatic *) bool * (*params*) string []
        // misc MemberInfo
        | Constructor of Type * (*isStatic*) bool * (* params *) Type []
        | Property of Type * (*reflected type*) Type option * (* name *) string * (* isStatic *) bool
        | Field of Type * (*reflected type*) Type option * (* name *) string * (* isStatic *) bool
        | Event of Type * (*reflected type*) Type option * (* name *) string * (* isStatic *) bool
        | Unknown of Type * string
    
    // Assembly Loading Code

    let loadAssembly (assemblyQualifiedName : string) =
        try Assembly.Load assemblyQualifiedName
        with :? FileNotFoundException | :? FileLoadException as e ->

            // in cases of assemblies loaded from reflection at runtime, Assembly.Load may fail.
            // Attempt a direct query on the AppDomain to resolve this.

            let result =
                System.AppDomain.CurrentDomain.GetAssemblies()
                |> Array.tryFind (fun a -> a.FullName = assemblyQualifiedName)

            match result with
            | None -> raise <| new FsPicklerException("FsPickler: Assembly load error.", e)
            | Some a -> a


    //
    //  MemberInfo Loading Code
    //
        
    // converts a memberInfo instance to a CompositeMemberInfo structure
    let getMemberInfo (tyConv : ITypeNameConverter option) 
//                        (getAssemblyInfo : Assembly -> AssemblyInfo)
                        (getQualifiedName : Type -> string) (m : MemberInfo) =

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
                GenericType(gt, gas)

            elif t.IsGenericParameter then
                match t.DeclaringMethod with
                | null -> GenericTypeParam(t.DeclaringType, t.GenericParameterPosition)
                | dm -> GenericMethodParam(dm :?> MethodInfo, t.GenericParameterPosition)
            else
                // is named type, no generic arguments
                let name =
                    match t.FullName with null -> t.Name | name -> name

                let tI = { Name = name ; AssemblyQualifiedName = t.Assembly.FullName }
                let tI' =
                    match tyConv with
                    | None -> tI
                    | Some tc -> tc.OfSerializedType tI

                NamedType tI'

        | :? MethodInfo as m ->
            if m.IsGenericMethod then
                if m.IsGenericMethodDefinition then
                    let mParams = m.GetParameterTypes() |> Array.map getQualifiedName
                    GenericMethodDefinition(m.DeclaringType, getReflectedType m, m.Name, m.IsStatic, mParams)
                else
                    let gm = m.GetGenericMethodDefinition()
                    let tyArgs = m.GetGenericArguments()
                    GenericMethod(gm, tyArgs)
            else
                let mParams = m.GetParameters() |> Array.map (fun p -> p.ParameterType)
                Method(m.DeclaringType, getReflectedType m, m.Name, m.IsStatic, mParams)

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
                        (loadAssembly : string -> Assembly) 
                        (getQualifiedName : Type -> string) (mI : CompositeMemberInfo) =

        let inline getFlags isStatic =
            let allVisibility = BindingFlags.Public ||| BindingFlags.NonPublic
            allVisibility ||| (if isStatic then BindingFlags.Static else BindingFlags.Instance)

        match mI with
        | NamedType tI ->
            let tI' =
                match tyConv with
                | None -> tI
                | Some tc -> tc.ToDeserializedType tI

            let assembly = loadAssembly tI'.AssemblyQualifiedName
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
        | GenericMethod(gm, tyParams) -> gm.MakeGenericMethod tyParams |> fastUnbox<MemberInfo>
        | Method(dt, None, name, isStatic, mParams) ->  dt.GetMethod(name, getFlags isStatic, null, mParams, [||]) |> fastUnbox<MemberInfo>
        | Method(dt, Some rt, name, isStatic, mParams) ->
            try
                rt.GetMethods(getFlags isStatic) 
                |> Array.find (fun m -> m.Name = name && m.DeclaringType = dt && m.GetParameterTypes() = mParams)
                |> fastUnbox<MemberInfo>

            with :? KeyNotFoundException ->
                let msg = sprintf "Cloud not load method '%s' from type '%O'." name dt
                raise <| new FsPicklerException(msg)

        | GenericMethodDefinition(dt, None, name, isStatic, signature) ->
            let isMatchingGenericMethod (m : MethodInfo) =
                if m.Name = name && m.IsGenericMethodDefinition then
                    let signature' = m.GetParameterTypes() |> Array.map getQualifiedName
                    signature = signature'
                else false

            try dt.GetMethods(getFlags isStatic) |> Array.find isMatchingGenericMethod |> fastUnbox<MemberInfo>
            with :? KeyNotFoundException -> 
                let msg = sprintf "Cloud not load method '%s' from type '%O'." name dt
                raise <| new FsPicklerException(msg)

        | GenericMethodDefinition(dt, Some rt, name, isStatic, signature) ->
            let isMatchingGenericMethod (m : MethodInfo) =
                if m.Name = name && m.IsGenericMethodDefinition && m.DeclaringType = dt then
                    let signature' = m.GetParameterTypes() |> Array.map getQualifiedName
                    signature = signature'
                else false

            try rt.GetMethods(getFlags isStatic) |> Array.find isMatchingGenericMethod |> fastUnbox<MemberInfo>
            with :? KeyNotFoundException -> 
                let msg = sprintf "Cloud not load method '%s' from type '%O'." name dt
                raise <| new FsPicklerException(msg)

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


    // qualified name generator that emulates Type.ToString() but respects ITypeConverter changes
    let generateTypeName (getInfo : Type -> CompositeMemberInfo) (t : Type) =
        let rec generate (b : StringBuilder) (t : Type) =
            let inline append (x : string) = b.Append x |> ignore
            match getInfo t with
            | NamedType tI -> append tI.Name
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

        let rec memberInfoCache = new BiMemoizer<_,_>(getMember, loadMember)
        and getMember m   = getMemberInfo tyConv getTypeName m
        and loadMember mI = loadMemberInfo tyConv loadAssembly getTypeName mI
        and getTypeName = memoize (fun t -> generateTypeName memberInfoCache.F t)

        member __.LoadAssembly(qualifiedName : string) = loadAssembly qualifiedName
        member __.GetCompositeMemberInfo(m : MemberInfo) = memberInfoCache.F m
        member __.LoadMemberInfo(m : CompositeMemberInfo) = memberInfoCache.G m
        member __.GetTypeName(t : Type) = getTypeName t

        static member Create(?tyConv : ITypeNameConverter) =
            match tyConv with
            | None -> defaultInstance.Value
            | Some tc -> new ReflectionCache(tc)