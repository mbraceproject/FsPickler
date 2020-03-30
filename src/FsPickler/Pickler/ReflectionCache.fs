module internal MBrace.FsPickler.ReflectionCache

open System
open System.IO
open System.Text
open System.Reflection
open System.Collections.Generic
open System.Collections.Concurrent

open MBrace.FsPickler
open MBrace.FsPickler.Reflection

/// Contains breakdown information for a MemberInfo instance
/// This information can be memoized for performance and
/// is sufficient to restructure the instance at deserialization.

type CompositeMemberInfo =
    // System.Type breakdown
    | NamedType of name:string * assembly:AssemblyInfo
    | Array of elementType:Type
    | ArrayMultiDimensional of elementType:Type * rank:int
    | Pointer of elementType:Type
    | ByRef of elementType:Type
    | GenericTypeInstance of genericType:Type * tyArgs:Type []
    | GenericTypeParam of genericType:Type * index:int
    | GenericMethodParam of declaringMethod:MethodInfo * index:int
    // System.MethodInfo breakdown
    | Method of declaringType:Type * reflectedType:Type option * signature:string * isStatic:bool
    | GenericMethodInstance of declaringMethod:MethodInfo * tyArgs:Type []
    // misc MemberInfo
    | Constructor of declaringType:Type * isStatic:bool * tyArgs:Type []
    | Property of declaringType:Type * reflectedType:Type option * name:string * isStatic:bool
    | Field of declaringType:Type * reflectedType:Type option * name:string * isStatic:bool
    | Event of declaringType:Type * reflectedType:Type option * name:string * isStatic:bool
    // Unknown instance ; cannot serialize
    | Unknown of declaringType:Type * id:string


// Assembly Loader

// AssemblyLoadContext is not available in netstandard2.0
// Use reflection to access its APIs
let currentLoadContext = lazy(
    match Type.GetType "System.Runtime.Loader.AssemblyLoadContext" with
    | null -> None
    | ty ->
        match ty.GetProperty "Assemblies" with
        | null -> None // property not available in netcoreapp < 3.0
        | assembliesProperty ->
            let loadContextM = ty.GetMethod("GetLoadContext", BindingFlags.Public ||| BindingFlags.Static, null, [|typeof<Assembly>|], null)
            let loadFromNameM = ty.GetMethod("LoadFromAssemblyName",  BindingFlags.Public ||| BindingFlags.Instance, null, [|typeof<AssemblyName>|], null)
            let asm = Assembly.GetExecutingAssembly()
            let currentLoadContext = loadContextM.Invoke(null, [|asm|])
            let getContextAssemblies () = assembliesProperty.GetValue(currentLoadContext) :?> seq<Assembly> |> Seq.toArray
            let loadFromAssemblyName (an : AssemblyName) = loadFromNameM.Invoke(currentLoadContext, [|an|]) :?> Assembly
            Some {| LoadContext = currentLoadContext ; GetCurrentAssemblies = getContextAssemblies ; LoadFromAssemblyName = loadFromAssemblyName |})

let getAssembly enableAssemblyLoading (aI : AssemblyInfo) =
    let an = aI.ToAssemblyName()

    let loadedAssemblies =
        match currentLoadContext.Value with
        | None -> System.AppDomain.CurrentDomain.GetAssemblies()
        | Some ctx -> ctx.GetCurrentAssemblies()

    let loadedAssembly =
        loadedAssemblies
        |> Array.tryFind (fun a -> a.FullName = an.FullName)

    match loadedAssembly with
    | Some la -> la
    | None when enableAssemblyLoading -> 
        // resort to CLR loader mechanism if nothing found.
        match currentLoadContext.Value with
        | None -> Assembly.Load an
        | Some ctx -> ctx.LoadFromAssemblyName an
    | None ->
        let msg = sprintf "Could not load file or assembly '%O' or one of its dependencies. The system cannot find the file specified." an
        raise <| new FileNotFoundException(msg)

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
                Array et
            else
                ArrayMultiDimensional (et, rk)

        elif t.IsPointer then
            let et = t.GetElementType()
            Pointer et

        elif t.IsByRef then
            let et = t.GetElementType()
            ByRef et

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
            let name = match t.FullName with null -> t.Name | name -> name
            let assemblyInfo = 
                match t.Assembly with 
                | null -> raise <| new FileNotFoundException(sprintf "Cloud not identify assembly for type '%O'." t)
                | a -> getAssemblyInfo a

            let tI = { Name = name ; AssemblyInfo = assemblyInfo }
            let tI' =
                match tyConv with
                | None -> tI
                | Some tc -> tc.OfSerializedType tI

            NamedType(tI'.Name, tI'.AssemblyInfo)

    | :? MethodInfo as m ->
        if m.DeclaringType = null then
            let message = sprintf "global method '%O' in assembly '%s'" m m.Module.Assembly.FullName
            raise <| NonSerializableTypeException(m.GetType(), message)

        elif m.IsGenericMethod && not m.IsGenericMethodDefinition then
            let gm = m.GetGenericMethodDefinition()
            if runsOnMono && gm = m then 
                // address an extremely rare mono reflection bug in which certain 
                // generic method definitions are reported as not being such. 
                // An example would be the current instance of CompositePickler.Unpack<'R>
                let signature = getMethodSignature m
                Method(m.DeclaringType, getReflectedType m, signature, m.IsStatic)
            else
                let mParams = m.GetGenericArguments()
                GenericMethodInstance(gm, mParams)
        else
            let signature = getMethodSignature m
            Method(m.DeclaringType, getReflectedType m, signature, m.IsStatic)

    | :? ConstructorInfo as ctor ->
        let dt = ctor.DeclaringType 
        let ps = ctor.GetParameterTypes()
        Constructor(dt, ctor.IsStatic, ps)

    | :? PropertyInfo as p ->
        let dt = p.DeclaringType
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
                    (getAssembly : bool -> AssemblyInfo -> Assembly) 
                    (getMethodSignature : MethodInfo -> string) 
                    (enableAssemblyLoading : bool) (mI : CompositeMemberInfo) =

    let inline getFlags isStatic =
        let allVisibility = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.FlattenHierarchy
        allVisibility ||| (if isStatic then BindingFlags.Static else BindingFlags.Instance)

    match mI with
    | NamedType(name, aI) ->
        let tI = { Name = name ; AssemblyInfo = aI }
        let tI' =
            match tyConv with
            | None -> tI
            | Some tc -> tc.ToDeserializedType tI

        let assembly = getAssembly enableAssemblyLoading tI'.AssemblyInfo
        assembly.GetType(tI'.Name, throwOnError = true) |> fastUnbox<MemberInfo>

    | Array et -> et.MakeArrayType() |> fastUnbox<MemberInfo>
    | ArrayMultiDimensional (et, rk) -> et.MakeArrayType(rk) |> fastUnbox<MemberInfo>
    | Pointer et -> et.MakePointerType() |> fastUnbox<MemberInfo>
    | ByRef et -> et.MakeByRefType() |> fastUnbox<MemberInfo>

    | GenericTypeInstance(dt, tyArgs) -> dt.MakeGenericType tyArgs |> fastUnbox<MemberInfo>
    | GenericTypeParam(dt, idx) -> dt.GetGenericArguments().[idx] |> fastUnbox<MemberInfo>
    | GenericMethodParam(dm, idx) -> dm.GetGenericArguments().[idx] |> fastUnbox<MemberInfo>

    | Method(dt, reflectedType, signature, isStatic) ->
        let isMatchingMethod (m : MethodInfo) = 
            m.DeclaringType = dt && getMethodSignature m = signature

        let qt = match reflectedType with None -> dt | Some r -> r

        try qt.GetMethods(getFlags isStatic) |> Array.find isMatchingMethod |> fastUnbox<MemberInfo>
        with :? KeyNotFoundException -> 
            let msg = sprintf "Cloud not locate method '%s' in type '%O'." signature dt
            raise <| new MissingMethodException(msg)

    | GenericMethodInstance(gm, mParams) -> gm.MakeGenericMethod mParams |> fastUnbox<MemberInfo>

    | Constructor (dt, isStatic, cParams) -> dt.GetConstructor(getFlags isStatic, null, cParams, [||]) |> fastUnbox<MemberInfo>
    | Property (dt, None, name, isStatic) -> dt.GetProperty(name, getFlags isStatic) |> fastUnbox<MemberInfo>
    | Field(dt, None, name, isStatic) -> dt.GetField(name, getFlags isStatic) |> fastUnbox<MemberInfo>
    | Event(dt, None, name, isStatic) -> dt.GetEvent(name, getFlags isStatic) |> fastUnbox<MemberInfo>

    | Property (dt, Some rt, name, isStatic) ->
        try 
            rt.GetProperties(getFlags isStatic) |> Array.find(fun p -> p.Name = name && p.DeclaringType = dt) |> fastUnbox<MemberInfo>
        with :? KeyNotFoundException ->
            let msg = sprintf "Cloud not locate property '%s' in type '%O'." name dt
            raise <| new MissingMemberException(msg)

    | Field (dt, Some rt, name, isStatic) ->
        try 
            rt.GetFields(getFlags isStatic) |> Array.find(fun f -> f.Name = name && f.DeclaringType = dt) |> fastUnbox<MemberInfo>
        with :? KeyNotFoundException ->
            let msg = sprintf "Cloud not load field '%s' from type '%O'." name dt
            raise <| new MissingFieldException(msg)

    | Event (dt, Some rt, name, isStatic) ->
        try 
            rt.GetEvents(getFlags isStatic) |> Array.find(fun e -> e.Name = name && e.DeclaringType = dt) |> fastUnbox<MemberInfo>
        with :? KeyNotFoundException ->
            let msg = sprintf "Cloud not load event '%s' from type '%O'." name dt
            raise <| new MissingMemberException(msg)

    | Unknown (t, name) -> invalidOp <| sprintf "Cannot load '%s' of type %O." name t


// type signature generator that emulates Type.ToString() but respects ITypeNameConverter rules
let generateTypeSignature (getInfo : Type -> CompositeMemberInfo) (t : Type) =
    let rec generate (sb : StringBuilder) (t : Type) =
        let inline append (x : string) = sb.Append x |> ignore
        match getInfo t with
        | NamedType(name, _) -> append name
        | Array et -> generate sb et ; append "[]"
        | ArrayMultiDimensional(et, 1) -> generate sb et ; append "[*]"
        | ArrayMultiDimensional(et, rk) ->
            generate sb et
            append "["
            for i = 1 to rk - 1 do append ","
            append "]"

        | Pointer et ->
            generate sb et
            append "*"

        | ByRef et ->
            generate sb et
            append "&"

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
    
[<AutoSerializable(false)>]
type ReflectionCache private (?tyConv : ITypeNameConverter) =

    static let defaultCache = lazy(new ReflectionCache())
    static let caches = new ConcurrentDictionary<ITypeNameConverter, ReflectionCache>(new ReferenceEqualityComparer<_>())

    let getAssemblyMemoized = memoizeParam getAssembly
    let getAssemblyInfoMemoized = memoize AssemblyInfo.OfAssembly

    let rec getMemberMemoized : MemberInfo -> CompositeMemberInfo = 
        memoize (getMemberInfo tyConv getAssemblyInfoMemoized (generateMethodSignature getSignatureMemoized))

    and loadMemberMemoized = 
        memoizeParam (loadMemberInfo tyConv getAssemblyMemoized (generateMethodSignature getSignatureMemoized))

    and getSignatureMemoized = 
        memoize (fun t -> generateTypeSignature getMemberMemoized t)

    member __.GetAssemblyInfo(a : Assembly) = getAssemblyInfoMemoized a
    member __.LoadAssembly(aI : AssemblyInfo, enableAssemblyLoading : bool) = getAssemblyMemoized enableAssemblyLoading aI
    member __.GetCompositeMemberInfo(m : MemberInfo) = getMemberMemoized m
    member __.LoadMemberInfo(m : CompositeMemberInfo, enableAssemblyLoading : bool) = loadMemberMemoized enableAssemblyLoading m
    member __.GetTypeSignature(t : Type) = getSignatureMemoized t

    static member Create(?tyConv : ITypeNameConverter) =
        match tyConv with
        | None -> defaultCache.Value
        | Some tc -> caches.GetOrAdd(tc, fun tc -> new ReflectionCache(tyConv = tc))