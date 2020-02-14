module internal MBrace.FsPickler.ReflectionPicklers

open System
open System.Reflection

open MBrace.FsPickler
open MBrace.FsPickler.ReflectionCache

[<RequireQualifiedAccess>]
module private MemberTag =
    let [<Literal>] NamedType = 0
    let [<Literal>] Array = 1
    let [<Literal>] ArrayMultiDimensional = 2
    let [<Literal>] Pointer = 3
    let [<Literal>] ByRef = 4
    let [<Literal>] GenericTypeInstance = 5
    let [<Literal>] GenericTypeParam = 6
    let [<Literal>] GenericMethodParam = 7
    let [<Literal>] Method = 8
    let [<Literal>] GenericMethodInstance = 9
    let [<Literal>] Constructor = 10
    let [<Literal>] Property = 11
    let [<Literal>] Field = 12
    let [<Literal>] Event = 13

//let mkReflectionPicklers (arrayPickler : IArrayPickler) =
type ReflectionPicklers private () =
    static let tagSerializer = UnionCaseSerializationHelper.OfUnionType<CompositeMemberInfo> ()
    static member CreateAssemblyInfoPickler() : Pickler<AssemblyInfo> =
        let writer (w : WriteState) (_ : string) (aI : AssemblyInfo) =
            let formatter = w.Formatter
            formatter.WriteString "Name" aI.Name
            formatter.WriteString "Version" aI.Version
            formatter.WriteString "Culture" aI.Culture
            formatter.WriteString "PublicKeyToken" aI.PublicKeyToken

        let reader (r : ReadState) (_ : string) =
            let formatter = r.Formatter
            let name = formatter.ReadString "Name"
            let version = formatter.ReadString "Version"
            let culture = formatter.ReadString "Culture" 
            let pkt = formatter.ReadString "PublicKeyToken"

            {
                Name = name
                Version = version
                Culture = culture
                PublicKeyToken = pkt
            }

        CompositePickler.Create(reader, writer, (fun _ aI -> aI), ignore2, PicklerInfo.ReflectionType, cacheByRef = true)

    static member CreateAssemblyPickler(resolver : IPicklerResolver) : Pickler<Assembly> =
        let assemblyInfoPickler = resolver.Resolve<AssemblyInfo>() :?> CompositePickler<AssemblyInfo>
        CompositePickler.Create(
            (fun r t -> let aI = assemblyInfoPickler.Reader r t in r.ReflectionCache.LoadAssembly(aI, not r.DisableAssemblyLoading)),
            (fun w t a -> let aI = w.ReflectionCache.GetAssemblyInfo a in assemblyInfoPickler.Writer w t aI),
            (fun _ a -> a), (fun _ _ -> ()), PicklerInfo.ReflectionType, cacheByRef = true, useWithSubtypes = true)

    static member CreateAssemblyNamePickler(resolver : IPicklerResolver) : Pickler<AssemblyName> =
        let assemblyInfoPickler = resolver.Resolve<AssemblyInfo>() :?> CompositePickler<AssemblyInfo>
        CompositePickler.Create(
            (fun r t -> let aI = assemblyInfoPickler.Reader r t in aI.ToAssemblyName()),
            (fun w t an -> let aI = AssemblyInfo.OfAssemblyName an in assemblyInfoPickler.Writer w t aI),
            (fun _ an -> an.Clone() |> fastUnbox<_>), ignore2,
                PicklerInfo.ReflectionType, cacheByRef = true, useWithSubtypes = true)

    static member CreateMemberInfoPickler (arrayPickler : Pickler<Type> -> Pickler<Type[]>) (resolver : IPicklerResolver) : Pickler<MemberInfo> =
        let assemblyInfoPickler = resolver.Resolve<AssemblyInfo>()

        let rec memberInfoWriter (w : WriteState) (_ : string) (m : MemberInfo) =
            let formatter = w.Formatter

            // note: order of cases must be kept same as type definition
            // so that tag assignments correspond to internal union tag
            // as this is the order defined in the UnionCaseSerializationHelper.
            match w.ReflectionCache.GetCompositeMemberInfo m with
            | NamedType (name, aI) ->
                tagSerializer.WriteTag(formatter, MemberTag.NamedType)

                formatter.WriteString "Name" name
                assemblyInfoPickler.Write w "Assembly" aI

            | Array et ->
                tagSerializer.WriteTag(formatter, MemberTag.Array)
                typePickler.Write w "ElementType" et

            | ArrayMultiDimensional (et, rk) ->
                tagSerializer.WriteTag(formatter, MemberTag.ArrayMultiDimensional)
                typePickler.Write w "ElementType" et
                formatter.WriteInt32 "Rank" rk

            | Pointer et ->
                tagSerializer.WriteTag(formatter, MemberTag.Pointer)
                typePickler.Write w "ElementType" et

            | ByRef et ->
                tagSerializer.WriteTag(formatter, MemberTag.ByRef)
                typePickler.Write w "ElementType" et

            | GenericTypeInstance(dt, tyArgs) ->
                tagSerializer.WriteTag(formatter, MemberTag.GenericTypeInstance)
                typePickler.Write w "GenericDefinition" dt
                typeArrayPickler.Write w "TypeArgs" tyArgs

            | GenericTypeParam(dt, idx) ->
                tagSerializer.WriteTag(formatter, MemberTag.GenericTypeParam)
                typePickler.Write w "DeclaringType" dt
                formatter.WriteInt32 "Index" idx

            | GenericMethodParam(dm, idx) ->
                tagSerializer.WriteTag(formatter, MemberTag.GenericMethodParam)
                methodInfoPickler.Write w "DeclaringMethod" dm
                formatter.WriteInt32 "Index" idx

            | Method(dt, rt, signature, isStatic) ->
                tagSerializer.WriteTag(formatter, MemberTag.Method)

                formatter.WriteString "Signature" signature
                formatter.WriteBoolean "IsStatic" isStatic

                typePickler.Write w "DeclaringType" dt
                typePickler.Write w "ReflectedType" (defaultArg rt null)

            | GenericMethodInstance(gm, tyArgs) ->
                tagSerializer.WriteTag(formatter, MemberTag.GenericMethodInstance)

                methodInfoPickler.Write w "GenericDefinition" gm
                typeArrayPickler.Write w "TypeArgs" tyArgs

            | Constructor(dt, isStatic, cParams) ->
                tagSerializer.WriteTag(formatter, MemberTag.Constructor)

                typePickler.Write w "DeclaringType" dt
                formatter.WriteBoolean "IsStatic" isStatic
                typeArrayPickler.Write w "Params" cParams

            | Property(dt, rt, name, isStatic) ->
                tagSerializer.WriteTag(formatter, MemberTag.Property)

                formatter.WriteString "Name" name
                formatter.WriteBoolean "IsStatic" isStatic

                typePickler.Write w "DeclaringType" dt
                typePickler.Write w "ReflectedType" (defaultArg rt null)

            | Field(dt, rt, name, isStatic) ->
                tagSerializer.WriteTag(formatter, MemberTag.Field)

                formatter.WriteString "Name" name
                formatter.WriteBoolean "IsStatic" isStatic

                typePickler.Write w "DeclaringType" dt
                typePickler.Write w "ReflectedType" (defaultArg rt null)

            | Event(dt, rt, name, isStatic) ->
                tagSerializer.WriteTag(formatter, MemberTag.Event)

                formatter.WriteString "Name" name
                formatter.WriteBoolean "IsStatic" isStatic

                typePickler.Write w "DeclaringType" dt
                typePickler.Write w "ReflectedType" (defaultArg rt null)

            | Unknown(t, name) ->
                raise <| new NonSerializableTypeException(t, sprintf "could not serialize '%s'." name)

        and memberInfoReader (r : ReadState) (_ : string) =
            let formatter = r.Formatter

            let cMemberInfo =
                match tagSerializer.ReadTag formatter with
                | MemberTag.NamedType ->
                    let name = formatter.ReadString "Name"
                    let assembly = assemblyInfoPickler.Read r "Assembly"
                    NamedType(name, assembly)
                
                | MemberTag.Array ->
                    let et = typePickler.Read r "ElementType"
                    Array(et)

                | MemberTag.ArrayMultiDimensional ->
                    let et = typePickler.Read r "ElementType"
                    let rk = formatter.ReadInt32 "Rank"
                    ArrayMultiDimensional(et, rk)

                | MemberTag.Pointer ->
                    let et = typePickler.Read r "ElementType"
                    Pointer(et)

                | MemberTag.ByRef ->
                    let et = typePickler.Read r "ElementType"
                    ByRef(et)

                | MemberTag.GenericTypeInstance ->
                    let gt = typePickler.Read r "GenericDefinition"
                    let tyArgs = typeArrayPickler.Read r "TypeArgs"
                    GenericTypeInstance(gt, tyArgs)

                | MemberTag.GenericTypeParam ->
                    let dt = typePickler.Read r "DeclaringType"
                    let idx = formatter.ReadInt32 "Index"
                    GenericTypeParam(dt, idx)

                | MemberTag.GenericMethodParam ->
                    let dm = methodInfoPickler.Read r "DeclaringMethod"
                    let idx = formatter.ReadInt32 "Index"
                    GenericMethodParam(dm, idx)

                | MemberTag.Method ->
                    let signature = formatter.ReadString "Signature"
                    let isStatic = formatter.ReadBoolean "IsStatic"

                    let dt = typePickler.Read r "DeclaringType"
                    let rt = denull <| typePickler.Read r "ReflectedType"

                    Method(dt, rt, signature, isStatic)

                | MemberTag.GenericMethodInstance ->
                    let gm = methodInfoPickler.Read r "GenericDefinition"
                    let tyArgs = typeArrayPickler.Read r "TypeArgs"
                    GenericMethodInstance(gm, tyArgs)

                | MemberTag.Constructor ->
                    let dt = typePickler.Read r "DeclaringType"
                    let isStatic = formatter.ReadBoolean "IsStatic"
                    let cParams = typeArrayPickler.Read r "Params"
                    Constructor(dt, isStatic, cParams)

                | MemberTag.Property ->
                    let name = formatter.ReadString "Name"
                    let isStatic = formatter.ReadBoolean "IsStatic"

                    let dt = typePickler.Read r "DeclaringType"
                    let rt = denull <| typePickler.Read r "ReflectedType"

                    Property(dt, rt, name, isStatic)

                | MemberTag.Field ->
                    let name = formatter.ReadString "Name"
                    let isStatic = formatter.ReadBoolean "IsStatic"

                    let dt = typePickler.Read r "DeclaringType"
                    let rt = denull <| typePickler.Read r "ReflectedType"

                    Field(dt, rt, name, isStatic)

                | MemberTag.Event ->
                    let name = formatter.ReadString "Name"
                    let isStatic = formatter.ReadBoolean "IsStatic"

                    let dt = typePickler.Read r "DeclaringType"
                    let rt = denull <| typePickler.Read r "ReflectedType"

                    Event(dt, rt, name, isStatic)

                // 'Unknown' cases never get serialized; this is a case of invalid data
                | _ -> raise <| new FormatException("invalid member type.")

            r.ReflectionCache.LoadMemberInfo(cMemberInfo, not r.DisableAssemblyLoading)

        and memberInfoPickler =
            CompositePickler.Create(memberInfoReader, memberInfoWriter, 
                                    (fun _ mI -> mI), ignore2, PicklerInfo.ReflectionType, 
                                    useWithSubtypes = true, cacheByRef = true)

        and typePickler : Pickler<Type> = memberInfoPickler.Cast<Type>()
        and typeArrayPickler : Pickler<Type []> = arrayPickler typePickler
        and methodInfoPickler : Pickler<MethodInfo> = memberInfoPickler.Cast<MethodInfo>()

        memberInfoPickler