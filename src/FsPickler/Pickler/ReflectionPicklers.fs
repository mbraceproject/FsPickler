module internal Nessos.FsPickler.ReflectionPicklers

open System
open System.IO
open System.Reflection
open System.Collections.Concurrent

open Nessos.FsPickler
open Nessos.FsPickler.Reflection
open Nessos.FsPickler.ReflectionCache
open Nessos.FsPickler.PrimitivePicklers

type IArrayPickler =
    abstract Create : Pickler<'T> -> Pickler<'T []>

let mkReflectionPicklers (arrayPickler : IArrayPickler) =
    let assemblyInfoPickler =
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

        new CompositePickler<_>(reader, writer, (fun _ aI -> aI), ignore2, PicklerInfo.ReflectionType, cacheByRef = true)

    let assemblyPickler =
        CompositePickler.Create(
            (fun r t -> let aI = assemblyInfoPickler.Reader r t in r.ReflectionCache.LoadAssembly aI),
            (fun w t a -> let aI = w.ReflectionCache.GetAssemblyInfo a in assemblyInfoPickler.Writer w t aI),
            (fun _ a -> a), (fun _ _ -> ()), PicklerInfo.ReflectionType, cacheByRef = true, useWithSubtypes = true)

    let assemblyNamePickler =
        CompositePickler.Create(
            (fun r t -> let aI = assemblyInfoPickler.Reader r t in aI.ToAssemblyName()),
            (fun w t an -> let aI = AssemblyInfo.OfAssemblyName an in assemblyInfoPickler.Writer w t aI),
            (fun _ an -> an.Clone() |> fastUnbox<_>), ignore2,
                PicklerInfo.ReflectionType, cacheByRef = true, useWithSubtypes = true)

    let tagSerializer = UnionCaseSerializationHelper.OfUnionType<CompositeMemberInfo> ()

    let rec memberInfoWriter (w : WriteState) (_ : string) (m : MemberInfo) =
        let formatter = w.Formatter

        // note: order of cases must be kept same as type definition
        // so that tag assignments correspond to internal union tag
        // as this is the order defined in the UnionCaseSerializationHelper.
        match w.ReflectionCache.GetCompositeMemberInfo m with
        | NamedType (name, aI) ->
            tagSerializer.WriteTag(formatter, 0)

            formatter.WriteString "Name" name
            assemblyInfoPickler.Write w "Assembly" aI

        | Array et ->
            tagSerializer.WriteTag(formatter, 1)
            typePickler.Write w "ElementType" et

        | ArrayMultiDimensional (et, rk) ->
            tagSerializer.WriteTag(formatter, 2)
            typePickler.Write w "ElementType" et
            formatter.WriteInt32 "Rank" rk

        | Pointer et ->
            tagSerializer.WriteTag(formatter, 3)
            typePickler.Write w "ElementType" et

        | ByRef et ->
            tagSerializer.WriteTag(formatter, 4)
            typePickler.Write w "ElementType" et

        | GenericTypeInstance(dt, tyArgs) ->
            tagSerializer.WriteTag(formatter, 5)
            typePickler.Write w "GenericDefinition" dt
            typeArrayPickler.Write w "TypeArgs" tyArgs

        | GenericTypeParam(dt, idx) ->
            tagSerializer.WriteTag(formatter, 6)
            typePickler.Write w "DeclaringType" dt
            formatter.WriteInt32 "Index" idx

        | GenericMethodParam(dm, idx) ->
            tagSerializer.WriteTag(formatter, 7)
            methodInfoPickler.Write w "DeclaringMethod" dm
            formatter.WriteInt32 "Index" idx

        | Method(dt, rt, signature, isStatic) ->
            tagSerializer.WriteTag(formatter, 8)

            formatter.WriteString "Signature" signature
            formatter.WriteBoolean "IsStatic" isStatic

            typePickler.Write w "DeclaringType" dt
            typePickler.Write w "ReflectedType" (defaultArg rt null)

        | GenericMethodInstance(gm, tyArgs) ->
            tagSerializer.WriteTag(formatter, 9)

            methodInfoPickler.Write w "GenericDefinition" gm
            typeArrayPickler.Write w "TypeArgs" tyArgs

        | Constructor(dt, isStatic, cParams) ->
            tagSerializer.WriteTag(formatter, 10)

            typePickler.Write w "DeclaringType" dt
            formatter.WriteBoolean "IsStatic" isStatic
            typeArrayPickler.Write w "Params" cParams

        | Property(dt, rt, name, isStatic) ->
            tagSerializer.WriteTag(formatter, 11)

            formatter.WriteString "Name" name
            formatter.WriteBoolean "IsStatic" isStatic

            typePickler.Write w "DeclaringType" dt
            typePickler.Write w "ReflectedType" (defaultArg rt null)

        | Field(dt, rt, name, isStatic) ->
            tagSerializer.WriteTag(formatter, 12)

            formatter.WriteString "Name" name
            formatter.WriteBoolean "IsStatic" isStatic

            typePickler.Write w "DeclaringType" dt
            typePickler.Write w "ReflectedType" (defaultArg rt null)

        | Event(dt, rt, name, isStatic) ->
            tagSerializer.WriteTag(formatter, 13)

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
            | 0 ->
                let name = formatter.ReadString "Name"
                let assembly = assemblyInfoPickler.Read r "Assembly"
                NamedType(name, assembly)
                
            | 1 ->
                let et = typePickler.Read r "ElementType"
                Array(et)

            | 2 ->
                let et = typePickler.Read r "ElementType"
                let rk = formatter.ReadInt32 "Rank"
                ArrayMultiDimensional(et, rk)

            | 3 ->
                let et = typePickler.Read r "ElementType"
                Pointer(et)

            | 4 ->
                let et = typePickler.Read r "ElementType"
                ByRef(et)

            | 5 ->
                let gt = typePickler.Read r "GenericDefinition"
                let tyArgs = typeArrayPickler.Read r "TypeArgs"
                GenericTypeInstance(gt, tyArgs)

            | 6 ->
                let dt = typePickler.Read r "DeclaringType"
                let idx = formatter.ReadInt32 "Index"
                GenericTypeParam(dt, idx)

            | 7 ->
                let dm = methodInfoPickler.Read r "DeclaringMethod"
                let idx = formatter.ReadInt32 "Index"
                GenericMethodParam(dm, idx)

            | 8 ->
                let signature = formatter.ReadString "Signature"
                let isStatic = formatter.ReadBoolean "IsStatic"

                let dt = typePickler.Read r "DeclaringType"
                let rt = denull <| typePickler.Read r "ReflectedType"

                Method(dt, rt, signature, isStatic)

            | 9 ->
                let gm = methodInfoPickler.Read r "GenericDefinition"
                let tyArgs = typeArrayPickler.Read r "TypeArgs"
                GenericMethodInstance(gm, tyArgs)

            | 10 ->
                let dt = typePickler.Read r "DeclaringType"
                let isStatic = formatter.ReadBoolean "IsStatic"
                let cParams = typeArrayPickler.Read r "Params"
                Constructor(dt, isStatic, cParams)

            | 11 ->
                let name = formatter.ReadString "Name"
                let isStatic = formatter.ReadBoolean "IsStatic"

                let dt = typePickler.Read r "DeclaringType"
                let rt = denull <| typePickler.Read r "ReflectedType"

                Property(dt, rt, name, isStatic)

            | 12 ->
                let name = formatter.ReadString "Name"
                let isStatic = formatter.ReadBoolean "IsStatic"

                let dt = typePickler.Read r "DeclaringType"
                let rt = denull <| typePickler.Read r "ReflectedType"

                Field(dt, rt, name, isStatic)

            | 13 ->
                let name = formatter.ReadString "Name"
                let isStatic = formatter.ReadBoolean "IsStatic"

                let dt = typePickler.Read r "DeclaringType"
                let rt = denull <| typePickler.Read r "ReflectedType"

                Event(dt, rt, name, isStatic)

            // 'Unknown' cases never get serialized; this is a case of invalid data
            | _ -> raise <| new InvalidDataException("invalid member type.")


        r.ReflectionCache.LoadMemberInfo cMemberInfo

    and memberInfoPickler = 
        CompositePickler.Create(memberInfoReader, memberInfoWriter, (fun _ mI -> mI), ignore2, PicklerInfo.ReflectionType, useWithSubtypes = true, cacheByRef = true)

    and typePickler : Pickler<Type> = memberInfoPickler.Cast<Type> ()
    and methodInfoPickler : Pickler<MethodInfo> = memberInfoPickler.Cast<MethodInfo> ()
    and typeArrayPickler : Pickler<Type []> = arrayPickler.Create typePickler

    [|
        assemblyPickler :> Pickler
        assemblyNamePickler :> _
        methodInfoPickler :> _
        memberInfoPickler :> _
        typePickler :> _
    |]