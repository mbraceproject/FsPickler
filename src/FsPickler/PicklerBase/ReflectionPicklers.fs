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
                formatter.WriteString "name" aI.Name
                formatter.WriteString "version" aI.Version
                formatter.WriteString "culture" aI.Culture
                formatter.WriteString "publicKeyToken" aI.PublicKeyToken

            let reader (r : ReadState) (_ : string) =
                let formatter = r.Formatter
                let name = formatter.ReadString "name"
                let version = formatter.ReadString "version"
                let culture = formatter.ReadString "culture" 
                let pkt = formatter.ReadString "publicKeyToken"

                {
                    Name = name
                    Version = version
                    Culture = culture
                    PublicKeyToken = pkt
                }

            new CompositePickler<_>(reader, writer, PicklerInfo.ReflectionType, cacheByRef = true, useWithSubtypes = false)

        let assemblyPickler =
            CompositePickler.Create(
                (fun r t -> let aI = assemblyInfoPickler.Reader r t in r.ReflectionCache.LoadAssembly aI),
                (fun w t a -> let aI = w.ReflectionCache.GetAssemblyInfo a in assemblyInfoPickler.Writer w t aI),
                    PicklerInfo.ReflectionType, cacheByRef = true, useWithSubtypes = true)

        let assemblyNamePickler =
            CompositePickler.Create(
                (fun r t -> let aI = assemblyInfoPickler.Reader r t in aI.ToAssemblyName()),
                (fun w t an -> let aI = AssemblyInfo.OfAssemblyName an in assemblyInfoPickler.Writer w t aI),
                PicklerInfo.ReflectionType, cacheByRef = true, useWithSubtypes = true)

        let stringArrayPickler = arrayPickler.Create <| PrimitivePicklers.mkString()

        let caseSerializer = UnionCaseSerializationHelper.OfUnionType<CompositeMemberInfo> ()

        let rec memberInfoWriter (w : WriteState) (tag : string) (m : MemberInfo) =
            let formatter = w.Formatter

            // not used anywhere ; placed here just to assist type inference
            let inline tp () : Pickler<Type> = typePickler
            let inline tp () : Pickler<Type []> = typeArrayPickler
            let inline mp () : Pickler<MemberInfo> = memberInfoPickler
            let inline mp () : Pickler<MethodInfo> = methodInfoPickler

            // note: order of cases must be kept same as type definition
            // so that tag assignments correspond to internal union tag.
            match w.ReflectionCache.GetCompositeMemberInfo m with
            | NamedType (name, aI) ->
                caseSerializer.WriteTag(formatter, 0)

                formatter.WriteString "name" name
                assemblyInfoPickler.Write w "assembly" aI

            | ArrayType (et, rk) ->
                caseSerializer.WriteTag(formatter, 1)
                typePickler.Write w "elementType" et

                match rk with
                | None -> formatter.WriteInt32 "rank" 0
                | Some r -> formatter.WriteInt32 "rank" r

            | GenericTypeInstance(dt, tyArgs) ->
                caseSerializer.WriteTag(formatter, 2)
                typePickler.Write w "definition" dt
                typeArrayPickler.Write w "typeArgs" tyArgs

            | GenericTypeParam(dt, idx) ->
                caseSerializer.WriteTag(formatter, 3)
                typePickler.Write w "declaringType" dt
                formatter.WriteInt32 "index" idx

            | GenericMethodParam(dm, idx) ->
                caseSerializer.WriteTag(formatter, 4)
                methodInfoPickler.Write w "declaringMethod" dm
                formatter.WriteInt32 "index" idx

            | Method(dt, rt, signature, isStatic) ->
                caseSerializer.WriteTag(formatter, 5)

                formatter.WriteString "signature" signature
                formatter.WriteBoolean "isStatic" isStatic

                typePickler.Write w "declaringType" dt
                typePickler.Write w "reflectedType" (defaultArg rt null)

            | GenericMethodInstance(gm, tyArgs) ->
                caseSerializer.WriteTag(formatter, 6)

                methodInfoPickler.Write w "definition" gm
                typeArrayPickler.Write w "typeArgs" tyArgs

            | Constructor(dt, isStatic, cParams) ->
                caseSerializer.WriteTag(formatter, 7)

                typePickler.Write w "declaringType" dt
                formatter.WriteBoolean "isStatic" isStatic
                typeArrayPickler.Write w "params" cParams

            | Property(dt, rt, name, isStatic) ->
                caseSerializer.WriteTag(formatter, 8)

                formatter.WriteString "name" name
                formatter.WriteBoolean "isStatic" isStatic

                typePickler.Write w "declaringType" dt
                typePickler.Write w "reflectedType" (defaultArg rt null)

            | Field(dt, rt, name, isStatic) ->
                caseSerializer.WriteTag(formatter, 9)

                formatter.WriteString "name" name
                formatter.WriteBoolean "isStatic" isStatic

                typePickler.Write w "declaringType" dt
                typePickler.Write w "reflectedType" (defaultArg rt null)

            | Event(dt, rt, name, isStatic) ->
                caseSerializer.WriteTag(formatter, 10)

                formatter.WriteString "name" name
                formatter.WriteBoolean "isStatic" isStatic

                typePickler.Write w "declaringType" dt
                typePickler.Write w "reflectedType" (defaultArg rt null)

            | Unknown(t, name) ->
                raise <| new NonSerializableTypeException(t, sprintf "could not serialize '%s'." name)

        and memberInfoReader (r : ReadState) (tag : string) =
            let formatter = r.Formatter

            let cMemberInfo =
                match caseSerializer.ReadTag formatter with
                | 0 ->
                    let name = formatter.ReadString "name"
                    let assembly = assemblyInfoPickler.Read r "assembly"
                    NamedType(name, assembly)

                | 1 ->
                    let et = typePickler.Read r "elementType"
                    match formatter.ReadInt32 "rank" with
                    | 0 -> ArrayType(et, None)
                    | rk -> ArrayType(et, Some rk)

                | 2 ->
                    let gt = typePickler.Read r "definition"
                    let tyArgs = typeArrayPickler.Read r "typeArgs"
                    GenericTypeInstance(gt, tyArgs)

                | 3 ->
                    let dt = typePickler.Read r "declaringType"
                    let idx = formatter.ReadInt32 "index"
                    GenericTypeParam(dt, idx)

                | 4 ->
                    let dm = methodInfoPickler.Read r "declaringMethod"
                    let idx = formatter.ReadInt32 "index"
                    GenericMethodParam(dm, idx)

                | 5 ->
                    let signature = formatter.ReadString "signature"
                    let isStatic = formatter.ReadBoolean "isStatic"

                    let dt = typePickler.Read r "declaringType"
                    let rt = denull <| typePickler.Read r "reflectedType"

                    Method(dt, rt, signature, isStatic)

                | 6 ->
                    let gm = methodInfoPickler.Read r "definition"
                    let tyArgs = typeArrayPickler.Read r "typeArgs"
                    GenericMethodInstance(gm, tyArgs)

                | 7 ->
                    let dt = typePickler.Read r "declaringType"
                    let isStatic = formatter.ReadBoolean "isStatic"
                    let cParams = typeArrayPickler.Read r "params"
                    Constructor(dt, isStatic, cParams)

                | 8 ->
                    let name = formatter.ReadString "name"
                    let isStatic = formatter.ReadBoolean "isStatic"

                    let dt = typePickler.Read r "declaringType"
                    let rt = denull <| typePickler.Read r "reflectedType"

                    Property(dt, rt, name, isStatic)

                | 9 ->
                    let name = formatter.ReadString "name"
                    let isStatic = formatter.ReadBoolean "isStatic"

                    let dt = typePickler.Read r "declaringType"
                    let rt = denull <| typePickler.Read r "reflectedType"

                    Field(dt, rt, name, isStatic)

                | 10 ->
                    let name = formatter.ReadString "name"
                    let isStatic = formatter.ReadBoolean "isStatic"

                    let dt = typePickler.Read r "declaringType"
                    let rt = denull <| typePickler.Read r "reflectedType"

                    Event(dt, rt, name, isStatic)

                // 'Unknown' cases never get serialized; this is a case of invalid data
                | _ -> raise <| new InvalidDataException("invalid member type.")


            r.ReflectionCache.LoadMemberInfo cMemberInfo

        and memberInfoPickler = 
            CompositePickler.Create(memberInfoReader, memberInfoWriter, PicklerInfo.ReflectionType, useWithSubtypes = true, cacheByRef = true)

        and typePickler = memberInfoPickler.Cast<Type> ()
        and methodInfoPickler = memberInfoPickler.Cast<MethodInfo> ()
        and typeArrayPickler = arrayPickler.Create typePickler

        [|
            assemblyPickler :> Pickler
            assemblyInfoPickler :> _
            assemblyNamePickler :> _
            methodInfoPickler :> _
            memberInfoPickler :> _
            typePickler :> _
        |]