module internal Nessos.FsPickler.ReflectionPicklers

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

        let rec memberInfoWriter (w : WriteState) (tag : string) (m : MemberInfo) =
            let formatter = w.Formatter

            // not used anywhere ; placed here just to assist type inference
            let inline tp () : Pickler<Type> = typePickler
            let inline tp () : Pickler<Type []> = typeArrayPickler
            let inline mp () : Pickler<MemberInfo> = memberInfoPickler
            let inline mp () : Pickler<MethodInfo> = methodInfoPickler

            match w.ReflectionCache.GetCompositeMemberInfo m with
            | NamedType (name, aI) ->
                formatter.WriteByte "memberType" 0uy

                formatter.WriteString "name" name
                assemblyInfoPickler.Write w "assembly" aI

            | ArrayType (et, rk) ->
                formatter.WriteByte "memberType" 1uy
                typePickler.Write w "elementType" et

                match rk with
                | None -> formatter.WriteInt32 "rank" 0
                | Some r -> formatter.WriteInt32 "rank" r

            | GenericType(dt, tyArgs) ->
                formatter.WriteByte "memberType" 2uy
                typePickler.Write w "definition" dt
                typeArrayPickler.Write w "typeArgs" tyArgs

            | GenericTypeParam(dt, idx) ->
                formatter.WriteByte "memberType" 3uy
                typePickler.Write w "declaringType" dt
                formatter.WriteInt32 "index" idx

            | GenericMethodParam(dm, idx) ->
                formatter.WriteByte "memberType" 4uy
                methodInfoPickler.Write w "declaringMethod" dm
                formatter.WriteInt32 "index" idx

            | Method(dt, rt, name, isStatic, mParams) ->
                formatter.WriteByte "memberType" 5uy

                formatter.WriteString "name" name
                formatter.WriteBoolean "isStatic" isStatic
                typeArrayPickler.Write w "params" mParams

                typePickler.Write w "declaringType" dt
                typePickler.Write w "reflectedType" (defaultArg rt null)

            | GenericMethod(gm, tyArgs) ->
                formatter.WriteByte "memberType" 6uy

                methodInfoPickler.Write w "definition" gm
                typeArrayPickler.Write w "typeArgs" tyArgs

            | GenericMethodDefinition(dt, rt, name, isStatic, signature) ->
                formatter.WriteByte "memberType" 7uy

                formatter.WriteString "name" name
                formatter.WriteBoolean "isStatic" isStatic
                stringArrayPickler.Write w "params" signature

                typePickler.Write w "declaringType" dt
                typePickler.Write w "reflectedType" (defaultArg rt null)

            | Constructor(dt, isStatic, cParams) ->
                formatter.WriteByte "memberType" 8uy

                typePickler.Write w "declaringType" dt
                formatter.WriteBoolean "isStatic" isStatic
                typeArrayPickler.Write w "params" cParams

            | Property(dt, rt, name, isStatic) ->
                formatter.WriteByte "memberType" 9uy

                formatter.WriteString "name" name
                formatter.WriteBoolean "isStatic" isStatic

                typePickler.Write w "declaringType" dt
                typePickler.Write w "reflectedType" (defaultArg rt null)

            | Field(dt, rt, name, isStatic) ->
                formatter.WriteByte "memberType" 10uy

                formatter.WriteString "name" name
                formatter.WriteBoolean "isStatic" isStatic

                typePickler.Write w "declaringType" dt
                typePickler.Write w "reflectedType" (defaultArg rt null)

            | Event(dt, rt, name, isStatic) ->
                formatter.WriteByte "memberType" 11uy

                formatter.WriteString "name" name
                formatter.WriteBoolean "isStatic" isStatic

                typePickler.Write w "declaringType" dt
                typePickler.Write w "reflectedType" (defaultArg rt null)

            | Unknown(t, name) ->
                raise <| new NonSerializableTypeException(t, sprintf "could not serialize '%s'." name)

        and memberInfoReader (r : ReadState) (tag : string) =
            let formatter = r.Formatter

            let cMemberInfo =
                match formatter.ReadByte "memberType" with
                | 0uy ->
                    let name = formatter.ReadString "name"
                    let assembly = assemblyInfoPickler.Read r "assembly"
                    NamedType(name, assembly)

                | 1uy ->
                    let et = typePickler.Read r "elementType"
                    match formatter.ReadInt32 "rank" with
                    | 0 -> ArrayType(et, None)
                    | rk -> ArrayType(et, Some rk)

                | 2uy ->
                    let gt = typePickler.Read r "definition"
                    let tyArgs = typeArrayPickler.Read r "typeArgs"
                    GenericType(gt, tyArgs)

                | 3uy ->
                    let dt = typePickler.Read r "declaringType"
                    let idx = formatter.ReadInt32 "index"
                    GenericTypeParam(dt, idx)

                | 4uy ->
                    let dm = methodInfoPickler.Read r "declaringMethod"
                    let idx = formatter.ReadInt32 "index"
                    GenericMethodParam(dm, idx)

                | 5uy ->
                    let name = formatter.ReadString "name"
                    let isStatic = formatter.ReadBoolean "isStatic"
                    let mParams = typeArrayPickler.Read r "params"

                    let dt = typePickler.Read r "declaringType"
                    let rt = denull <| typePickler.Read r "reflectedType"

                    Method(dt, rt, name, isStatic, mParams)

                | 6uy ->
                    let gm = methodInfoPickler.Read r "definition"
                    let tyArgs = typeArrayPickler.Read r "typeArgs"
                    GenericMethod(gm, tyArgs)

                | 7uy ->
                    let name = formatter.ReadString "name"
                    let isStatic = formatter.ReadBoolean "isStatic"
                    let signature = stringArrayPickler.Read r "params"

                    let dt = typePickler.Read r "declaringType"
                    let rt = denull <| typePickler.Read r "reflectedType"

                    GenericMethodDefinition(dt, rt, name, isStatic, signature)

                | 8uy ->
                    let dt = typePickler.Read r "declaringType"
                    let isStatic = formatter.ReadBoolean "isStatic"
                    let cParams = typeArrayPickler.Read r "params"
                    Constructor(dt, isStatic, cParams)

                | 9uy ->
                    let name = formatter.ReadString "name"
                    let isStatic = formatter.ReadBoolean "isStatic"

                    let dt = typePickler.Read r "declaringType"
                    let rt = denull <| typePickler.Read r "reflectedType"

                    Property(dt, rt, name, isStatic)

                | 10uy ->
                    let name = formatter.ReadString "name"
                    let isStatic = formatter.ReadBoolean "isStatic"

                    let dt = typePickler.Read r "declaringType"
                    let rt = denull <| typePickler.Read r "reflectedType"

                    Field(dt, rt, name, isStatic)

                | 11uy ->
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