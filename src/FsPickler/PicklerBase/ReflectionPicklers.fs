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
    open Nessos.FsPickler.TypeCache
    open Nessos.FsPickler.PicklerUtils

    let mkReflectionPicklers () =
        let assemblyInfoPickler =
            let writer (w : WriteState) _ (aI : AssemblyInfo) =
                let formatter = w.Formatter
                formatter.WriteString "name" aI.Name
                formatter.WriteString "version" aI.Version
                formatter.WriteString "culture" aI.Culture
                formatter.WriteBytes "pkt" aI.PublicKeyToken

            let reader (r : ReadState) _ =
                let formatter = r.Formatter
                let name = formatter.ReadString "name"
                let version = formatter.ReadString "version"
                let culture = formatter.ReadString "culture" 
                let pkt = formatter.ReadBytes "pkt"

                {
                    Name = name
                    Version = version
                    Culture = culture
                    PublicKeyToken = pkt
                }

            mkPickler PicklerInfo.ReflectionType false true reader writer

        let assemblyPickler =
            mkPickler PicklerInfo.ReflectionType true true
                (fun r _ -> let aI = assemblyInfoPickler.Read r "info" in r.ReflectionCache.LoadAssembly aI)
                (fun w _ a -> let aI = w.ReflectionCache.GetAssemblyInfo a in assemblyInfoPickler.Write w "info" aI)

        let rec memberInfoWriter (w : WriteState) (tag : string) (m : MemberInfo) =
            let formatter = w.Formatter

            // not used anywhere ; there just to aid type inference
            let inline tp () : Pickler<Type> = typePickler
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
                | None ->
                    formatter.WriteInt32 "rank" 0
                | Some r ->
                    formatter.WriteInt32 "rank" r

            | GenericType(dt, tyArgs) ->
                formatter.WriteByte "memberType" 2uy
                typePickler.Write w "genericDefinition" dt
                writeArray w typePickler "tyArgs" tyArgs

            | GenericTypeParam(dt, idx) ->
                formatter.WriteByte "memberType" 3uy
                typePickler.Write w "declaringType" dt
                formatter.WriteInt32 "idx" idx

            | GenericMethodParam(dm, idx) ->
                formatter.WriteByte "memberType" 4uy
                methodInfoPickler.Write w "declaringMethod" dm
                formatter.WriteInt32 "idx" idx

            | Method(dt, rt, name, isStatic, mParams) ->
                formatter.WriteByte "memberType" 5uy
                typePickler.Write w "declaringType" dt
                match rt with
                | None -> formatter.WriteBoolean "isReflected" false
                | Some rt ->
                    formatter.WriteBoolean "isReflected" true
                    typePickler.Write w "reflectedType" rt

                formatter.WriteString "name" name
                formatter.WriteBoolean "isStatic" isStatic
                writeArray w typePickler "params" mParams

            | GenericMethod(gm, tyArgs) ->
                formatter.WriteByte "memberType" 6uy
                methodInfoPickler.Write w "genericMethod" gm
                writeArray w typePickler "tyArgs" tyArgs

            | GenericMethodDefinition(dt, rt, name, isStatic, signature) ->
                formatter.WriteByte "memberType" 7uy
                typePickler.Write w "declaringType" dt
                match rt with
                | None -> formatter.WriteBoolean "isReflected" false
                | Some rt ->
                    formatter.WriteBoolean "isReflected" true
                    typePickler.Write w "reflectedType" rt

                formatter.WriteString "name" name
                formatter.WriteBoolean "isStatic" isStatic
                formatter.BeginWriteBoundedSequence "params" signature.Length
                for s in signature do formatter.WriteString "param" s
                formatter.EndWriteBoundedSequence ()

            | Constructor(dt, isStatic, cParams) ->
                formatter.WriteByte "memberType" 8uy
                typePickler.Write w "declaringType" dt
                formatter.WriteBoolean "isStatic" isStatic
                writeArray w typePickler "params" cParams

            | Property(dt, rt, name, isStatic) ->
                formatter.WriteByte "memberType" 9uy
                typePickler.Write w "declaringType" dt
                match rt with
                | None -> formatter.WriteBoolean "isReflected" false
                | Some rt ->
                    formatter.WriteBoolean "isReflected" true
                    typePickler.Write w "reflectedType" rt

                formatter.WriteString "name" name
                formatter.WriteBoolean "isStatic" isStatic

            | Field(dt, rt, name, isStatic) ->
                formatter.WriteByte "memberType" 10uy
                typePickler.Write w "declaringType" dt
                match rt with
                | None -> formatter.WriteBoolean "isReflected" false
                | Some rt ->
                    formatter.WriteBoolean "isReflected" true
                    typePickler.Write w "reflectedType" rt

                formatter.WriteString "name" name
                formatter.WriteBoolean "isStatic" isStatic

            | Event(dt, rt, name, isStatic) ->
                formatter.WriteByte "memberType" 11uy
                typePickler.Write w "declaringType" dt
                match rt with
                | None -> formatter.WriteBoolean "isReflected" false
                | Some rt ->
                    formatter.WriteBoolean "isReflected" true
                    typePickler.Write w "reflectedType" rt

                formatter.WriteString "name" name
                formatter.WriteBoolean "isStatic" isStatic

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
                    let gt = typePickler.Read r "genericDefinition"
                    let tyArgs = readArray r typePickler "tyArgs"
                    GenericType(gt, tyArgs)

                | 3uy ->
                    let dt = typePickler.Read r "declaringType"
                    let idx = formatter.ReadInt32 "idx"
                    GenericTypeParam(dt, idx)

                | 4uy ->
                    let dm = methodInfoPickler.Read r "declaringMethod"
                    let idx = formatter.ReadInt32 "idx"
                    GenericMethodParam(dm, idx)

                | 5uy ->
                    let dt = typePickler.Read r "declaringType"
                    let rt =
                        if formatter.ReadBoolean "isReflected" then
                            Some <| typePickler.Read r "reflectedType"
                        else
                            None

                    let name = formatter.ReadString "name"
                    let isStatic = formatter.ReadBoolean "isStatic"
                    let mParams = readArray r typePickler "params"
                    Method(dt, rt, name, isStatic, mParams)

                | 6uy ->
                    let gm = methodInfoPickler.Read r "genericMethod"
                    let tyArgs = readArray r typePickler "tyArgs"
                    GenericMethod(gm, tyArgs)

                | 7uy ->
                    let dt = typePickler.Read r "declaringType"
                    let rt =
                        if formatter.ReadBoolean "isReflected" then
                            Some <| typePickler.Read r "reflectedType"
                        else
                            None

                    let name = formatter.ReadString "name"
                    let isStatic = formatter.ReadBoolean "isStatic"
                    let plength = formatter.BeginReadBoundedSequence "params"
                    let signature = Array.zeroCreate plength
                    for i = 0 to plength - 1 do
                        signature.[i] <- formatter.ReadString "param"
                    formatter.EndReadBoundedSequence ()
                    GenericMethodDefinition(dt, rt, name, isStatic, signature)

                | 8uy ->
                    let dt = typePickler.Read r "declaringType"
                    let isStatic = formatter.ReadBoolean "isStatic"
                    let cParams = readArray r typePickler "params"
                    Constructor(dt, isStatic, cParams)

                | 9uy ->
                    let dt = typePickler.Read r "declaringType"
                    let rt =
                        if formatter.ReadBoolean "isReflected" then
                            Some <| typePickler.Read r "reflectedType"
                        else
                            None

                    let name = formatter.ReadString "name"
                    let isStatic = formatter.ReadBoolean "isStatic"
                    Property(dt, rt, name, isStatic)

                | 10uy ->
                    let dt = typePickler.Read r "declaringType"
                    let rt =
                        if formatter.ReadBoolean "isReflected" then
                            Some <| typePickler.Read r "reflectedType"
                        else
                            None

                    let name = formatter.ReadString "name"
                    let isStatic = formatter.ReadBoolean "isStatic"
                    Field(dt, rt, name, isStatic)

                | 11uy ->
                    let dt = typePickler.Read r "declaringType"
                    let rt =
                        if formatter.ReadBoolean "isReflected" then
                            Some <| typePickler.Read r "reflectedType"
                        else
                            None

                    let name = formatter.ReadString "name"
                    let isStatic = formatter.ReadBoolean "isStatic"
                    Event(dt, rt, name, isStatic)

                // 'Unknown' cases never get serialized; this is a case of invalid data
                | _ ->
                    raise <| new InvalidDataException("invalid member type.")


            r.ReflectionCache.LoadMemberInfo cMemberInfo

        and memberInfoPickler = mkPickler PicklerInfo.ReflectionType true true memberInfoReader memberInfoWriter
        and typePickler = mkPickler PicklerInfo.ReflectionType true true (fun r t -> memberInfoReader r t |> fastUnbox<Type>) memberInfoWriter
        and methodInfoPickler = mkPickler PicklerInfo.ReflectionType true true (fun r t -> memberInfoReader r t |> fastUnbox<MethodInfo>) memberInfoWriter

        [|
            assemblyPickler :> Pickler
            assemblyInfoPickler :> _
            methodInfoPickler :> _
            memberInfoPickler :> _
            typePickler :> _
        |]