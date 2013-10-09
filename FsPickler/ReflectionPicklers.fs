namespace FsPickler

    open System
    open System.Globalization
    open System.IO
    open System.Reflection
    open System.Runtime.Serialization
    open System.Collections.Generic

    open FsPickler
    open FsPickler.Utils
    open FsPickler.PicklerUtils

    module internal ReflectionPicklers =

        let getAssemblyInfo (a : Assembly) =
            let an = a.GetName()
            {
                Name = an.Name
                Version = an.Version.ToString()
                CultureInfo = an.CultureInfo
                PublicKeyToken = an.GetPublicKeyToken()
            }

        let loadAssembly (aI : AssemblyInfo) =
            let an = new AssemblyName()
            an.Name <- aI.Name
            an.Version <- new Version(aI.Version)
            an.CultureInfo <- aI.CultureInfo
            an.SetPublicKeyToken(aI.PublicKeyToken)
            Assembly.Load(an)


        // type pickler definition

        let mkTypePickler (tyConv : ITypeNameConverter) =
            let getAssemblyInfo = memoize getAssemblyInfo
            let loadAssembly = memoize loadAssembly
            let loadType = memoize (fun (a,n) -> FormatterServices.GetTypeFromAssembly(a,n))

            let rec writer (w : Writer) (t : Type) =
                if t.IsGenericType && not t.IsGenericTypeDefinition then
                    w.BinaryWriter.Write 0uy
                    w.Write(tyPickler, t.GetGenericTypeDefinition())
                    let gas = t.GetGenericArguments()
                    w.BinaryWriter.Write gas.Length
                    for ga in gas do
                        w.Write(tyPickler, ga)

                elif t.IsGenericParameter then
                    w.BinaryWriter.Write 1uy
                    let dt = t.DeclaringType
                    let rank = dt.GetGenericArguments() |> Array.findIndex((=) t)
                    w.Write(tyPickler, dt)
                    w.BinaryWriter.Write rank
                else
                    w.BinaryWriter.Write 2uy
                    let aI = getAssemblyInfo t.Assembly
                    tyConv.Write w.BinaryWriter { FullName = t.FullName ; AssemblyInfo = aI }

            and reader (r : Reader) : Type =
                match r.BinaryReader.ReadByte() with
                | 0uy ->
                    let gt = r.Read tyPickler
                    let n = r.BinaryReader.ReadInt32()
                    let gas = Array.zeroCreate<Type> n
                    for i = 0 to n - 1 do
                        gas.[i] <- r.Read tyPickler

                    gt.MakeGenericType gas
                | 1uy ->
                    let dt = r.Read tyPickler
                    let idx = r.BinaryReader.ReadInt32()
                    dt.GetGenericArguments().[idx]
                | _ ->
                    let tI = tyConv.Read r.BinaryReader
                    let assembly = loadAssembly tI.AssemblyInfo
                    FormatterServices.GetTypeFromAssembly(assembly, tI.FullName)
//                    loadType(assembly, tI.FullName)

            and tyPickler = mkPickler PicklerInfo.ReflectionType true true reader writer

            tyPickler


        let mkMemberInfoPickler typePickler =
            let writer (w : Writer) (m : MemberInfo) =
                w.Write(typePickler, m.ReflectedType)
                match m with
                | :? MethodInfo as m when m.IsGenericMethod && not m.IsGenericMethodDefinition ->
                    let gm = m.GetGenericMethodDefinition()
                    let ga = m.GetGenericArguments()
                    w.BinaryWriter.Write (gm.ToString())

                    w.BinaryWriter.Write true
                    w.BinaryWriter.Write ga.Length
                    for a in ga do w.Write(typePickler, a)
                | _ ->
                    w.BinaryWriter.Write (m.ToString())
                    w.BinaryWriter.Write false

            let reader (r : Reader) =
                let t = r.Read typePickler
                let mname = r.BinaryReader.ReadString()
                let m = 
                    try t.GetMembers(allMembers) |> Array.find (fun m -> m.ToString() = mname)
                    with :? KeyNotFoundException ->
                        raise <| new SerializationException(sprintf "FsPickler: could not deserialize member '%O.%s'" t.Name mname)

                if r.BinaryReader.ReadBoolean() then
                    let n = r.BinaryReader.ReadInt32()
                    let ga = Array.zeroCreate<Type> n
                    for i = 0 to n - 1 do ga.[i] <- r.Read typePickler
                    (m :?> MethodInfo).MakeGenericMethod ga :> MemberInfo
                else
                    m

            mkPickler PicklerInfo.ReflectionType true true reader writer


        let mkReflectionPicklers tyConv =
            let typePickler = mkTypePickler tyConv
            let memberPickler = mkMemberInfoPickler typePickler
            [
                typePickler :> Pickler ; memberPickler :> _ ; 
            ]


    type DefaultTypeNameConverter(?strongNames : bool) =
        let strongNames = defaultArg strongNames true

        interface ITypeNameConverter with
            member __.Write (bw : BinaryWriter) (tI : TypeInfo) =
                bw.Write tI.FullName
                let aI = tI.AssemblyInfo
                bw.Write aI.Name
                if strongNames then
                    bw.Write(aI.CultureInfo.LCID)
                    bw.Write(aI.Version)
                    match aI.PublicKeyToken with
                    | null | [||] -> bw.Write false
                    | bytes ->
                        bw.Write true
                        bw.Write aI.PublicKeyToken

            member __.Read (br : BinaryReader) =
                let name = br.ReadString()
                let assemblyName = br.ReadString ()
                let assemblyInfo =
                    if strongNames then
                        let lcid = br.ReadInt32()
                        let version = br.ReadString()
                        let pkt = 
                            if br.ReadBoolean() then br.ReadBytes(8)
                            else null
                        {
                            Name = assemblyName
                            CultureInfo = new System.Globalization.CultureInfo(lcid)
                            Version = version
                            PublicKeyToken = pkt
                        }
                    else
                        {
                            Name = assemblyName
                            CultureInfo = null
                            Version = null
                            PublicKeyToken = null
                        }

                { FullName = name ; AssemblyInfo = assemblyInfo }
