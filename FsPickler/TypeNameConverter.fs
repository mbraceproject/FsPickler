namespace FsPickler

    open System
    open System.IO
    open System.Collections.Generic
    open System.Runtime.Serialization

    // pluggable type serialization

    /// <summary>Provides facility for implementing a custom type serialization scheme.
    /// This is particularly useful in cases where bridging mono/.NET runtimes or
    /// dynamic/static assemblies is required.</summary>
    type ITypeNameConverter =
        abstract WriteQualifiedName : BinaryWriter -> Type -> unit
        abstract ReadQualifiedName : BinaryReader -> Type

    /// provides standard type serialization
    and DefaultTypeNameConverter () =
        interface ITypeNameConverter with
            member __.WriteQualifiedName bw t = bw.Write t.AssemblyQualifiedName
            member __.ReadQualifiedName br =
                let aqn = br.ReadString () in Type.GetType(aqn, true)

    /// quick'n'dirty fix to the mono issue filed in https://bugzilla.xamarin.com/show_bug.cgi?id=15124
    /// warning! no interop with default typenameconverter
    and MonoTypeNameConverter () =
        let rec write (bw : BinaryWriter) (t : Type) =
            let aqn = t.AssemblyQualifiedName
            if aqn.Contains("@") && t.IsGenericType && not t.IsGenericTypeDefinition then
                bw.Write true
                t.GetGenericTypeDefinition() |> write bw
                let gas = t.GetGenericArguments() 
                bw.Write gas.Length
                for ga in gas do write bw ga
            else
                bw.Write false
                bw.Write aqn

        let rec read (br : BinaryReader) =
            if br.ReadBoolean() then
                let gt = read br
                let n = br.ReadInt32()
                let gas = Array.zeroCreate<Type> n
                for i = 0 to n - 1 do
                    gas.[i] <- read br
                gt.MakeGenericType gas
            else
                Type.GetType (br.ReadString())

        interface ITypeNameConverter with
            member tc.WriteQualifiedName bw t = write bw t
            member tc.ReadQualifiedName br = read br