namespace FsCoreSerializer

    open System
    open System.IO
    open System.Collections.Generic
    open System.Runtime.Serialization

    // pluggable type serialization

    type ITypeFormatter =
        abstract Write : BinaryWriter -> Type -> unit
        abstract Read : BinaryReader -> Type

    and TypeFormatter private () =
        static let mutable serializer = 
            new DefaultTypeFormatter () :> ITypeFormatter

        static member Default
            with get () = serializer
            and set conv = serializer <- conv

    and DefaultTypeFormatter () =
        interface ITypeFormatter with
            member __.Write (bw : BinaryWriter) (t : Type) =
                if t.IsGenericParameter then
                    bw.Write t.ReflectedType.AssemblyQualifiedName
                    bw.Write true
                    bw.Write t.Name
                else
                    bw.Write t.AssemblyQualifiedName
                    bw.Write false

            member __.Read (br : BinaryReader) =
                let aqn = br.ReadString()
                let t = Type.GetType(aqn, true)
                if br.ReadBoolean() then
                    // is generic parameter
                    let pname = br.ReadString()
                    try t.GetGenericArguments() |> Array.find(fun a -> a.Name = pname)
                    with :? KeyNotFoundException -> raise <| new SerializationException(sprintf "cannot deserialize type '%s'" pname)
                else
                    t