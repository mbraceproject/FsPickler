namespace FsCoreSerializer

    open System
    open System.IO

    // holds type name conversion rules
    type ITypeFormatter =
        abstract Write : Type -> string
        abstract Read : string -> Type

    and TypeFormatter private () =
        static let mutable serializer = 
            new DefaultTypeFormatter () :> ITypeFormatter

        static member Default
            with get () = serializer
            and set conv = serializer <- conv

    and DefaultTypeFormatter () =
        interface ITypeFormatter with
            member __.Write (t : Type) = t.AssemblyQualifiedName
            member __.Read (aqn : string) = Type.GetType(aqn, true)