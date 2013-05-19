namespace FsCoreSerializer

    open System

    // TODO : directly access type formatter from Reader/Writer

    // holds type name conversion rules
    type ITypeSerializer =
        abstract Write : Type -> string
        abstract Read : string -> Type

    and TypeSerializer private () =
        static let mutable serializer = 
            new DefaultTypeSerializer () :> ITypeSerializer

        static member Default
            with get () = serializer
            and set conv = serializer <- conv

    and DefaultTypeSerializer () =
        interface ITypeSerializer with
            member __.Write (t : Type) = t.AssemblyQualifiedName
            member __.Read (aqn : string) = Type.GetType(aqn, true)