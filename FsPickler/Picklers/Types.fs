namespace Nessos.FsPickler

    open System
    open System.Reflection
//    open System.Globalization
    open System.IO
    open System.Runtime.Serialization

    open Nessos.FsPickler.Utils

    /// Marks a type that uses a pickler generated from a static factory method.
    type CustomPicklerAttribute () = inherit System.Attribute()


    /// raised by pickler generator whenever an unexpected error is encountered.
    type PicklerGenerationException =
        inherit SerializationException

        val private ty : Type
        
        new (t : Type, ?message : string, ?inner : exn) =
            let message =
                match message with
                | None -> sprintf "Error generating pickler for type '%O'." t
                | Some msg -> sprintf "Error generating pickler for type '%O': %s." t msg

            let inner = defaultArg inner null

            { inherit SerializationException(message, inner) ; ty = t }

        new (sI : SerializationInfo, sc : StreamingContext) =
            {
                inherit SerializationException(sI, sc)
                ty = sI.Read<Type> "picklerType"
            }

        member __.GeneratedType = __.ty

        interface ISerializable with
            member __.GetObjectData(sI : SerializationInfo, sc : StreamingContext) =
                base.GetObjectData(sI, sc)
                sI.Write<Type> ("picklerType", __.ty)


    /// raised by pickler generator whenever an unsupported type is encountered in the type graph.
    type NonSerializableTypeException =
        inherit SerializationException

        val private ty : Type

        new (t : Type, ?message : string, ?inner : exn) =
            let message =
                match message with
                | None -> sprintf "Serialization of type '%O' is not supported." t
                | Some msg -> sprintf "Serialization of type '%O' is not supported: %s" t msg

            let inner = defaultArg inner null

            { inherit SerializationException(message, inner) ; ty = t }

        new (sI : SerializationInfo, sc : StreamingContext) =
            {
                inherit SerializationException(sI, sc)
                ty = sI.Read<Type> "picklerType"
            }

        member __.UnsupportedType = __.ty

        interface ISerializable with
            member __.GetObjectData(sI : SerializationInfo, sc : StreamingContext) =
                base.GetObjectData(sI, sc)
                sI.Write<Type> ("picklerType", __.ty)



    // reflection - related types

    /// <summary>Provides facility for implementing a custom type serialization scheme.
    /// This is particularly useful in cases where bridging mono/.NET runtimes or
    /// dynamic/static assemblies is required.</summary>
    type ITypeNameConverter =
        abstract member OfSerializedType : TypeInfo -> TypeInfo
        abstract member ToDeserializedType : TypeInfo -> TypeInfo

    and TypeInfo =
        {
            Name : string
            AssemblyInfo : AssemblyInfo
        }

    // Need an immutable, structurally equatable version of AssemblyName

    and AssemblyInfo =
        {
            Name : string
            Version : string
            Culture : string
            PublicKeyToken : byte []
        }
    with
        static member OfAssemblyName(an : AssemblyName) =
            {
                Name = an.Name
                Version = an.Version.ToString()
                Culture = an.CultureInfo.Name
                PublicKeyToken = an.GetPublicKeyToken()
            }

        static member OfAssembly(a : Assembly) =
            a.GetName() |> AssemblyInfo.OfAssemblyName

//    type IAssemblyLoader =
//        abstract TryLoad : AssemblyName -> Assembly option
//    with
//        member t.AssemblyQualifiedName = 
//            let sb = new System.Text.StringBuilder()
//            let inline add (x:string) = sb.Append x |> ignore
//            add t.AssemblyName
//            add ", Version="
//            add (match t.Version with null | "" -> "0.0.0.0" | c -> c)
//            add ", Culture="
//            add (match t.Culture with null | "" -> "neutral" | c -> c)
//            add ", PublicKeyToken="
//            if t.PublicKeyToken.Length = 0 then add "null"
//            else
//                for b in t.PublicKeyToken do
//                    add <| sprintf "%02x" b
//
//            sb.ToString()
//
//        member t.FullName = sprintf "%s, %s" t.Name t.AssemblyQualifiedName
//
//
//        member internal tI.Assembly : AssemblyName =
//            {
//                Name = tI.AssemblyName
//                Version = tI.Version
//                Culture = tI.Culture
//                PublicKeyToken = tI.PublicKeyToken
//            }
//
//    and internal AssemblyName =
//        {
//            Name : string
//            Version : string
//            Culture : string
//            PublicKeyToken : byte []
//        }
//    with
//        member aI.GetType(typeName : string) : TypeInfo =
//            {
//                Name = typeName
//                AssemblyName = aI.Name
//                Version = aI.Version
//                Culture = aI.Culture
//                PublicKeyToken = aI.PublicKeyToken
//            }


//    [<AbstractClass>]
//    type Existential internal (t : Type) =
//        member __.Type = t
//
//        abstract Apply : IExistentialConsumer<'U> -> 'U
//
//        static member Create(t : Type) =
//            let et = typedefof<Existential<_>>.MakeGenericType [|t|]
//            Activator.CreateInstance et :?> Existential
//            
//    and Existential<'T> () =
//        inherit Existential(typeof<'T>)
//        override e.Apply consumer = consumer.Consume e
//
//    and IExistentialConsumer<'U> =
//        abstract Consume<'T> : Existential<'T> -> 'U