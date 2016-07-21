namespace MBrace.FsPickler

open System
open System.Reflection

open MBrace.FsPickler.Reflection

type internal CustomPickler =
    static member Create<'T>(resolver : IPicklerResolver) =
        //  check if type implements a static factory method : IPicklerResolver -> Pickler<DeclaringType>
        let factoryMethod =
            match typeof<'T>.GetMethod("CreatePickler", allStatic) with
            | null -> None
            | m when    not m.IsGenericMethod &&
                        m.GetParameterTypes() = [| typeof<IPicklerResolver> |] && 
                        m.ReturnType = typedefof<Pickler<_>>.MakeGenericType [| typeof<'T> |] -> 
                Some m

            | _ -> None

        match factoryMethod with
        | Some m -> m.GuardedInvoke(null, [| resolver :> obj |]) :?> Pickler<'T>
        | None ->
            let msg = sprintf "marked [<CustomPickler>] but missing static method 'CreatePickler : IPicklerResolver -> Pickler<%s>'." typeof<'T>.Name
            raise <| new NonSerializableTypeException(typeof<'T>, msg)


type internal CloneableOnlyPickler =
    /// Defines a pickler that forces cloneability/hashability on nonserializable types.
    static member Create<'T>() =
        let writer (w : WriteState) (_ : string) (t : 'T) =
            if w.IsHashComputation then w.Formatter.WriteInt32 "hash" <| (box t).GetHashCode()
            else
                raise <| NonSerializableTypeException(typeof<'T>)

        let reader (_ : ReadState) (_ : string) = raise <| new NonSerializableTypeException(typeof<'T>)
        let cloner (_ : CloneState) (t : 'T) = t
        let visitor (_ : VisitState) (_ : 'T) = ()

        CompositePickler.Create(reader, writer, cloner, visitor, PicklerInfo.UserDefined, cacheByRef = true, isCloneableOnly = true)