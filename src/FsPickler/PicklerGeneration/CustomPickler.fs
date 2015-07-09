namespace Nessos.FsPickler

open System
open System.Reflection

open Nessos.FsPickler.Reflection

//  check if type implements a static factory method : IPicklerResolver -> Pickler<DeclaringType>

type internal CustomPickler =
    static member Create<'T>(resolver : IPicklerResolver) =
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
    static member Create<'T>() =
        let writer (w : WriteState) (_ : string) (t : 'T) =
            if not w.IsHashComputation then
                raise <| NonSerializableTypeException(typeof<'T>)

        let reader (_ : ReadState) (_ : string) = raise <| new NonSerializableTypeException(typeof<'T>)
        let cloner (c : CloneState) (t : 'T) = t
        let visitor (_ : VisitState) (_ : 'T) = ()

        CompositePickler.Create(reader, writer, cloner, visitor, PicklerInfo.UserDefined, cacheByRef = true, isCloneableOnly = true)