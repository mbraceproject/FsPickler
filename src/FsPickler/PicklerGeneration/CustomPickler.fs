namespace Nessos.FsPickler

    open System
    open System.Reflection

    open Nessos.FsPickler.Reflection

    //  check if type implements a static factory method : IPicklerResolver -> Pickler<DeclaringType>

    type internal CustomPickler =
        static member Create(t : Type, resolver : IPicklerResolver) =
            let factoryMethod =
                match t.GetMethod("CreatePickler", allStatic) with
                | null -> None
                | m when    not m.IsGenericMethod &&
                            m.GetParameterTypes() = [| typeof<IPicklerResolver> |] && 
                            m.ReturnType = typedefof<Pickler<_>>.MakeGenericType [| t |] -> 
                    Some m

                | _ -> None

            match factoryMethod with
            | Some m -> m.GuardedInvoke(null, [| resolver :> obj |]) :?> Pickler
            | None ->
                let msg = sprintf "marked [<CustomPickler>] but missing static method 'CreatePickler : IPicklerResolver -> Pickler<%s>'." t.Name
                raise <| new NonSerializableTypeException(t, msg)