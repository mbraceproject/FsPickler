namespace Nessos.FsPickler

    open System
    open System.Collections.Generic
    open System.Collections.Concurrent

    open Nessos.FsPickler.Utils
    open Nessos.FsPickler.PrimitivePicklers
    open Nessos.FsPickler.ReflectionPicklers
    open Nessos.FsPickler.PicklerResolution

    type internal PicklerCache private () =

        static let instance = lazy(new PicklerCache())

        let basePicklers =
            seq {  
                yield CompositePickler.ObjectPickler :> Pickler
                yield! PrimitivePicklers.mkAll ()
                yield! mkReflectionPicklers ()
            } |> Seq.map (fun p -> KeyValuePair(p.Type, Success p))

        let dict = new ConcurrentDictionary<Type, Exn<Pickler>>(basePicklers)

        let cache =
            {
                new ICache<Type, Exn<Pickler>> with
                    member __.Lookup(t : Type) =
                        let found, p = dict.TryGetValue t
                        if found then Some p
                        else None

                    member __.Commit (t : Type) (p : Exn<Pickler>) = dict.GetOrAdd(t, p)
            }

        let resolve (t : Type) = YParametric cache resolvePickler t

        interface IPicklerResolver with
            member r.Resolve<'T> () = resolve typeof<'T> :?> Pickler<'T>
            member r.Resolve (t : Type) = resolve t

        static member Instance = instance.Value