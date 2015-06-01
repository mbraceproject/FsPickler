namespace Nessos.FsPickler

// The Pickler cache stores all generated picklers.
// It is essentially a ConcurrentDictionary<Type, Pickler>
// It is a singleton that is used by every pickler implementation in the AppDomain.

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading

open Nessos.FsPickler.Utils
open Nessos.FsPickler.PrimitivePicklers
open Nessos.FsPickler.ReflectionPicklers
open Nessos.FsPickler.PicklerResolution

[<AutoSerializable(false)>]
type internal PicklerCache private () =

    static let instance = lazy(new PicklerCache())

    let basePicklers =
        seq {  
            yield CompositePickler.ObjectPickler :> Pickler
            yield! PrimitivePicklers.mkAll ()
            yield ArrayPickler.CreateByteArrayPickler() :> Pickler
            yield! mkReflectionPicklers <| ArrayPickler.GetInterface()
        } |> Seq.map (fun p -> KeyValuePair(p.Type, Success p))

    /// declares pickler generation locked for cache
    [<VolatileField>]
    let mutable isLocked = false
    /// number of picklers that are currently being generated
    [<VolatileField>]
    let mutable resolutionCount = 0

    let dict = new ConcurrentDictionary<Type, Exn<Pickler>>(basePicklers)

    let cache =
        {
            new ICache<Type, Exn<Pickler>> with
                member __.Lookup(t : Type) =
                    let mutable p = Unchecked.defaultof<Exn<Pickler>>
                    let found = dict.TryGetValue(t, &p)
                    if found then Some p
                    else None

                member __.Commit (t : Type) (p : Exn<Pickler>) = dict.GetOrAdd(t, p)
        }

    let resolve (t : Type) = 
        let mutable p = Unchecked.defaultof<Exn<Pickler>>
        let found = dict.TryGetValue(t, &p)
        if found then p
        else
            // spinwait while cache is in locked state.
            while isLocked && resolutionCount = 0 do Thread.SpinWait(20)

            // keep track of number of current pickler generation operations
            Interlocked.Increment &resolutionCount |> ignore
            try generatePickler cache t
            finally Interlocked.Decrement &resolutionCount |> ignore

    /// Performs an operation while no picklers are being appended to the cache.
    member c.WithLockedCache (f : unit -> unit) =
        // synchronize all calls to this method
        lock c (fun () -> 
            // declare lock intention to pickler generation
            isLocked <- true ; Thread.Sleep 10
            // spinwait until all pending pickler resolutions are completed
            while resolutionCount > 0 do Thread.SpinWait(20)
            // perform operation, finally resetting lock switch
            try f () finally isLocked <- false)
        
    member __.IsPicklerGenerated t = dict.ContainsKey t

    interface IPicklerResolver with
        member r.IsSerializable (t : Type) = (resolve t).IsValue
        member r.IsSerializable<'T> () = (resolve typeof<'T>).IsValue

        member r.Resolve (t : Type) = (resolve t).Value
        member r.Resolve<'T> () = (resolve typeof<'T>).Value :?> Pickler<'T>

    static member Instance = instance.Value