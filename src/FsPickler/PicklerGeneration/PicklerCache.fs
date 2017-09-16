namespace MBrace.FsPickler

// The Pickler cache stores all generated picklers.
// It is essentially a ConcurrentDictionary<Type, Pickler>
// It is a singleton that is used by every pickler implementation in the AppDomain.

open System
open System.Threading
open System.Collections.Concurrent

open MBrace.FsPickler.Utils
open MBrace.FsPickler.PicklerResolution

/// Defines a cache of generated picklers for every type being used by a serializer.
/// Picklers are being generated recursively and on-demand. Note that this is an extremely
/// heavyweight object both in terms of size and cost of pickler generation. Most applications
/// should just make use of the `PicklerCache.Instance` singleton. Otherwise extreme care must
/// be exercised so that multiple instances of this cache are not created.
[<Sealed; AutoSerializable(false)>]
type PicklerCache private (registry : ICustomPicklerRegistry) =
#if !DISABLE_PICKLERCACHE_INSTANCE_COUNT_CHECKS
    static let mutable cacheInstaceCounter = 0
    do if Interlocked.Increment &cacheInstaceCounter > 50 then
        "Too many PicklerCache instances have been created for this process." +
        "Chances are you are mismanaging PicklerCache instances, which should be treated as singletons."
        |> invalidOp
#endif

    static let instance = lazy(new PicklerCache(EmptyPicklerRegistry()))

    let dict = new ConcurrentDictionary<Type, Exn<Pickler>>()

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
        else generatePickler registry cache t

    member __.Registry = registry
    member __.IsPicklerGenerated t = dict.ContainsKey t

    /// Decides if given type is serializable by the pickler cache
    member __.IsSerializableType<'T> () : bool = 
        isSerializable(resolve typeof<'T>)

    /// Decides if given type is serializable by the pickler cache
    member __.IsSerializableType (t : Type) : bool = 
        isSerializable(resolve t)

    /// Auto generates a pickler for given type variable
    member __.GeneratePickler<'T> () : Pickler<'T> = 
        (resolve typeof<'T>).Value :?> Pickler<'T>
        
    /// Auto generates a pickler for given type
    member __.GeneratePickler (t : Type) : Pickler = 
        (resolve t).Value

    interface IPicklerResolver with
        member __.IsSerializable (t : Type) = __.IsSerializableType t
        member __.IsSerializable<'T> () = __.IsSerializableType<'T>()

        member __.Resolve (t : Type) = __.GeneratePickler t
        member __.Resolve<'T> () = __.GeneratePickler<'T>()

    /// Gets the singleton PicklerCache instance with the default pickler generation semantics 
    static member Instance = instance.Value

    /// <summary>
    ///     Creates a custom pickler cache based off a supplied pickler registry.
    /// </summary>
    /// <param name="registry"></param>
    [<CompilerMessage("PicklerCache instances are extremely heavyweight. Should only be created as singletons.", 8989)>]
    static member FromCustomPicklerRegistry(registry : ICustomPicklerRegistry) : PicklerCache =
        new PicklerCache(registry)