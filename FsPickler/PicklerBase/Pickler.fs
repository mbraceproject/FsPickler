namespace Nessos.FsPickler

    open System
    open System.Collections
    open System.Collections.Generic
    open System.IO
    open System.Runtime.Serialization

    [<AutoSerializable(false)>]
    [<AbstractClass>]
    type Pickler internal () =

        let mutable cacheId : string = null
        
        abstract Type : Type
        abstract ImplementationType : Type

        abstract TypeInfo : TypeInfo
        abstract PicklerInfo : PicklerInfo

        abstract IsRecursiveType : bool
        abstract IsCacheByRef : bool
        abstract IsOfFixedSize : bool
        abstract UseWithSubtypes : bool

        abstract UntypedWrite : state:WriteState -> tag:string -> value:obj -> unit
        abstract UntypedRead  : state:ReadState  -> tag:string -> obj

        abstract Unpack : IPicklerUnpacker<'U> -> 'U

        abstract Cast<'S> : unit -> Pickler<'S>
        abstract Clone : unit -> Pickler
        abstract InitializeFrom : other:Pickler -> unit

        member __.CacheId
            with get () = cacheId
            and internal set id = cacheId <- id

    and
     [<AutoSerializable(false)>]
     [<AbstractClass>]
     Pickler<'T> internal () =
        inherit Pickler ()
        
        override __.Type = typeof<'T>

        abstract Write : state:WriteState -> tag:string -> value:'T -> unit
        abstract Read  : state:ReadState  -> tag:string -> 'T

        override p.Unpack unpacker = unpacker.Apply p

    and IPicklerUnpacker<'U> =
        abstract Apply : Pickler<'T> -> 'U

    and IPicklerResolver =
        abstract Resolve : Type -> Pickler
        abstract Resolve<'T> : unit -> Pickler<'T>

    and [<AutoSerializable(false)>] WriteState internal (stream : Stream, formatP : IPickleFormatProvider, resolver : IPicklerResolver, ?streamingContext) =

        let formatter = formatP.CreateWriter stream
        let tyPickler = resolver.Resolve<Type> ()

        let sc = match streamingContext with None -> new StreamingContext() | Some sc -> sc

        let mutable idGen = new ObjectIDGenerator()
        let mutable nextWriteIsSubtype = false
        let objStack = new Stack<int64> ()
        let cyclicObjects = new HashSet<int64> ()

        

        member internal __.PicklerResolver = resolver
        member __.StreamingContext = sc
        member internal __.Formatter = formatter
        member internal __.TypePickler = tyPickler
        member internal __.ObjectIdGenerator = idGen
        member internal __.ResetCounters () = 
            idGen <- new ObjectIDGenerator ()

        member internal __.ObjectStack = objStack
        member internal __.CyclicObjectSet = cyclicObjects

        member internal __.NextWriteIsSubtype
            with get () = nextWriteIsSubtype
            and set b = nextWriteIsSubtype <- b

        interface IDisposable with
            member __.Dispose () = formatter.Dispose()

    and [<AutoSerializable(false)>] ReadState internal (stream : Stream, formatP : IPickleFormatProvider, resolver : IPicklerResolver, ?streamingContext) =
        
        let formatter = formatP.CreateReader stream
        let sc = match streamingContext with None -> new StreamingContext() | Some sc -> sc
        let mutable nextWriteIsSubtype = false
        let mutable nextObjectId = 1L
        let mutable currentArrayId = 0L
        let objCache = new Dictionary<int64, obj> ()
        let fixupIndex = new Dictionary<int64, Type * obj> ()
        let tyPickler = resolver.Resolve<Type> ()

        member internal __.PicklerResolver = resolver
        member internal __.Formatter = formatter
        member internal __.TypePickler = tyPickler
        member __.StreamingContext = sc
        member internal __.ObjectCache = objCache
        member internal __.FixupIndex = fixupIndex
        member internal __.GetObjectId (isArray : bool) =
            let id = nextObjectId
            nextObjectId <- id + 1L
            if isArray then currentArrayId <- id
            id

        member internal __.ResetCounters() = 
            nextObjectId <- 1L
            objCache.Clear()

        member internal __.RegisterUninitializedArray(array : Array) =
            objCache.Add(currentArrayId, array)

        member internal __.NextWriteIsSubtype
            with get () = nextWriteIsSubtype
            and set b = nextWriteIsSubtype <- b

        interface IDisposable with
            member __.Dispose () = formatter.Dispose()