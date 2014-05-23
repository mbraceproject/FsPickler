namespace Nessos.FsPickler

    open System
    open System.Collections
    open System.Collections.Generic
    open System.IO
    open System.Runtime.Serialization

    open Nessos.FsPickler.Header

    [<AutoSerializable(false)>]
    [<AbstractClass>]
    type Pickler internal () =

        let mutable cacheId : string = null
        
        abstract Type : Type
        abstract TypeKind : TypeKind
        abstract ImplementationType : Type

        abstract IsCyclicType : bool
        abstract IsPrimitive : bool
        abstract IsCacheByRef : bool
        abstract IsOfFixedSize : bool
        abstract UseWithSubtypes : bool
        abstract PicklerFlags : PicklerFlags

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
        let sc = match streamingContext with None -> new StreamingContext() | Some sc -> sc
        let mutable idGen = new ObjectIDGenerator()
        let mutable nextWriteIsSubtype = false
        let objStack = new Stack<int64> ()
        let cyclicObjects = new HashSet<int64> ()

        let tyPickler = resolver.Resolve<Type> ()

        member internal __.PicklerResolver = resolver
        member __.StreamingContext = sc
        member internal __.Formatter = formatter
        member internal __.TypePickler = tyPickler
        member internal __.ObjectIdGenerator = idGen
        member internal __.ResetCounters () = 
            idGen <- new ObjectIDGenerator ()
//        member internal __.ObjectIdGenerator 
//            with get () = idGen
//            and set gen = idGen <- gen

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



    and IPickleFormatWriter =

        inherit IDisposable

        abstract BeginWriteRoot : string -> unit
        abstract EndWriteRoot : unit -> unit

        abstract BeginWriteObject : pickler:Pickler -> tag:string -> flags:ObjectFlags -> unit
        abstract EndWriteObject : unit -> unit
        
        abstract WriteBoolean : tag:string -> value:bool -> unit
        abstract WriteByte : tag:string -> value:byte -> unit
        abstract WriteSByte : tag:string -> value:sbyte -> unit

        abstract WriteInt16 : tag:string -> value:int16 -> unit
        abstract WriteInt32 : tag:string -> value:int32 -> unit
        abstract WriteInt64 : tag:string -> value:int64 -> unit

        abstract WriteUInt16 : tag:string -> value:uint16 -> unit
        abstract WriteUInt32 : tag:string -> value:uint32 -> unit
        abstract WriteUInt64 : tag:string -> value:uint64 -> unit

        abstract WriteSingle : tag:string -> value:float32 -> unit
        abstract WriteDouble : tag:string -> value:float -> unit
        abstract WriteDecimal : tag:string -> value:decimal -> unit

        abstract WriteChar : tag:string -> value : char -> unit
        abstract WriteString : tag:string -> value:string -> unit

        abstract WriteBytes : tag:string -> value:byte [] -> unit
        abstract WriteBytesFixed : tag:string -> value:byte [] -> unit
        abstract WritePrimitiveArray : tag:string -> value:Array -> unit


    and IPickleFormatReader =
        inherit IDisposable

        abstract BeginReadRoot : unit -> string
        abstract EndReadRoot : unit -> unit

        abstract BeginReadObject : pickler:Pickler -> tag:string -> ObjectFlags
        abstract EndReadObject : unit -> unit
        
        abstract ReadBoolean : tag:string -> bool
        abstract ReadByte : tag:string -> byte
        abstract ReadSByte : tag:string -> sbyte

        abstract ReadInt16 : tag:string -> int16
        abstract ReadInt32 : tag:string -> int32
        abstract ReadInt64 : tag:string -> int64

        abstract ReadUInt16 : tag:string -> uint16
        abstract ReadUInt32 : tag:string -> uint32
        abstract ReadUInt64 : tag:string -> uint64

        abstract ReadDecimal : tag:string -> decimal
        abstract ReadSingle : tag:string -> float32
        abstract ReadDouble : tag:string -> float

        abstract ReadChar : tag:string -> char
        abstract ReadString : tag:string -> string

        abstract ReadBytes : tag:string -> byte []
        abstract ReadBytesFixed : tag:string -> length:int -> byte []
        abstract ReadToPrimitiveArray : tag:string -> Array -> unit

    and IPickleFormatProvider =
        abstract CreateWriter : Stream -> IPickleFormatWriter
        abstract CreateReader : Stream -> IPickleFormatReader