namespace Nessos.FsPickler

open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.Runtime.Serialization

open Nessos.FsPickler.Reflection
open Nessos.FsPickler.ReflectionCache

[<AbstractClass>]
[<AutoSerializable(false)>]
type Pickler internal (t : Type) =

    let typeKind = Kind.compute t
    let isRecursive = 
        try isRecursiveType t
        with 
        | PolymorphicRecursiveException t' when t = t' -> 
            let msg = "type is polymorphic recursive."
            raise <| NonSerializableTypeException(t, msg)
        | PolymorphicRecursiveException t' ->
            let msg = sprintf "contains polymorphic recursive type '%O'." t'
            raise <| NonSerializableTypeException(t, msg)

    let isOfFixedSize = isOfFixedSize isRecursive t
    // In order for a type to be considered recursive,
    // it must additionally be a reference type
    let isRecursive = isRecursive && not t.IsValueType

    member __.Type = t
    member __.Kind = typeKind
    member __.IsOfFixedSize = isOfFixedSize
    member __.IsRecursiveType = isRecursive

    abstract ImplementationType : Type

    abstract PicklerInfo : PicklerInfo
    abstract IsCacheByRef : bool
    abstract UseWithSubtypes : bool

    abstract UntypedWrite : state:WriteState -> tag:string -> value:obj -> unit
    abstract UntypedRead  : state:ReadState  -> tag:string -> obj
    abstract UntypedClone : state:CloneState -> value:obj -> obj
    abstract UntypedAccept : state:VisitState -> value:obj -> unit

    abstract Unpack : IPicklerUnpacker<'U> -> 'U

    abstract Cast<'S> : unit -> Pickler<'S>
and
    [<AutoSerializable(false)>]
    [<AbstractClass>]
    Pickler<'T> internal () =
    inherit Pickler (typeof<'T>)

    abstract Write : state:WriteState -> tag:string -> value:'T -> unit
    abstract Read  : state:ReadState  -> tag:string -> 'T
    abstract Clone : state:CloneState -> value:'T -> 'T
    abstract Accept : state:VisitState -> value:'T -> unit

    override p.UntypedWrite (state : WriteState) (tag : string) (value : obj) = p.Write state tag (fastUnbox value)
    override p.UntypedRead (state : ReadState) (tag : string) = p.Read state tag :> _
    override p.UntypedClone (state : CloneState) (value : obj) = p.Clone state (fastUnbox value) :> _
    override p.UntypedAccept(state : VisitState) (value : obj) = p.Accept state (fastUnbox value)

    override p.Unpack unpacker = unpacker.Apply p

and IPicklerUnpacker<'U> =
    abstract Apply : Pickler<'T> -> 'U

and IObjectVisitor =
    abstract Visit<'T> : pickler:Pickler<'T> * value:'T -> bool

and ISpecializedObjectVisitor<'T> =
    inherit IObjectVisitor
    abstract VisitSpecialized : pickler:Pickler<'T> * value:'T -> bool

and IObjectSifter =
    abstract member Sift<'T> : pickler:Pickler<'T> * value:'T -> bool

and IPicklerResolver =
    abstract IsSerializable : Type -> bool
    abstract IsSerializable<'T> : unit -> bool

    abstract Resolve : Type -> Pickler
    abstract Resolve<'T> : unit -> Pickler<'T>

and [<AutoSerializable(false); Sealed>]
    WriteState internal (formatter : IPickleFormatWriter, resolver : IPicklerResolver, 
                            reflectionCache : ReflectionCache, ?streamingContext, ?sifter : IObjectSifter) =

    let tyPickler = resolver.Resolve<Type> ()

    let sc = match streamingContext with None -> new StreamingContext() | Some sc -> sc

    let mutable currentId = 0L
    let mutable idGen = new ObjectIDGenerator()
    let objStack = new Stack<int64> ()
    let cyclicObjects = new HashSet<int64> ()
    let sifted = new ResizeArray<int64 * obj> ()

    member internal __.PicklerResolver = resolver
    member __.StreamingContext = sc
    member internal __.Formatter = formatter
    member internal __.Sifter = sifter
    member internal __.Sifted = sifted
    member internal __.ReflectionCache = reflectionCache
    member internal __.TypePickler = tyPickler
    member internal __.GetObjectId(obj:obj, firstTime:byref<bool>) =
        let id = idGen.GetId(obj, &firstTime)
        if firstTime then currentId <- id
        id

    member internal __.ObjectCount = currentId
    member internal __.ObjectStack = objStack
    member internal __.CyclicObjectSet = cyclicObjects
    member internal __.Reset () =
        currentId <- 0L
        idGen <- new ObjectIDGenerator()
        objStack.Clear()
        cyclicObjects.Clear()

and [<AutoSerializable(false); Sealed>]
    ReadState internal (formatter : IPickleFormatReader, resolver : IPicklerResolver, reflectionCache : ReflectionCache, 
                            ?streamingContext : StreamingContext, ?sifted : (int64 * obj) []) =
        
    let sc = match streamingContext with None -> new StreamingContext() | Some sc -> sc

    let mutable currentId = 0L
    let isUnsifting = Option.isSome sifted
    let objCache = new Dictionary<int64, obj> ()
    do 
        match sifted with
        | None -> ()
        | Some s -> for id,value in s do objCache.Add(id, value)

    let tyPickler = resolver.Resolve<Type> ()

    member internal __.PicklerResolver = resolver
    member internal __.IsUnSifting = isUnsifting
    member internal __.Formatter = formatter
    member internal __.TypePickler = tyPickler
    member internal __.ReflectionCache = reflectionCache
    member __.StreamingContext = sc
    member internal __.ObjectCache = objCache
    member internal __.NextObjectId () =
        currentId <- currentId + 1L
        currentId

    member internal __.ObjectCount = currentId

    member internal __.EarlyRegisterArray(array : Array) =
        objCache.Add(currentId, array)

    member internal __.Reset () =
        currentId <- 0L
        objCache.Clear()

and [<AutoSerializable(false); Sealed>] 
    CloneState internal (resolver : IPicklerResolver, ?streamingContext : StreamingContext, 
                                ?sifter : IObjectSifter, ?unSiftData : (int64 * obj) [] * (int64 * int64[]) []) =

    let sc = match streamingContext with None -> new StreamingContext() | Some sc -> sc

    let mutable currentId = 0L
    let mutable idGen = new ObjectIDGenerator()
    let objStack = new Stack<int64> ()
    let cyclicObjects = new HashSet<int64> ()
    let objCache = new Dictionary<int64, obj> ()

    let mutable nodeCount = 0L

    let siftData =
        match sifter with
        | Some s -> 
            let state = new Dictionary<int64, obj * ResizeArray<int64>>()
            Some(s, state)
        | None -> None
        
    // merges usift metadata and values in a single dictionary
    let unsiftData = 
        match unSiftData with
        | Some(_, [||]) | None -> None
        | Some(values, indices) ->
            let vdict = new Dictionary<int64, int64 * obj> ()
            for (id,_) as v in values do vdict.Add(id, v)
            let dict = new Dictionary<int64, int64 * obj> ()
            for id,is in indices do
                let mutable value = Unchecked.defaultof<int64 * obj>
                if vdict.TryGetValue(id, &value) then
                    for i in is do dict.Add(i, value)
                else
                    let msg = sprintf "Expected could not locate unsift value for id %d." id
                    invalidArg "values" msg

            Some dict

    member __.StreamingContext = sc

    member internal __.PicklerResolver = resolver
    member internal __.GetReferenceId(obj:obj, firstTime:byref<bool>) = 
        let id = idGen.GetId(obj, &firstTime)
        if firstTime then currentId <- id
        id

    member internal __.ObjectCache = objCache
    member internal __.EarlyRegisterArray(array : Array) = objCache.Add(currentId, array)
    member internal __.ObjectStack = objStack
    member internal __.CyclicObjectSet = cyclicObjects

    member internal __.NextNodeId() = let nc = nodeCount in nodeCount <- nc + 1L ; nc
    member internal __.UnSiftData = unsiftData
    member internal __.SiftData = siftData
    /// Creates a sift pair using provided sifted value.
    member internal __.CreateSift (value : 'T) = 
        let _,dict = Option.get siftData
        let indices = Array.zeroCreate<int64 * int64 []> dict.Count
        let values = Array.zeroCreate<int64 * obj> dict.Count
        let mutable i = 0
        for kv in dict do
            let id = kv.Key
            let obj, ra = kv.Value
            indices.[i] <- id, ra.ToArray()
            values.[i] <- id, obj
            i <- i + 1

        let sift = new Sifted<'T>(value, indices)
        sift, values

and [<AutoSerializable(false); Sealed>]
    VisitState internal (resolver : IPicklerResolver, visitor : IObjectVisitor, ?visitOrder : VisitOrder, ?streamingContext : StreamingContext) =

    let sc = match streamingContext with None -> new StreamingContext() | Some sc -> sc

    let mutable isCancelled = false
    let mutable idGen = new ObjectIDGenerator()
    let visitOrder = defaultArg visitOrder VisitOrder.PreOrder

    member __.StreamingContext = sc
    member internal __.VisitOrder = visitOrder
    member internal __.PicklerResolver = resolver
    member internal __.Visitor = visitor
    member internal __.ObjectIDGenerator = idGen
    member internal __.IsCancelled
        with get () = isCancelled
        and set s = isCancelled <- s