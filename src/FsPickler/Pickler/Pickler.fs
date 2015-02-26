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

    let typeKind = TypeKind.compute t
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
    member __.TypeKind = typeKind
    member __.IsOfFixedSize = isOfFixedSize
    member __.IsRecursiveType = isRecursive

    abstract ImplementationType : Type

    abstract PicklerInfo : PicklerInfo
    abstract IsCacheByRef : bool
    abstract UseWithSubtypes : bool

    abstract UntypedWrite : state:WriteState -> tag:string -> value:obj -> unit
    abstract UntypedRead  : state:ReadState  -> tag:string -> obj

    abstract Unpack : IPicklerUnpacker<'U> -> 'U

    abstract Cast<'S> : unit -> Pickler<'S>
and
    [<AutoSerializable(false)>]
    [<AbstractClass>]
    Pickler<'T> internal () =
    inherit Pickler (typeof<'T>)

    abstract Write : state:WriteState -> tag:string -> value:'T -> unit
    abstract Read  : state:ReadState  -> tag:string -> 'T

    override p.UntypedWrite (state : WriteState) (tag : string) (value : obj) = p.Write state tag (fastUnbox value)
    override p.UntypedRead (state : ReadState) (tag : string) = p.Read state tag :> _

    override p.Unpack unpacker = unpacker.Apply p

and IPicklerUnpacker<'U> =
    abstract Apply : Pickler<'T> -> 'U

and IObjectVisitor =
    abstract Visit<'T> : 'T -> unit

and IPicklerResolver =
    abstract IsSerializable : Type -> bool
    abstract IsSerializable<'T> : unit -> bool

    abstract Resolve : Type -> Pickler
    abstract Resolve<'T> : unit -> Pickler<'T>

and [<AutoSerializable(false)>] 
    WriteState internal (formatter : IPickleFormatWriter, resolver : IPicklerResolver, 
                            reflectionCache : ReflectionCache, ?streamingContext, ?visitor : IObjectVisitor) =

    let tyPickler = resolver.Resolve<Type> ()

    let sc = match streamingContext with None -> new StreamingContext() | Some sc -> sc

    let mutable objCount = 0L
    let mutable idGen = new ObjectIDGenerator()
    let objStack = new Stack<int64> ()
    let cyclicObjects = new HashSet<int64> ()

    member internal __.PicklerResolver = resolver
    member __.StreamingContext = sc
    member internal __.Formatter = formatter
    member internal __.Visitor = visitor
    member internal __.ReflectionCache = reflectionCache
    member internal __.TypePickler = tyPickler
    member internal __.GetObjectId(obj:obj, firstTime:byref<bool>) =
        let id = idGen.GetId(obj, &firstTime)
        if firstTime then objCount <- id
        id

    member internal __.ObjectCount = objCount
    member internal __.ObjectStack = objStack
    member internal __.CyclicObjectSet = cyclicObjects
    member internal __.Reset () =
        objCount <- 0L
        idGen <- new ObjectIDGenerator()
        objStack.Clear()
        cyclicObjects.Clear()

and [<AutoSerializable(false)>] 
    ReadState internal (formatter : IPickleFormatReader, resolver : IPicklerResolver, reflectionCache : ReflectionCache, ?streamingContext) =
        
    let sc = match streamingContext with None -> new StreamingContext() | Some sc -> sc

    let mutable idCounter = 0L
    let objCache = new Dictionary<int64, obj> ()
    let tyPickler = resolver.Resolve<Type> ()

    member internal __.PicklerResolver = resolver
    member internal __.Formatter = formatter
    member internal __.TypePickler = tyPickler
    member internal __.ReflectionCache = reflectionCache
    member __.StreamingContext = sc
    member internal __.ObjectCache = objCache
    member internal __.NextObjectId () =
        idCounter <- idCounter + 1L
        idCounter

    member internal __.ObjectCount = idCounter

    member internal __.EarlyRegisterArray(array : Array) =
        objCache.Add(idCounter, array)

    member internal __.Reset () =
        idCounter <- 0L
        objCache.Clear()