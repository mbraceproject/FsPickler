namespace Nessos.FsPickler

open System
open System.Collections.Generic
open System.Runtime.Serialization

open Nessos.FsPickler.ReflectionCache

/// Base pickler type.

[<AbstractClass>]
type Pickler =
    class
        internal new : Type -> Pickler

        /// Type of values serialized by this pickler.
        member Type : System.Type

        /// The underlying type that this pickler implements.
        abstract member ImplementationType : System.Type

        /// Specifies if pickled objects are to be cached by reference.
        abstract member IsCacheByRef : bool

        /// Specifies if instances of this pickler type are of fixed size.
        member IsOfFixedSize : bool

        /// Specifies if instances of this pickler type can be cyclic objects.
        member IsRecursiveType : bool

        /// Pickler type classification
        member TypeKind : TypeKind

        /// Pickler generation metadata.
        abstract member PicklerInfo : PicklerInfo
            
        /// Specifies if this pickler can be applied to proper subtypes.
        abstract member UseWithSubtypes : bool

        abstract member internal Cast : unit -> Pickler<'S>
        abstract member internal Unpack : IPicklerUnpacker<'U> -> 'U

        abstract member internal UntypedRead : state:ReadState -> tag:string -> obj
        abstract member internal UntypedWrite : state:WriteState -> tag:string -> value:obj -> unit
        abstract member internal UntypedClone : state:CloneState -> obj -> obj
    end

/// Defines serialization rules for given type parameter.

and [<AbstractClass>] Pickler<'T> =
    class
        inherit Pickler
        internal new : unit -> Pickler<'T>

        abstract member Read : state:ReadState -> tag:string -> 'T
        abstract member Write : state:WriteState -> tag:string -> value:'T -> unit
        abstract member Clone : state:CloneState -> value:'T -> 'T

        override internal UntypedRead : state:ReadState -> tag:string -> obj
        override internal UntypedWrite : state:WriteState -> tag:string -> value:obj -> unit
        override internal UntypedClone : state:CloneState -> obj -> obj

        override internal Unpack : IPicklerUnpacker<'R> -> 'R
    end

and internal IPicklerUnpacker<'U> =
    interface
        abstract member Apply : Pickler<'T> -> 'U
    end

/// Object graph visitor abstraction.
and IObjectVisitor =
    interface
        /// <summary>
        ///     Visit provided value inside an object graph.
        /// </summary>
        /// <param name="pickler">Pickler used for traversal. Used for metadata reference.</param>
        /// <param name="value">Value that is being visited.</param>
        abstract member Visit<'T> : pickler:Pickler<'T> * value:'T -> unit
    end

/// Object graph sifting predicate.
and IObjectSifter =
    interface
        /// <summary>
        ///     Predicate deciding whether provided object is to be sifted from serialization.
        /// </summary>
        /// <param name="pickler">Pickler used for traversal. Used for metadata reference.</param>
        /// <param name="value">Value that is being visited.</param>
        abstract member Sift<'T> : pickler:Pickler<'T> * value:'T -> bool
    end
    
/// Provides access to automated pickler generation facility.
and IPicklerResolver =
    interface
        /// Identifies if instances of given type can be serialized.
        abstract member IsSerializable : System.Type -> bool
        /// Identifies if instances of given type can be serialized.
        abstract member IsSerializable<'T> : unit -> bool

        /// Attempt to generate a pickler instance for given type.
        abstract member Resolve : System.Type -> Pickler
        /// Attempt to generate a pickler instance for given type.
        abstract member Resolve : unit -> Pickler<'T>
    end

/// Contains all state related to object serializations
and [<AutoSerializable(false); Sealed>] WriteState =
    class
        internal new : 
            formatter:IPickleFormatWriter * resolver:IPicklerResolver * reflectionCache:ReflectionCache * 
                ?streamingContext:StreamingContext * ?visitor : IObjectVisitor * ?sifter : IObjectSifter -> WriteState

        member internal CyclicObjectSet : HashSet<int64>
        member internal ObjectStack : Stack<int64>
        member internal Formatter : IPickleFormatWriter
        member internal GetObjectId : obj:obj * firstTime:byref<bool> -> int64
        member internal ObjectCount : int64
        member internal PicklerResolver : IPicklerResolver
        member internal ReflectionCache : ReflectionCache
        /// Streaming context to the serialization
        member StreamingContext : StreamingContext
        member internal Visitor : IObjectVisitor option
        member internal Sifter : IObjectSifter option
        member internal Sifted : ResizeArray<int64 * obj>
        member internal TypePickler : Pickler<System.Type>
        member internal Reset : unit -> unit
    end
    
/// Contains all state related to object deserializations
and [<AutoSerializable(false); Sealed>] ReadState =
    class
        internal new : 
            formatter:IPickleFormatReader * resolver:IPicklerResolver * 
                reflectionCache:ReflectionCache * ?streamingContext:StreamingContext * ?sifted:(int64 * obj)[] -> ReadState

        member internal NextObjectId : unit -> int64
        member internal EarlyRegisterArray : array:System.Array -> unit
        member internal Formatter : IPickleFormatReader
        member internal IsUnSifting : bool
        member internal ObjectCache : Dictionary<int64,obj>
        member internal ObjectCount : int64
        member internal PicklerResolver : IPicklerResolver
        member internal ReflectionCache : ReflectionCache.ReflectionCache
        /// Streaming context to the deserialization
        member StreamingContext : StreamingContext
        member internal TypePickler : Pickler<System.Type>
        member internal Reset : unit -> unit
    end

/// Contains all state related to object cloning
and [<AutoSerializable(false); Sealed>] CloneState =
    class
        internal new : 
            resolver:IPicklerResolver * ?streamingContext:StreamingContext * ?sifter:IObjectSifter *
                   ?unSiftData:((int64 * obj) [] * (int64 * int64 []) []) -> CloneState

        
        member internal EarlyRegisterArray : array:Array -> unit
        member internal GetReferenceId : obj:obj * firstTime:byref<bool> -> int64
        member internal CyclicObjectSet : HashSet<int64>
        member internal ObjectCache : Dictionary<int64, obj>
        member internal ObjectStack : Stack<int64>
        member internal PicklerResolver : IPicklerResolver
        /// Gets the cloning streaming context.
        member StreamingContext : StreamingContext
        // sifting-related state
        member internal NextNodeId : unit -> int64
        member internal SiftData : (IObjectSifter * Dictionary<int64, (obj * ResizeArray<int64>)>) option
        member internal CreateSift : value:'T -> Sifted<'T> * (int64 * obj) []
        member internal UnSiftData : Dictionary<int64, int64 * obj> option
    end