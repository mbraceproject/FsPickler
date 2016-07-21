namespace MBrace.FsPickler

open System
open System.Collections.Generic
open System.Runtime.Serialization

open MBrace.FsPickler.ReflectionCache

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

        /// Specifies if instances of this type are of fixed size.
        member IsOfFixedSize : bool

        /// Specifies if instances of this type can be cyclic objects.
        member IsRecursiveType : bool
        /// Specifies if objects graphs of this type can contain open hierarchies.
        member IsOpenHierarchy : bool

        /// Pickler type classification
        member Kind : Kind

        /// Pickler generation metadata.
        abstract member PicklerInfo : PicklerInfo
            
        /// Specifies if this pickler can be applied to proper subtypes.
        abstract member UseWithSubtypes : bool

        /// Specifies that pickler provides logic only for object cloning/visiting/hashing
        /// and that type is not otherwise serializable.
        abstract member IsCloneableOnly : bool

        abstract member internal Cast : unit -> Pickler<'S>
        abstract member internal Unpack : IPicklerUnpacker<'U> -> 'U

        abstract member internal UntypedRead : state:ReadState -> tag:string -> obj
        abstract member internal UntypedWrite : state:WriteState -> tag:string -> value:obj -> unit
        abstract member internal UntypedClone : state:CloneState -> value:obj -> obj
        abstract member internal UntypedAccept : state:VisitState -> value:obj -> unit
    end

/// Defines serialization rules for given type parameter.

and [<AbstractClass>] Pickler<'T> =
    class
        inherit Pickler
        internal new : unit -> Pickler<'T>

        /// <summary>
        ///     Deserializes a value with provided tag from reader state.
        /// </summary>
        /// <param name="state">Object deserialization state.</param>
        /// <param name="tag">String identifier for value.</param>
        abstract member Read : state:ReadState -> tag:string -> 'T

        /// <summary>
        ///     Serializes a value with provided tag to the underlying writer state.
        /// </summary>
        /// <param name="state">Object serialization state.</param>
        /// <param name="tag">String identifier for value.</param>
        /// <param name="value">Value to be serialized.</param>
        abstract member Write : state:WriteState -> tag:string -> value:'T -> unit

        /// <summary>
        ///     Clones a value using the underlying cloning state.
        /// </summary>
        /// <param name="state">Object cloning state.</param>
        /// <param name="value">Value to be cloned.</param>
        abstract member Clone : state:CloneState -> value:'T -> 'T

        /// <summary>
        ///     Accepts visitor for traversal of child nodes of given value.
        /// </summary>
        /// <param name="state">Visitor state.</param>
        /// <param name="value">Value to be visited.</param>
        abstract member Accept : state:VisitState -> value:'T -> unit

        override internal UntypedRead : state:ReadState -> tag:string -> obj
        override internal UntypedWrite : state:WriteState -> tag:string -> value:obj -> unit
        override internal UntypedClone : state:CloneState -> obj -> obj
        override internal UntypedAccept : state:VisitState -> value:obj -> unit

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
        abstract member Visit<'T> : pickler:Pickler<'T> * value:'T -> bool
    end

/// Specialized object visitor abstraction.
and ISpecializedObjectVisitor<'T> =
    interface
        inherit IObjectVisitor

        /// <summary>
        ///     Visit value inside an object graph that matches given type.
        /// </summary>
        /// <param name="pickler">Pickler used for traversal. Used for metadata reference.</param>
        /// <param name="value">Value that is being visited.</param>
        abstract VisitSpecialized : pickler:Pickler<'T> * value:'T -> bool
    end

/// Object graph sifting predicate.
and IObjectSifter =
    interface
        /// <summary>
        ///     Predicate deciding whether provided object is to be sifted from serialization.
        /// </summary>
        /// <param name="pickler">Pickler used for traversal. Used for metadata reference.</param>
        /// <param name="id">Object id for current value.</param>
        /// <param name="value">Value that is being visited.</param>
        abstract member Sift<'T> : pickler:Pickler<'T> * id:int64 * value:'T -> bool
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
                isHashComputation:bool * disableSubtypeResolution:bool *
                ?streamingContext:StreamingContext * ?sifter : IObjectSifter -> WriteState

        /// Identifies this serialization session as a hash computation
        member IsHashComputation : bool
        /// Do not allow subtype resolution when serializing classes
        member DisableSubtypeResolution : bool
        /// Set containing the id's of all objects identified as cyclic
        member internal CyclicObjectSet : HashSet<int64>
        /// Stack containing all object id's that are currently being serialized.
        /// Used for identifying cyclic objects.
        member internal ObjectStack : Stack<int64>
        /// Serialization format provider
        member internal Formatter : IPickleFormatWriter
        /// Gets an object id which is unique by reference
        member internal GetObjectId : obj:obj * firstTime:byref<bool> -> int64
        /// Total number of serialized objects
        member internal ObjectCount : int64
        /// Pickler resolver instance used for dynamic pickler resolution.
        member internal PicklerResolver : IPicklerResolver
        /// Cache for quick reflection type serialization.
        member internal ReflectionCache : ReflectionCache
        /// Streaming context to the serialization
        member StreamingContext : StreamingContext
        /// Optional object sifter predicate.
        member internal Sifter : IObjectSifter option
        /// Contains all currently sifted objects.
        member internal Sifted : ResizeArray<int64 * obj>
        /// Pickler for serializing System.Type instances.
        member internal TypePickler : Pickler<System.Type>
        /// Resets the serialization state.
        member internal Reset : unit -> unit
    end
    
/// Contains all state related to object deserializations
and [<AutoSerializable(false); Sealed>] ReadState =
    class
        internal new : 
            formatter:IPickleFormatReader * resolver:IPicklerResolver * 
                reflectionCache:ReflectionCache * disableSubtypeResolution : bool * disableAssemblyLoading : bool *
                ?streamingContext:StreamingContext * ?sifted:(int64 * obj)[] -> ReadState

        /// Do not allow subtype resolution when deserializing classes
        member DisableSubtypeResolution : bool
        /// Disable assembly loading when deserializing classes specifying System.Reflection.Assembly instances
        member DisableAssemblyLoading : bool
        /// Generates an object id for the upcoming object
        member internal NextObjectId : unit -> int64
        /// Register's array instances upon initialization and before
        /// element deserialization has taken place. This is done to
        /// properly deserialize cyclic array instances.
        member internal EarlyRegisterArray : array:System.Array -> unit
        /// Deserialization format provider
        member internal Formatter : IPickleFormatReader
        /// In unsifting deserialization instance
        member internal IsUnSifting : bool
        /// Object deserialization cache indexed by id
        member internal ObjectCache : Dictionary<int64,obj>
        /// Number of deserialized objects
        member internal ObjectCount : int64
        /// Pickler resolver used for runtime deserializations
        member internal PicklerResolver : IPicklerResolver
        /// Reflection cache used for quick reflection type deserialization.
        member internal ReflectionCache : ReflectionCache.ReflectionCache
        /// Streaming context to the deserialization
        member StreamingContext : StreamingContext
        /// Pickler for System.Type serializations.
        member internal TypePickler : Pickler<System.Type>
        /// Reset deserializer state.
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
        /// Declares that the current node was found to be of proper subtyped.
        /// Used for proper NodeId generation.
        member internal DeclareProperSubtype : unit -> unit
        /// Object sifting state
        member internal SiftData : (IObjectSifter * Dictionary<int64, (obj * ResizeArray<int64>)>) option
        /// Object unsifting state
        member internal UnSiftData : Dictionary<int64, int64 * obj> option
        /// Create a sifted object using accumulated sifting data
        member internal CreateSift : value:'T -> Sifted<'T> * (int64 * obj) []
    end

/// Contains all state related to object visiting
and [<AutoSerializable(false); Sealed>] VisitState =
    class
        internal new : 
            resolver:IPicklerResolver * visitor:IObjectVisitor *
                ?visitOrder:VisitOrder * ?streamingContext:StreamingContext -> VisitState

        member internal IsCancelled : bool
        member internal ObjectIDGenerator : ObjectIDGenerator
        member internal PicklerResolver : IPicklerResolver
        /// Gets the visiting streaming context.
        member StreamingContext : StreamingContext
        member internal VisitOrder : VisitOrder
        member internal Visitor : IObjectVisitor
        member internal IsCancelled : bool with set
    end