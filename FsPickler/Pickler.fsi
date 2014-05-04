namespace Nessos.FsPickler

    open System
    open System.IO
    open System.Text
    open System.Collections
    open System.Collections.Generic
    open System.Runtime.Serialization

    /// The base type for all picklers
    [<AbstractClass>]
    type Pickler =
        class
            internal new : Type -> Pickler
            internal new : declaredType:Type * picklerType:Type * PicklerInfo * cacheByRef:bool * useWithSubtypes:bool -> Pickler

            /// casts pickler to a typed version; may result in runtime error.
            abstract member Cast<'S> : unit -> Pickler<'S>
            /// clones pickler
            abstract member internal ClonePickler : unit -> Pickler

            // serialization managed by reader/writer objects
            abstract member internal ReadRootObject : Reader * id:string -> obj
            abstract member internal WriteRootObject : Writer * id:string * value:obj -> unit

            // untyped version of reader/writers
            abstract member internal UntypedRead : Reader * managed:bool -> obj
            abstract member internal UntypedWrite : Writer * value:obj * managed:bool -> unit

            // used internally for reading/writing sequences to the underlying stream
            abstract member internal WriteSequence : Writer * id : string * sequence:IEnumerable -> int
            abstract member internal ReadSequence : Reader * id : string * length:int -> IEnumerator

            // used for recursive binding of picklers
            abstract member internal InitializeFrom : Pickler -> unit
            default internal InitializeFrom : Pickler -> unit

            /// Returns the pickler's declared type.
            member Type : Type
            /// Returns the pickler's actual type.
            member PicklerType : Type

            /// Specifies if pickled objects are to be cached by reference.
            member IsCacheByRef : bool
            /// Specifies if pickler can be used on objects of proper subtype.
            member UseWithSubtypes : bool
            /// Specifies if pickled type supports cyclic objects.
            member IsCyclicType : bool
            /// Specifies if pickled type has object graphs of fixed size.
            member IsFixedSize : bool
            /// Provides information on the pickler generation method.
            member PicklerInfo : PicklerInfo
            /// Cache Id used to generate this pickler.
            member CacheId : string
            member internal CacheId : string with set

            member internal IsInitialized : bool
            member internal PicklerFlags : PicklerFlags
            member internal TypeKind : TypeKind
            
        end

    and [<Sealed>] Pickler<'T> =
        class
            inherit Pickler

            internal new : unit -> Pickler<'T>
            internal new : (Reader -> 'T) * (Writer -> 'T -> unit) * PicklerInfo * cacheByRef:bool * useWithSubtypes:bool -> Pickler<'T>
            private new : nested:Pickler * (Reader -> 'T) * (Writer -> 'T -> unit) -> Pickler<'T>

            // gives access to reader/writer functions
            member internal Read : (Reader -> 'T)
            member internal Write : (Writer -> 'T -> unit)

        end

    /// Pickler resolution interface
    and IPicklerResolver =
        interface
            /// untyped pickler generation
            abstract member internal Resolve : Type -> Pickler
            /// auto generates a pickler of type 'T
            abstract member Resolve : unit -> Pickler<'T>
        end

    /// Serialization state object.
    and Writer =
        class
            interface IDisposable

            internal new : Stream * IPicklerResolver * ?streamingContext:StreamingContext * ?encoding:Encoding * ?leaveOpen:bool -> Writer

            /// <summary>The Pickler resolver used by the writer.</summary>
            member internal Resolver : IPicklerResolver

            /// <summary>StreamingContext of the current serialization.</summary>
            member StreamingContext : StreamingContext

            /// <summary>BinaryWriter to underlying stream.</summary>
            member BinaryWriter : BinaryWriter
            
            /// <summary>Serialize value to the underlying stream using the given pickler.</summary>
            /// <param name="pickler">Pickler used in serialization.</param>
            /// <param name="value">The input value.</param>
            member Write : pickler:Pickler<'T> * value:'T -> unit

            /// used internally for writing a root  object to the underlying stream.
            /// must only be performed as a top-level-operation
            member internal WriteRootObject : pickler:Pickler<'T> * id : string * value:'T -> unit

            /// used internally for optimized writing of sequences to the underlying stream.
            /// must only be performed as a top-level-operation
            member internal WriteSequence<'T> : Pickler<'T> * id : string * sequence:seq<'T> -> int
        end

    /// Deserialization State object
    and Reader =
        class
            interface IDisposable

            internal new : Stream * IPicklerResolver * ?streamingContext:StreamingContext * ?encoding:Encoding * ?leaveOpen:bool -> Reader

            /// <summary>The Pickler resolver used by the reader.</summary>
            member internal Resolver : IPicklerResolver
        
            /// <summary>StreamingContext of the current deserialization.</summary>
            member StreamingContext : StreamingContext

            /// <summary>BinaryReader to underlying stream.</summary> 
            member BinaryReader : BinaryReader

            /// <summary>Deserialize value from the underlying stream using the given pickler.</summary>
            /// <param name="pickler">Pickler used in deserialization.</param>
            member Read : pickler:Pickler<'T> -> 'T
        
            /// used internally for cyclic object graphs
            member internal EarlyRegisterArray : Array -> unit

            /// used internally for reading root  object from the underlying stream.
            /// must only be performed as a top-level-operation
            member internal ReadRootObject : pickler:Pickler<'T> * id : string -> 'T

            /// used internally for optimized reading of sequences from the underlying stream.
            /// must only be performed as a top-level-operation
            member internal ReadSequence<'T> : Pickler<'T> * id : string * length : int -> IEnumerator<'T>
        end