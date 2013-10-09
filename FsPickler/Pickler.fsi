namespace FsPickler

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
            abstract member internal ManagedRead : Reader -> obj
            abstract member internal ManagedWrite : Writer -> obj -> unit

            // untyped version of reader/writers
            abstract member internal UntypedRead : Reader -> obj
            abstract member internal UntypedWrite : Writer -> obj -> unit

            // used internally for reading/writing sequences to the underlying stream
            abstract member internal WriteSequence : Writer * IEnumerable -> int
            abstract member internal ReadSequence : Reader * int -> IEnumerator

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
            /// Specifies if pickler type supports cyclic objects.
            member IsRecursiveType : bool
            /// Provides information on the pickler generation method.
            member PicklerInfo : PicklerInfo
            /// Cache Id used to generate this pickler.
            member CacheId : string
            member internal CacheId : string with set

            member internal IsInitialized : bool
            member internal TypeHash : TypeHash
            member internal TypeKind : TypeKind
            
        end

    and [<Sealed>] Pickler<'T> =
        class
            inherit Pickler

            internal new : unit -> Pickler<'T>
            internal new : (Reader -> 'T) * (Writer -> 'T -> unit) * PicklerInfo * cacheByRef:bool * useWithSubtypes:bool -> Pickler<'T>
            private new : nested:Pickler * (Reader -> 'T) * (Writer -> 'T -> unit) -> Pickler<'T>

            /// casts pickler to a typed version. may result in runtime error.
            override Cast : unit -> Pickler<'S>

            // gives access to reader/writer functions
            member internal Read : (Reader -> 'T)
            member internal Write : (Writer -> 'T -> unit)

            // used for recursive binding of picklers
            override internal InitializeFrom : Pickler -> unit

            // serialization managed by reader/writer objects
            override internal ManagedRead : Reader -> obj
            override internal ManagedWrite : Writer -> obj -> unit

            // used for recursive binding of picklers
            override internal UntypedRead : Reader -> obj
            override internal UntypedWrite : Writer -> obj -> unit
        end

    /// Pickler resolution interface
    and IPicklerResolver =
        interface
            /// Unique resolver identifier
            abstract member UUId : string
            /// untyped pickler generation
            abstract member internal Resolve : Type -> Pickler
            /// auto generates a pickler of type 'T
            abstract member Resolve : unit -> Pickler<'T>
        end

    /// Serialization state object.
    and Writer =
        class
            interface IDisposable

            internal new : Stream * IPicklerResolver * ?streamingContext:obj * ?leaveOpen:bool * ?encoding:Encoding -> Writer

            /// <summary>The Pickler resolver used by the writer.</summary>
            member internal Resolver : IPicklerResolver

//            member internal TryWriteFromCache : o:obj -> bool

            /// <summary>StreamingContext of the current serialization.</summary>
            member StreamingContext : StreamingContext

            /// <summary>BinaryWriter to underlying stream.</summary>
            member BinaryWriter : BinaryWriter

            /// <summary>Serialize value to the underlying stream. Pickler is resolved at runtime.</summary>
            /// <param name="value">The input value.</param>
            member Write : value:'T -> unit
            
            /// <summary>Serialize value to the underlying stream using the given pickler.</summary>
            /// <param name="pickler">Pickler used in serialization.</param>
            /// <param name="value">The input value.</param>
            member Write : pickler:Pickler<'T> * value:'T -> unit

            /// used internally for optimized writing of sequences to the underlying stream.
            /// must only be performed as a top-level-operation
            member internal WriteSequence<'T> : Pickler<'T> * sequence:seq<'T> -> int
        end

    /// Deserialization State object
    and Reader =
        class
            interface IDisposable

            internal new : Stream * IPicklerResolver * ?streamingContext:obj * ?leaveOpen:bool * ?encoding:Encoding -> Reader

            /// <summary>The Pickler resolver used by the reader.</summary>
            member internal Resolver : IPicklerResolver

//            /// used for external cache evaluation
//            member internal TryReadFromCache : success:byref<bool> -> obj
//            member internal CacheObj : obj -> unit
        
            /// <summary>StreamingContext of the current deserialization.</summary>
            member StreamingContext : StreamingContext

            /// <summary>BinaryReader to underlying stream.</summary> 
            member BinaryReader : BinaryReader

            /// <summary>Deserialize value from the underlying stream. Pickler is resolved at runtime.</summary>
            member Read : unit -> 'T

            /// <summary>Deserialize value from the underlying stream using the given pickler.</summary>
            /// <param name="pickler">Pickler used in deserialization.</param>
            member Read : pickler:Pickler<'T> -> 'T
        
            /// used internaly for cyclic object graphs
            member internal EarlyRegisterObject : obj -> unit

            /// used internally for optimized reading of sequences from the underlying stream.
            /// must only be performed as a top-level-operation
            member internal ReadSequence<'T> : Pickler<'T> * int -> IEnumerator<'T>
        end