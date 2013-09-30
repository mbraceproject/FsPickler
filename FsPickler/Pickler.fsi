namespace FsPickler

    open System
    open System.IO
    open System.Text
    open System.Runtime.Serialization

    /// The base type for all picklers
    [<AbstractClass>]
    type Pickler =
        class
            internal new : Type -> Pickler
            internal new : Type * PicklerInfo * bool * bool -> Pickler

            val private declared_type: Type
            val private is_recursive_type: bool
            val mutable private m_pickler_type: Type
            val mutable private m_typeInfo: TypeInfo
            val mutable private m_typeHash: TypeHash
            val mutable private m_isInitialized: bool
            val mutable private m_picklerInfo: PicklerInfo
            val mutable private m_isCacheByRef: bool
            val mutable private m_useWithSubtypes: bool

            /// casts pickler to a typed version. may result in runtime error.
            abstract member Cast<'S> : unit -> Pickler<'S>

            // serialization managed by reader/writer objects
            abstract member internal ManagedRead : Reader -> obj
            abstract member internal ManagedWrite : Writer -> obj -> unit

            // untyped version of reader/writers
            abstract member internal UntypedRead : Reader -> obj
            abstract member internal UntypedWrite : Writer -> obj -> unit

            // used for recursive binding of picklers
            abstract member internal InitializeFrom : Pickler -> unit
            default internal InitializeFrom : Pickler -> unit

            /// The declared generic type of given pickler.
            member Type : Type
            /// The actual type of the given pickler.
            member PicklerType : Type

            /// Specifies if pickled objects are to be cached by reference.
            member IsCacheByRef : bool
            /// Specifies if pickler can be used on objects of proper subtype.
            member UseWithSubtypes : bool
            /// Specifies if given object graphs of given type can be cyclic.
            member IsRecursiveType : bool
            /// Provides information on the pickler generation method.
            member PicklerInfo : PicklerInfo

            member internal IsInitialized : bool
            member internal TypeHash : TypeHash
            member internal TypeInfo : TypeInfo
        end

    and [<Sealed>] Pickler<'T> =
        class
            inherit Pickler

            internal new : unit -> Pickler<'T>
            internal new : (Reader -> 'T) * (Writer -> 'T -> unit) * 
                                        PicklerInfo * cacheByRef:bool * useWithSubtypes:bool -> Pickler<'T>

            private new : Type * (Reader -> 'T) * (Writer -> 'T -> unit) * 
                                                    PicklerInfo * cacheByRef:bool * useWithSubtypes:bool -> Pickler<'T>

            val mutable private m_writer: Writer -> 'T -> unit
            val mutable private m_reader: Reader -> 'T

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
            /// auto generates a pickler of type 'T
            abstract member Resolve : unit -> Pickler<'T>
            abstract member internal Resolve : Type -> Pickler
        end

    /// Serialization state object.
    and Writer =
        class
            interface IDisposable

            internal new : Stream * IPicklerResolver * ?streamingContext:obj * ?leaveOpen:bool * ?encoding:Encoding -> Writer

            /// <summary>The Pickler resolver used by the writer.</summary>
            member internal Resolver : IPicklerResolver

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

            /// internal unsafe serialization method
            member internal WriteObj : t:Type * o:obj -> unit

        end

    /// Deserialization State object
    and Reader =
        class
            interface IDisposable

            internal new : Stream * IPicklerResolver * ?streamingContext:obj * ?leaveOpen:bool * ?encoding:Encoding -> Reader

            /// <summary>The Pickler resolver used by the reader.</summary>
            member internal Resolver : IPicklerResolver
        
            /// <summary>StreamingContext of the current deserialization.</summary>
            member StreamingContext : StreamingContext

            /// <summary>BinaryReader to underlying stream.</summary> 
            member BinaryReader : BinaryReader

            /// <summary>Deserialize value from the underlying stream. Pickler is resolved at runtime.</summary>
            member Read : unit -> 'T

            /// <summary>Deserialize value from the underlying stream using the given pickler.</summary>
            /// <param name="pickler">Pickler used in deserialization.</param>
            member Read : pickler:Pickler<'T> -> 'T

            /// internal unsafe deserialization method
            member internal ReadObj : Type -> obj
        
            /// used internaly for cyclic object graphs
            member internal EarlyRegisterObject : obj -> unit
        end