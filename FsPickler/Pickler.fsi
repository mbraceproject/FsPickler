namespace FsPickler

    open System
    open System.IO
    open System.Text
    open System.Runtime.Serialization

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
        val mutable private m_cacheObj: bool
        val mutable private m_useWithSubtypes: bool

        abstract member internal Cast : unit -> Pickler<'S>
        abstract member internal ManagedRead : Reader -> obj
        abstract member internal ManagedWrite : Writer -> obj -> unit
        abstract member internal UntypedRead : Reader -> obj
        abstract member internal UntypedWrite : Writer -> obj -> unit

        abstract member internal InitializeFrom : Pickler -> unit
        override internal InitializeFrom : Pickler -> unit

        member Type : Type
        member PicklerType : Type

        member CacheObj : bool
        member TypeInfo : TypeInfo
        member UseWithSubtypes : bool
        member IsRecursiveType : bool
        member PicklerInfo : PicklerInfo

        member internal IsInitialized : bool
        member internal TypeHash : TypeHash
      end

    and [<Sealed>] Pickler<'T> =
      class
        inherit Pickler

        internal new : unit -> Pickler<'T>
        internal new : (Reader -> 'T) * (Writer -> 'T -> unit) * PicklerInfo * cacheObj:bool * useWithSubtypes:bool -> Pickler<'T>
        private new : Type * (Reader -> 'T) * (Writer -> 'T -> unit) * PicklerInfo * cacheObj:bool * useWithSubtypes:bool -> Pickler<'T>

        val mutable private m_writer: Writer -> 'T -> unit
        val mutable private m_reader: Reader -> 'T

        override internal Cast : unit -> Pickler<'S>
        override internal InitializeFrom : Pickler -> unit
        override internal ManagedRead : Reader -> obj
        override internal ManagedWrite : Writer -> obj -> unit
        override internal UntypedRead : Reader -> obj
        override internal UntypedWrite : Writer -> obj -> unit

        member internal Read : (Reader -> 'T)
        member internal Write : (Writer -> 'T -> unit)

      end

    and IPicklerResolver =
      interface
        abstract member Resolve : unit -> Pickler<'T>
        abstract member internal Resolve : Type -> Pickler
      end

    and Writer =
      class
        interface IDisposable

        internal new : Stream * IPicklerResolver * StreamingContext * ?leaveOpen:bool * ?encoding:Encoding -> Writer

        member ResolvePickler : unit -> Pickler<'T>

        member Write : 'T -> unit
        member Write : Pickler<'T> * 'T -> unit

        member internal WriteObj : Type * obj -> unit
        member internal BW : BinaryWriter

        member StreamingContext : StreamingContext

      end

    and Reader =
      class
        interface IDisposable

        internal new : Stream * IPicklerResolver * StreamingContext * ?leaveOpen:bool * ?encoding:Encoding -> Reader
        member internal EarlyRegisterObject : obj -> unit

        member Read : unit -> 'T
        member Read : Pickler<'T> -> 'T
        member internal ReadObj : Type -> obj
        member ResolvePickler : unit -> Pickler<'T>
        member internal BR : BinaryReader
        member StreamingContext : StreamingContext

      end