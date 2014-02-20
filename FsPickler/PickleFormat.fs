namespace FsPickler

    open System
    open System.Text
    open System.IO

    type ObjectInfo = 
        | IsNull = 1uy
        | IsProperSubtype = 2uy
        | IsNewCacheableInstance = 4uy
        | IsCachedInstance = 8uy
        | IsCyclicInstance = 16uy
        | IsSequenceHeader = 32uy

    
    type IPickleFormatWriter =
        inherit IDisposable
        
        abstract WriteBool : tag:string -> value:bool -> unit
        abstract WriteByte : tag:string -> value:byte -> unit
        abstract WriteSByte : tag:string -> value:sbyte -> unit
        
        abstract WriteBytes : tag:string -> value:byte [] -> unit

        abstract WriteDecimal : tag:string -> value:decimal -> unit
        abstract WriteSingle : tag:string -> value:float32 -> unit
        abstract WriteDouble : tag:string -> value:float -> unit

        abstract WriteInt16 : tag:string -> value:int16 -> unit
        abstract WriteInt32 : tag:string -> value:int32 -> unit
        abstract WriteInt64 : tag:string -> value:int64 -> unit

        abstract WriteUInt16 : tag:string -> value:uint16 -> unit
        abstract WriteUInt32 : tag:string -> value:uint32 -> unit
        abstract WriteUInt64 : tag:string -> value:uint64 -> unit

        abstract WriteChar : tag:string -> value : char -> unit
        abstract WriteChars : tag:string -> value : char [] -> unit
        abstract WriteString : tag:string -> value:string -> unit

        abstract BeginWriteObject : tag:string -> info:ObjectInfo -> unit
        abstract EndWriteObject : unit -> unit


    and IPickleFormatReader =
        inherit IDisposable
        
        abstract ReadBool : tag:string -> bool
        abstract ReadByte : tag:string -> byte
        abstract ReadSByte : tag:string -> sbyte
        
        abstract ReadBytes : tag:string -> byte[]

        abstract ReadDecimal : tag:string -> decimal
        abstract ReadSingle : tag:string -> float32
        abstract ReadDouble : tag:string -> float

        abstract ReadInt16 : tag:string -> int16
        abstract ReadInt32 : tag:string -> int32
        abstract ReadInt64 : tag:string -> int64

        abstract ReadUInt16 : tag:string -> uint16
        abstract ReadUInt32 : tag:string -> uint32
        abstract ReadUInt64 : tag:string -> uint64

        abstract ReadChar : tag:string -> char
        abstract ReadChars : tag:string -> char []
        abstract ReadString : tag:string -> string

        abstract BeginReadObject : tag:string -> ObjectInfo
        abstract EndReadObject : unit -> unit

    and IPickleFormat =
        abstract GetWriter : Encoding -> Stream -> IPickleFormatWriter
        abstract GetReader : Encoding -> Stream -> IPickleFormatReader