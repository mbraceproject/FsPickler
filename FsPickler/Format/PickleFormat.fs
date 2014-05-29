namespace Nessos.FsPickler

    open System
    open System.Text
    open System.IO

    type IPickleFormatWriter =

        inherit IDisposable

        abstract BeginWriteRoot : tag:string -> unit
        abstract EndWriteRoot : unit -> unit

        abstract BeginWriteObject : typeInfo:TypeInfo -> picklerInfo:PicklerInfo -> tag:string -> objectFlags:ObjectFlags -> unit
        abstract EndWriteObject : unit -> unit

        abstract BeginWriteBoundedSequence : tag:string -> length:int -> unit
        abstract EndWriteBoundedSequence : unit -> unit

        abstract BeginWriteUnBoundedSequence : tag:string -> unit
        abstract WriteHasNextElement : hasNext:bool -> unit
        
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

        abstract WriteChar : tag:string -> value:char -> unit
        abstract WriteString : tag:string -> value:string -> unit

        abstract WriteDate : tag:string -> date:DateTime -> unit
        abstract WriteTimeSpan : tag:string -> span:TimeSpan -> unit
        abstract WriteGuid : tag:string -> guid:Guid -> unit

        abstract WriteBigInteger : tag:string -> value:bigint -> unit
        abstract WriteBytes : tag:string -> value:byte [] -> unit

        abstract IsPrimitiveArraySerializationSupported : bool
        abstract WritePrimitiveArray : tag:string -> value:Array -> unit


    and IPickleFormatReader =
        inherit IDisposable

        abstract BeginReadRoot : tag:string -> unit
        abstract EndReadRoot : unit -> unit

        abstract BeginReadObject : typeInfo:TypeInfo -> picklerInfo:PicklerInfo -> tag:string -> ObjectFlags
        abstract EndReadObject : unit -> unit

        abstract BeginReadBoundedSequence : tag:string -> int
        abstract EndReadBoundedSequence : unit -> unit

        abstract BeginReadUnBoundedSequence : tag:string -> unit
        abstract ReadHasNextElement : unit -> bool
        
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
        
        abstract ReadDate : tag:string -> DateTime
        abstract ReadTimeSpan : tag:string -> TimeSpan
        abstract ReadGuid : tag:string -> Guid

        abstract ReadBigInteger : tag:string -> bigint
        abstract ReadBytes : tag:string -> byte []
//        abstract ReadBytesFixed : tag:string -> length:int -> byte []

        abstract IsPrimitiveArraySerializationSupported : bool
        abstract ReadPrimitiveArray : tag:string -> Array -> unit

    and IPickleFormatProvider =

        abstract CreateWriter : Stream -> IPickleFormatWriter
        abstract CreateReader : Stream -> IPickleFormatReader