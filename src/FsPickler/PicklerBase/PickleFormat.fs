namespace Nessos.FsPickler

    open System
    open System.Text
    open System.IO

    type IPickleFormatWriter =

        inherit IDisposable
        
        /// <summary>
        ///     Writes the start of the pickle.
        /// </summary>
        /// <param name="tag">pickle identifier.</param>
        abstract BeginWriteRoot : tag:string -> unit
        /// <summary>
        ///     Writes the end of the pickle.
        /// </summary>
        abstract EndWriteRoot : unit -> unit

        /// <summary>
        ///     Start writing a new object to the pickle.
        /// </summary>
        /// <param name="tag">object identifier.</param>
        /// <param name="objectFlags">runtime object flags.</param>
        abstract BeginWriteObject : tag:string -> objectFlags:ObjectFlags -> unit
        /// <summary>
        ///     End write of an object.
        /// </summary>
        abstract EndWriteObject : unit -> unit

        /// <summary>
        ///     Start writing a new array to the pickle. The array has a predetermined length.
        /// </summary>
        /// <param name="tag">array identifier.</param>
        /// <param name="length">array length.</param>
        abstract BeginWriteBoundedSequence : tag:string -> length:int -> unit
        /// <summary>
        ///     End write a bounded array.
        /// </summary>
        abstract EndWriteBoundedSequence : unit -> unit

        /// <summary>
        ///     Start writing a new array to the pickle. The array has undetermined length.
        /// </summary>
        /// <param name="tag">array identifier.</param>
        abstract BeginWriteUnBoundedSequence : tag:string -> unit
        /// <summary>
        ///     Writes boolean indicating that unbounded array has an upcoming element.
        /// </summary>
        /// <param name="hasNext">next element indicator.</param>
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

        /// specifies if the format supports custom serialization for primitive arrays.
        /// this functionality is reserved for binary formats that use Buffer.BlockCopy
        abstract IsPrimitiveArraySerializationSupported : bool

        /// <summary>
        ///     Write primitive array contents to pickle
        /// </summary>
        /// <param name="tag">array identifier.</param>
        /// <param name="value">source array.</param>
        abstract WritePrimitiveArray : tag:string -> value:Array -> unit


    and IPickleFormatReader =
        inherit IDisposable

        /// <summary>
        ///     Begin reading the pickle.
        /// </summary>
        /// <param name="tag">pickle identifier.</param>
        abstract BeginReadRoot : tag:string -> unit
        /// <summary>
        ///     End reading the pickle.
        /// </summary>
        abstract EndReadRoot : unit -> unit

        /// <summary>
        ///     Begin reading a new object.
        /// </summary>
        /// <param name="tag">object identifier.</param>
        abstract BeginReadObject : tag:string -> ObjectFlags
        /// <summary>
        ///     End reading an object.
        /// </summary>
        abstract EndReadObject : unit -> unit

        /// <summary>
        ///     Begin reading an array of predetermined length.
        /// </summary>
        /// <param name="tag">array identifier.</param>
        /// <returnType>array length.</returnType>
        abstract BeginReadBoundedSequence : tag:string -> int

        /// <summary>
        ///     End reading an array of predetermined length.
        /// </summary>
        abstract EndReadBoundedSequence : unit -> unit

        /// <summary>
        ///     Begins reading an array of undetermined length
        /// </summary>
        /// <param name="tag">array identifier.</param>
        abstract BeginReadUnBoundedSequence : tag:string -> unit

        /// <summary>
        ///     Read if unbounded array has more elements.
        /// </summary>
        /// <returnType>True iff array has more elements.</returnType>
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

        /// specifies if the format supports custom serialization for primitive arrays.
        /// this functionality is reserved for binary formats that use Buffer.BlockCopy
        abstract IsPrimitiveArraySerializationSupported : bool
        /// <summary>
        ///     Copies data into preallocated primitive array.
        /// </summary>
        /// <param name="tag">array identifier.</param>
        /// <param name="target">target array.</param>
        abstract ReadPrimitiveArray : tag:string -> target:Array -> unit

    /// Factory abstraction for binary pickle formats.
    type IPickleFormatProvider =

        /// Pickle format name
        abstract Name : string

        /// Initializes a new format writer
        abstract CreateWriter : Stream * Encoding * leaveOpen:bool -> IPickleFormatWriter
        /// Initializes a new format reader
        abstract CreateReader : Stream * Encoding * leaveOpen:bool -> IPickleFormatReader

    /// Factory abstraction for text-based pickle formats.
    and ITextPickleFormatProvider =
        inherit IPickleFormatProvider

        /// Initializes a new format writer
        abstract CreateWriter : TextWriter * leaveOpen:bool -> IPickleFormatWriter
        /// Initializes a new format reader
        abstract CreateReader : TextReader * leaveOpen:bool -> IPickleFormatReader