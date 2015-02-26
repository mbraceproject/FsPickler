module internal Nessos.FsPickler.PrimitivePicklers

open System

open Microsoft.FSharp.Core.LanguagePrimitives

open Nessos.FsPickler
open Nessos.FsPickler.Reflection

[<AbstractClass>]
[<AutoSerializable(false)>]
type PrimitivePickler<'T> () =
    inherit Pickler<'T>()

    override p.ImplementationType = typeof<'T>
    override p.PicklerInfo = PicklerInfo.Primitive
    override p.IsCacheByRef = false
    override p.UseWithSubtypes = false

    override p.Cast<'S> () : Pickler<'S> = raise <| new NotSupportedException("Cannot cast primitive picklers.")

[<AutoSerializable(false)>]
type BooleanPickler () =
    inherit PrimitivePickler<bool> ()

    override __.Write (writer : WriteState) (tag : string) (b : bool) = writer.Formatter.WriteBoolean tag b
    override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadBoolean tag

[<AutoSerializable(false)>]
type BytePickler () =
    inherit PrimitivePickler<byte> ()

    override __.Write (writer : WriteState) (tag : string) (b : byte) = writer.Formatter.WriteByte tag b
    override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadByte tag

[<AutoSerializable(false)>]
type SBytePickler () =
    inherit PrimitivePickler<sbyte> ()

    override __.Write (writer : WriteState) (tag : string) (b : sbyte) = writer.Formatter.WriteSByte tag b
    override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadSByte tag

[<AutoSerializable(false)>]
type Int16Pickler () =
    inherit PrimitivePickler<int16> ()

    override __.Write (writer : WriteState) (tag : string) (n : int16) = writer.Formatter.WriteInt16 tag n
    override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadInt16 tag

[<AutoSerializable(false)>]
type Int32Pickler () =
    inherit PrimitivePickler<int32> ()

    override __.Write (writer : WriteState) (tag : string) (n : int32) = writer.Formatter.WriteInt32 tag n
    override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadInt32 tag

[<AutoSerializable(false)>]
type Int64Pickler () =
    inherit PrimitivePickler<int64> ()

    override __.Write (writer : WriteState) (tag : string) (n : int64) = writer.Formatter.WriteInt64 tag n
    override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadInt64 tag

[<AutoSerializable(false)>]
type UInt16Pickler () =
    inherit PrimitivePickler<uint16> ()

    override __.Write (writer : WriteState) (tag : string) (n : uint16) = writer.Formatter.WriteUInt16 tag n
    override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadUInt16 tag

[<AutoSerializable(false)>]
type UInt32Pickler () =
    inherit PrimitivePickler<uint32> ()

    override __.Write (writer : WriteState) (tag : string) (n : uint32) = writer.Formatter.WriteUInt32 tag n
    override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadUInt32 tag

[<AutoSerializable(false)>]
type UInt64Pickler () =
    inherit PrimitivePickler<uint64> ()

    override __.Write (writer : WriteState) (tag : string) (n : uint64) = writer.Formatter.WriteUInt64 tag n
    override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadUInt64 tag

[<AutoSerializable(false)>]
type SinglePickler () =
    inherit PrimitivePickler<single> ()

    override __.Write (writer : WriteState) (tag : string) (f : single) = writer.Formatter.WriteSingle tag f
    override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadSingle tag

[<AutoSerializable(false)>]
type DoublePickler () =
    inherit PrimitivePickler<double> ()

    override __.Write (writer : WriteState) (tag : string) (f : double) = writer.Formatter.WriteDouble tag f
    override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadDouble tag

[<AutoSerializable(false)>]
type DecimalPickler () =
    inherit PrimitivePickler<decimal> ()

    override __.Write (writer : WriteState) (tag : string) (d : decimal) = writer.Formatter.WriteDecimal tag d
    override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadDecimal tag

[<AutoSerializable(false)>]
type CharPickler () =
    inherit PrimitivePickler<char> ()

    override __.Write (writer : WriteState) (tag : string) (c : char) = writer.Formatter.WriteChar tag c
    override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadChar tag

[<AutoSerializable(false)>]
type StringPickler () =
    inherit PrimitivePickler<string> ()

    override __.Write (writer : WriteState) (tag : string) (s : string) = writer.Formatter.WriteString tag s
    override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadString tag

[<AutoSerializable(false)>]
type ByteArrayPickler () =
    inherit PrimitivePickler<byte []> ()

    override __.Write (writer : WriteState) (tag : string) (bytes : byte []) = writer.Formatter.WriteBytes tag bytes
    override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadBytes tag

[<AutoSerializable(false)>]
type GuidPickler () =
    inherit PrimitivePickler<Guid> ()

    override __.Write (writer : WriteState) (tag : string) (guid : Guid) =
        writer.Formatter.WriteGuid tag guid

    override __.Read (reader : ReadState) (tag : string) =
        reader.Formatter.ReadGuid tag

[<AutoSerializable(false)>]
type DateTimePickler () =
    inherit PrimitivePickler<DateTime> ()

    override __.Write (writer : WriteState) (tag : string) (date : DateTime) =
        writer.Formatter.WriteDate tag date

    override __.Read (reader : ReadState) (tag : string) =
        reader.Formatter.ReadDate tag

[<AutoSerializable(false)>]
type TimeSpanPickler () =
    inherit PrimitivePickler<TimeSpan> ()

    override __.Write (writer : WriteState) (tag : string) (date : TimeSpan) =
        writer.Formatter.WriteTimeSpan tag date

    override __.Read (reader : ReadState) (tag : string) =
        reader.Formatter.ReadTimeSpan tag

[<AutoSerializable(false)>]
type BigIntPickler () =
    inherit PrimitivePickler<bigint> ()

    override __.Write (writer : WriteState) (tag : string) (bint : bigint) =
        writer.Formatter.WriteBigInteger tag bint

    override __.Read (reader : ReadState) (tag : string) =
        reader.Formatter.ReadBigInteger tag

[<AutoSerializable(false)>]
type DBNullPickler () =
    inherit PrimitivePickler<DBNull> ()

    override __.Write (writer : WriteState) (tag : string) (_ : DBNull) =
        writer.Formatter.BeginWriteObject tag ObjectFlags.IsNull
        writer.Formatter.EndWriteObject ()

    override __.Read (reader : ReadState) (tag : string) =
        let _ = reader.Formatter.BeginReadObject tag
        reader.Formatter.EndReadObject ()
        DBNull.Value

[<AutoSerializable(false)>]
type private UnitPickler<'T> (value : 'T) =
    // UnitPickler generic due to a bug in F# compiler: cannot explicitly instantiate Pickler<unit>
    inherit PrimitivePickler<'T> ()

    override __.Write (writer : WriteState) (tag : string) (_ : 'T) =
        writer.Formatter.BeginWriteObject tag ObjectFlags.IsNull
        writer.Formatter.EndWriteObject ()

    override __.Read (reader : ReadState) (tag : string) : 'T =
        let _ = reader.Formatter.BeginReadObject tag
        reader.Formatter.EndReadObject ()
        value


module PrimitivePicklers =
        
    let mkBoolean () = new BooleanPickler () :> Pickler<bool>
    let mkByte () = new BytePickler () :> Pickler<byte>
    let mkSByte () = new SBytePickler () :> Pickler<sbyte>
    let mkInt16 () = new Int16Pickler () :> Pickler<int16>
    let mkInt32 () = new Int32Pickler () :> Pickler<int>
    let mkInt64 () = new Int64Pickler () :> Pickler<int64>
    let mkUInt16 () = new UInt16Pickler () :> Pickler<uint16>
    let mkUInt32 () = new UInt32Pickler () :> Pickler<uint32>
    let mkUInt64 () = new UInt64Pickler () :> Pickler<uint64>
    let mkSingle () = new SinglePickler () :> Pickler<single>
    let mkDouble () = new DoublePickler () :> Pickler<double>
    let mkDecimal () = new DecimalPickler () :> Pickler<decimal>
    let mkString () = new StringPickler () :> Pickler<string>
    let mkChar () = new CharPickler () :> Pickler<char>
    let mkBytes () = new ByteArrayPickler () :> Pickler<byte []>
    let mkGuid () = new GuidPickler () :> Pickler<Guid>
    let mkDate () = new DateTimePickler () :> Pickler<DateTime>
    let mkTimeSpan () = new TimeSpanPickler () :> Pickler<TimeSpan>
    let mkBigInt () = new BigIntPickler () :> Pickler<bigint>
    let mkDBNull () = new DBNullPickler () :> Pickler<DBNull>
    let mkUnit () = new UnitPickler<unit>(()) :> Pickler<unit>


    let mkAll () : Pickler [] =
        let inline uc (factory : unit -> Pickler<'T>) = factory () :> Pickler
        [|
            uc mkBoolean ; uc mkByte ; uc mkSByte ; uc mkInt16 ; uc mkInt32 ; uc mkInt64
            uc mkUInt16 ; uc mkUInt32 ; uc mkUInt64 ; uc mkSingle ; uc mkDouble ; uc mkDecimal
            uc mkString ; uc mkChar ; uc mkBytes ; uc mkGuid ; uc mkDate ; uc mkTimeSpan
            uc mkBigInt ; uc mkDBNull ; uc mkUnit
        |]