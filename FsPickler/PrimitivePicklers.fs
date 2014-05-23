module internal Nessos.FsPickler.PrimitivePicklers

    open Nessos.FsPickler
    open Nessos.FsPickler.Utils
    open Nessos.FsPickler.Header

    [<AbstractClass>]
    [<AutoSerializable(false)>]
    type PrimitivePickler<'T> (typeKind, isOfFixedSize) =
        inherit Pickler<'T>()

        let flags = computePicklerFlags PicklerInfo.Atomic TypeKind.Primitive false false false isOfFixedSize typeof<'T>

        override p.ImplementationType = typeof<'T>

        override p.IsCyclicType = false
        override p.TypeKind = typeKind
        override p.IsPrimitive = true
        override p.IsCacheByRef = false
        override p.IsOfFixedSize = isOfFixedSize
        override p.PicklerFlags = flags
        override p.UseWithSubtypes = false

        override p.UntypedWrite (state:WriteState) (tag:string) (value:obj) =
            state.NextWriteIsSubtype <- false
            p.Write state tag (fastUnbox value)

        override p.UntypedRead  (state:ReadState) (tag:string) =
            state.NextWriteIsSubtype <- false
            p.Read state tag :> obj

        override self.Cast<'S> () = self :> Pickler :?> Pickler<'S>
        override self.Clone () = self :> Pickler
        override self.InitializeFrom _ = raise <| System.NotSupportedException("Primitive pickler late initialization not supported.")

    [<AutoSerializable(false)>]
    type BooleanPickler () =
        inherit PrimitivePickler<bool> (TypeKind.Primitive, true)

        override __.Write (writer : WriteState) (tag : string) (b : bool) = writer.Formatter.WriteBoolean tag b
        override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadBoolean tag

    [<AutoSerializable(false)>]
    type BytePickler () =
        inherit PrimitivePickler<byte> (TypeKind.Primitive, true)

        override __.Write (writer : WriteState) (tag : string) (b : byte) = writer.Formatter.WriteByte tag b
        override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadByte tag

    [<AutoSerializable(false)>]
    type SBytePickler () =
        inherit PrimitivePickler<sbyte> (TypeKind.Primitive, true)

        override __.Write (writer : WriteState) (tag : string) (b : sbyte) = writer.Formatter.WriteSByte tag b
        override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadSByte tag

    [<AutoSerializable(false)>]
    type Int16Pickler () =
        inherit PrimitivePickler<int16> (TypeKind.Primitive, true)

        override __.Write (writer : WriteState) (tag : string) (n : int16) = writer.Formatter.WriteInt16 tag n
        override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadInt16 tag

    [<AutoSerializable(false)>]
    type Int32Pickler () =
        inherit PrimitivePickler<int32> (TypeKind.Primitive, true)

        override __.Write (writer : WriteState) (tag : string) (n : int32) = writer.Formatter.WriteInt32 tag n
        override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadInt32 tag

    [<AutoSerializable(false)>]
    type Int64Pickler () =
        inherit PrimitivePickler<int64> (TypeKind.Primitive, true)

        override __.Write (writer : WriteState) (tag : string) (n : int64) = writer.Formatter.WriteInt64 tag n
        override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadInt64 tag

    [<AutoSerializable(false)>]
    type UInt16Pickler () =
        inherit PrimitivePickler<uint16> (TypeKind.Primitive, true)

        override __.Write (writer : WriteState) (tag : string) (n : uint16) = writer.Formatter.WriteUInt16 tag n
        override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadUInt16 tag

    [<AutoSerializable(false)>]
    type UInt32Pickler () =
        inherit PrimitivePickler<uint32> (TypeKind.Primitive, true)

        override __.Write (writer : WriteState) (tag : string) (n : uint32) = writer.Formatter.WriteUInt32 tag n
        override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadUInt32 tag

    [<AutoSerializable(false)>]
    type UInt64Pickler () =
        inherit PrimitivePickler<uint64> (TypeKind.Primitive, true)

        override __.Write (writer : WriteState) (tag : string) (n : uint64) = writer.Formatter.WriteUInt64 tag n
        override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadUInt64 tag

    [<AutoSerializable(false)>]
    type SinglePickler () =
        inherit PrimitivePickler<single> (TypeKind.Primitive, true)

        override __.Write (writer : WriteState) (tag : string) (f : single) = writer.Formatter.WriteSingle tag f
        override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadSingle tag

    [<AutoSerializable(false)>]
    type DoublePickler () =
        inherit PrimitivePickler<double> (TypeKind.Primitive, true)

        override __.Write (writer : WriteState) (tag : string) (f : double) = writer.Formatter.WriteDouble tag f
        override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadDouble tag

    [<AutoSerializable(false)>]
    type DecimalPickler () =
        inherit PrimitivePickler<decimal> (TypeKind.Primitive, true)

        override __.Write (writer : WriteState) (tag : string) (d : decimal) = writer.Formatter.WriteDecimal tag d
        override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadDecimal tag

    [<AutoSerializable(false)>]
    type CharPickler () =
        inherit PrimitivePickler<char> (TypeKind.Primitive, true)

        override __.Write (writer : WriteState) (tag : string) (c : char) = writer.Formatter.WriteChar tag c
        override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadChar tag

    [<AutoSerializable(false)>]
    type StringPickler () =
        inherit PrimitivePickler<string> (TypeKind.String, false)

        override __.IsOfFixedSize = false
        override __.TypeKind = TypeKind.String

        override __.Write (writer : WriteState) (tag : string) (s : string) = writer.Formatter.WriteString tag s
        override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadString tag

//    [<AutoSerializable(false)>]
//    type ByteArrayBuilder () =
//        inherit PrimitivePickler<byte []> ()
//
//        override __.IsOfFixedSize = false
//
//        override __.Write (writer : WriteState) (tag : string) (bytes : byte []) = writer.Formatter.WriteBytes tag bytes
//        override __.Read (reader : ReadState) (tag : string) = reader.Formatter.ReadBytes tag

    type PrimitivePicklers =
        static member GetBooleanPickler () = new BooleanPickler() :> Pickler<bool>


    let mkPrimitivePicklers () : Pickler [] =
        [|
            new BooleanPickler () :> Pickler ; new BytePickler () :> Pickler ; new SBytePickler () :> Pickler
            new Int16Pickler () :> Pickler ; new Int32Pickler () :> Pickler ; new Int64Pickler () :> Pickler
            new UInt16Pickler () :> Pickler ; new UInt32Pickler () :> Pickler ; new UInt64Pickler () :> Pickler
            new SinglePickler () :> Pickler ; new DoublePickler () :> Pickler ; new DecimalPickler () :> Pickler
            new StringPickler () :> Pickler ; new CharPickler () :> Pickler
        |]