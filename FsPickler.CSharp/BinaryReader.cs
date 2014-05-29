namespace Nessos.FsPickler.Binary
{

    using System;
    using System.IO;
    using System.Runtime;
    using System.Text;
    using System.Globalization;
    using System.Diagnostics;
    using System.Security;

    public class BinaryReader : IDisposable
    {
        private const int bufSize = 256;

        private byte[] buffer;
        private int bufferedDataSize = 0;
        private int idx = 0;
        private Stream stream;

        public BinaryReader(Stream InputStream)
        {
            buffer = new byte[bufSize];
            stream = InputStream;
        }

        private bool TryFillBuffer()
        {
            if (idx < bufferedDataSize) return false;

            var bytesRead = 0;
            var bytesToRead = 1 + stream.ReadByte();
            
            // stream returned -1, throw EndOfStream
            if (bytesToRead <= 0) throw new EndOfStreamException();

            do
            {
                var read = stream.Read(buffer, bytesRead, bytesToRead);
                bytesRead += read;
                bytesToRead -= read;
            } while (bytesToRead > 0);

            bufferedDataSize = bytesRead;
            idx = 0;
            return true;
        }

        //private void TryFillBuffer()
        //{
        //    if (idx == bufferedDataSize) TryFillBuffer();
        //}

        private unsafe void BlockRead(byte* dest, int size)
        {
            TryFillBuffer();

            fixed (byte* bp = buffer)
            {

                while (idx + size > bufferedDataSize)
                {
                    var count = bufferedDataSize - idx;
                    Utils.Memcpy(dest, bp + idx, count);
                    idx = bufferedDataSize;
                    dest += count;
                    size -= count;
                    TryFillBuffer();
                }

                Utils.Memcpy(dest, bp + idx, size);
                idx = idx + size;
            }
        }

        public void ReadArray(Array array)
        {
            var count = (int)Read7BitEncodedLength();
            if (count > Buffer.ByteLength(array))
                throw new InvalidOperationException("incoming length exceeds array size.");

            TryFillBuffer();

            var j = 0;

            while (idx + count > bufferedDataSize)
            {
                var copied = bufferedDataSize - idx;
                Buffer.BlockCopy(buffer, idx, array, j, copied);
                idx = bufferedDataSize;
                j += copied;
                count -= copied;
                TryFillBuffer();
            }

            Buffer.BlockCopy(buffer, idx, array, j, count);
            idx = idx + count;
        }

        // adapted from Microsoft's BCL source code
        private int Read7BitEncodedLength()
        {
            // Read out an Int32 7 bits at a time.  The high bit
            // of the byte when on means to continue reading more bytes. 
            uint count = 0;
            int shift = 0;
            byte b;
            do
            {
                if (shift == 5 * 7)  // 5 bytes max per Int32, shift += 7 
                    throw new FormatException("invalid 7-bit integer format.");

                b = ReadByte();
                count |= ((uint)(0x7F & b)) << shift;
                shift += 7;
            } while ((b & 0x80) != 0);
            
            return (int)count;
        }

        public byte ReadByte()
        {
            TryFillBuffer();
            return buffer[idx++];
        }

        public bool ReadBoolean()
        {
            TryFillBuffer();
            return (buffer[idx++] != 0);
        }

        public sbyte ReadSByte()
        {
            TryFillBuffer();
            return (sbyte)(buffer[idx++]);
        }

        public char ReadChar()
        {
            TryFillBuffer();
            var i = idx;
            var value = (char)(buffer[i] | buffer[i + 1] << 8);
            idx = i + 2;
            return value;
        }

        public short ReadInt16()
        {
            TryFillBuffer();
            var i = idx;
            var value = (short)(buffer[i] | buffer[i + 1] << 8);
            idx = i + 2;
            return value;
        }

        public ushort ReadUInt16()
        {
            TryFillBuffer();
            var i = idx;
            var value = (ushort)(buffer[i] | buffer[i + 1] << 8);
            idx = i + 2;
            return value;
        }

        public int ReadInt32()
        {
            TryFillBuffer();
            var i = idx;
            var value = (int)(buffer[i] | buffer[i + 1] << 8 | buffer[i + 2] << 16 | buffer[i + 3] << 24);
            idx = i + 4;
            return value;
        }

        public uint ReadUInt32()
        {
            TryFillBuffer();
            var i = idx;
            var value = (uint)(buffer[i] | buffer[i + 1] << 8 | buffer[i + 2] << 16 | buffer[i + 3] << 24);
            idx = i + 4;
            return value;
        }

        public long ReadInt64()
        {
            TryFillBuffer();
            var i = idx;
            var value =
                (ulong)buffer[i] |
                (ulong)buffer[i + 1] << 8 |
                (ulong)buffer[i + 2] << 16 |
                (ulong)buffer[i + 3] << 24 |
                (ulong)buffer[i + 4] << 32 |
                (ulong)buffer[i + 5] << 40 |
                (ulong)buffer[i + 6] << 48 |
                (ulong)buffer[i + 7] << 56;

            idx = i + 8;
            return (long)value;
        }

        public ulong ReadUInt64()
        {
            TryFillBuffer();
            var i = idx;
            var value =
                (ulong)buffer[i] |
                (ulong)buffer[i + 1] << 8 |
                (ulong)buffer[i + 2] << 16 |
                (ulong)buffer[i + 3] << 24 |
                (ulong)buffer[i + 4] << 32 |
                (ulong)buffer[i + 5] << 40 |
                (ulong)buffer[i + 6] << 48 |
                (ulong)buffer[i + 7] << 56;

            idx = i + 8;
            return value;
        }

        public unsafe float ReadSingle()
        {
            TryFillBuffer();
            var i = idx;
            uint tmpBuffer = (uint)(buffer[i] | buffer[i + 1] << 8 | buffer[i + 2] << 16 | buffer[i + 3] << 24);
            idx = i + 4;
            return *((float*)&tmpBuffer);
        }

        public unsafe double ReadDouble()
        {
            TryFillBuffer();
            var i = idx;
            var tmpBuffer =
                (ulong)buffer[i] |
                (ulong)buffer[i + 1] << 8 |
                (ulong)buffer[i + 2] << 16 |
                (ulong)buffer[i + 3] << 24 |
                (ulong)buffer[i + 4] << 32 |
                (ulong)buffer[i + 5] << 40 |
                (ulong)buffer[i + 6] << 48 |
                (ulong)buffer[i + 7] << 56;

            idx = i + 8;

            return *((double*)&tmpBuffer);
        }

        public unsafe decimal ReadDecimal()
        {
            TryFillBuffer();
            decimal value = 0;
            var i = idx;

            *(ulong*)&value =
                (ulong)buffer[i] |
                (ulong)buffer[i + 1] << 8 |
                (ulong)buffer[i + 2] << 16 |
                (ulong)buffer[i + 3] << 24 |
                (ulong)buffer[i + 4] << 32 |
                (ulong)buffer[i + 5] << 40 |
                (ulong)buffer[i + 6] << 48 |
                (ulong)buffer[i + 7] << 56;

            *(1 + (ulong*)&value) =
                (ulong)buffer[i + 8] |
                (ulong)buffer[i + 9] << 8 |
                (ulong)buffer[i + 10] << 16 |
                (ulong)buffer[i + 11] << 24 |
                (ulong)buffer[i + 12] << 32 |
                (ulong)buffer[i + 13] << 40 |
                (ulong)buffer[i + 14] << 48 |
                (ulong)buffer[i + 15] << 56;

            idx = i + 16;

            return value;
        }

        public unsafe Guid ReadGuid()
        {
            TryFillBuffer();
            Guid value;
            var i = idx;

            *(ulong*)&value =
                (ulong)buffer[i] |
                (ulong)buffer[i + 1] << 8 |
                (ulong)buffer[i + 2] << 16 |
                (ulong)buffer[i + 3] << 24 |
                (ulong)buffer[i + 4] << 32 |
                (ulong)buffer[i + 5] << 40 |
                (ulong)buffer[i + 6] << 48 |
                (ulong)buffer[i + 7] << 56;

            *(1 + (ulong*)&value) =
                (ulong)buffer[i + 8] |
                (ulong)buffer[i + 9] << 8 |
                (ulong)buffer[i + 10] << 16 |
                (ulong)buffer[i + 11] << 24 |
                (ulong)buffer[i + 12] << 32 |
                (ulong)buffer[i + 13] << 40 |
                (ulong)buffer[i + 14] << 48 |
                (ulong)buffer[i + 15] << 56;

            idx = i + 16;

            return value;
        }

        public byte[] ReadBytes()
        {
            var length = Read7BitEncodedLength();

            if (length < 0) return null;

            TryFillBuffer();

            var bytes = new byte[length];

            if (ReadBoolean()) // byte array contained in current buffer
            {
                Debug.Assert(idx + length <= bufferedDataSize);
                Buffer.BlockCopy(buffer, idx, bytes, 0, length);
                idx += length;
                return bytes;
            }

            Debug.Assert(idx == bufferedDataSize);

            var readBytes = 0;
            while (length > 0)
            {
                var read = stream.Read(bytes, readBytes, length);
                readBytes += read;
                length -= read;
            }

            return bytes;
        }

        public unsafe string ReadString()
        {
            var length = Read7BitEncodedLength();

            if (length < 0) return null;
            if (length == 0) return String.Empty;

            var value = new String(' ', length);
            fixed (char* cp = value)
            {
                BlockRead((byte*)cp, 2 * length);
            }

            return value;
        }

        public void Dispose() { }

    }
}