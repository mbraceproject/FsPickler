using System;
using System.Runtime; 
using System.Runtime.Serialization;
using System.IO;
using System.Text;

namespace Nessos.FsPickler.Binary
{

    public class BinaryWriter : IDisposable
    {
        private Stream stream;

        private const int bufsize = 256;
        private int idx = 0;
        private byte[] buffer;

        public BinaryWriter(Stream output)
        {
            if (output == null)
                throw new ArgumentNullException("output");
            if (!output.CanWrite)
                throw new ArgumentException("BinaryWriter: stream is not writeable");

            stream = output;
            buffer = new byte[bufsize];
        }

        private void FlushBuffer()
        {
            if (idx == 0) return;

            stream.WriteByte((byte)(idx - 1));
            stream.Write(buffer, 0, idx);
            idx = 0;
        }

        private void FlushBufferIfNotEnoughSpace(int size)
        {
            if (idx + size <= bufsize) return;
            FlushBuffer();
        }

        private unsafe void BlockWrite(byte* src, int count)
        {
            FlushBufferIfNotEnoughSpace(1);

            fixed (byte* buf = buffer)
            {

                while (idx + count > bufsize)
                {
                    var copied = bufsize - idx;
                    Utils.Memcpy(buf + idx, src, copied);
                    idx = bufsize;
                    FlushBuffer();
                    src += copied;
                    count -= copied;
                }

                Utils.Memcpy(buf + idx, src, count);
                idx = idx + count;
            }
        }

        public void Write(Array array)
        {
            int count = Buffer.ByteLength(array);
            Write7BitEncodedLength(count);

            var j = 0;

            while (idx + count > bufsize)
            {
                var copied = bufsize - idx;
                Buffer.BlockCopy(array, j, buffer, idx, copied);
                idx = bufsize;
                FlushBuffer();
                j += copied;
                count -= copied;
            }

            Buffer.BlockCopy(array, j, buffer, idx, count);
            idx = idx + count;
        }

        private void Write7BitEncodedLength(int length)
        {
            uint value = (uint)length;
            // Write out an int 7 bits at a time.  The high bit of the byte,
            // when on, tells reader to continue reading more bytes.
            while (value >= 0x80)
            {
                var b = (byte)(value | 0x80);
                Write(b); Write(b);
                value >>= 7;
            }
            Write((byte)value);
            Write((byte)value);
        } 

        public void Write(bool value)
        {
            FlushBufferIfNotEnoughSpace(1);
            buffer[idx++] = (byte)(value ? 1 : 0);
        }

        public void Write(byte value)
        {
            FlushBufferIfNotEnoughSpace(1);
            buffer[idx++] = value;
        }

        public void Write(sbyte value)
        {
            FlushBufferIfNotEnoughSpace(1);
            buffer[idx++] = (byte)value;
        }

        public void Write(byte[] bytes)
        {
            if (Object.ReferenceEquals(bytes, null))
            {
                Write7BitEncodedLength(-1);
                return;
            }

            var length = bytes.Length;

            Write7BitEncodedLength(length);

            if (idx + length < bufsize)
            {
                Write(true);
                Buffer.BlockCopy(bytes, 0, buffer, idx, length);
                idx += length;
                return;
            }

            Write(false);
            FlushBuffer();
            stream.Write(bytes, 0, length);
        }

        public void Write(char ch)
        {
            FlushBufferIfNotEnoughSpace(2);
            var i = idx;
            buffer[i] = (byte)ch;
            buffer[i + 1] = (byte)(ch >> 8);
            idx = i + 2;
        }

        public unsafe void Write(double value)
        {
            FlushBufferIfNotEnoughSpace(8);
            var i = idx;
            ulong TmpValue = *(ulong*)&value;
            buffer[i] = (byte)TmpValue;
            buffer[i+1] = (byte)(TmpValue >> 8);
            buffer[i+2] = (byte)(TmpValue >> 16);
            buffer[i+3] = (byte)(TmpValue >> 24);
            buffer[i+4] = (byte)(TmpValue >> 32);
            buffer[i+5] = (byte)(TmpValue >> 40);
            buffer[i+6] = (byte)(TmpValue >> 48);
            buffer[i+7] = (byte)(TmpValue >> 56);
            idx = i + 8;
        }

        public unsafe void Write(decimal value)
        {
            FlushBufferIfNotEnoughSpace(16);
            var addr = (ulong*)&value;
            var i = idx;
            ulong v1 = *addr;
            ulong v2 = *(addr + 1);

            buffer[i] = (byte)v1;
            buffer[i + 1] = (byte)(v1 >> 8);
            buffer[i + 2] = (byte)(v1 >> 16);
            buffer[i + 3] = (byte)(v1 >> 24);
            buffer[i + 4] = (byte)(v1 >> 32);
            buffer[i + 5] = (byte)(v1 >> 40);
            buffer[i + 6] = (byte)(v1 >> 48);
            buffer[i + 7] = (byte)(v1 >> 56);
            
            buffer[i + 8] = (byte)v2;
            buffer[i + 9] = (byte)(v2 >> 8);
            buffer[i + 10] = (byte)(v2 >> 16);
            buffer[i + 11] = (byte)(v2 >> 24);
            buffer[i + 12] = (byte)(v2 >> 32);
            buffer[i + 13] = (byte)(v2 >> 40);
            buffer[i + 14] = (byte)(v2 >> 48);
            buffer[i + 15] = (byte)(v2 >> 56);

            idx = i + 16;
        }

        public unsafe void Write(Guid value)
        {
            FlushBufferIfNotEnoughSpace(16);
            var addr = (ulong*)&value;
            var i = idx;
            ulong v1 = *addr;
            ulong v2 = *(addr + 1);

            buffer[i] = (byte)v1;
            buffer[i + 1] = (byte)(v1 >> 8);
            buffer[i + 2] = (byte)(v1 >> 16);
            buffer[i + 3] = (byte)(v1 >> 24);
            buffer[i + 4] = (byte)(v1 >> 32);
            buffer[i + 5] = (byte)(v1 >> 40);
            buffer[i + 6] = (byte)(v1 >> 48);
            buffer[i + 7] = (byte)(v1 >> 56);

            buffer[i + 8] = (byte)v2;
            buffer[i + 9] = (byte)(v2 >> 8);
            buffer[i + 10] = (byte)(v2 >> 16);
            buffer[i + 11] = (byte)(v2 >> 24);
            buffer[i + 12] = (byte)(v2 >> 32);
            buffer[i + 13] = (byte)(v2 >> 40);
            buffer[i + 14] = (byte)(v2 >> 48);
            buffer[i + 15] = (byte)(v2 >> 56);

            idx = i + 16;
        }

        public void Write(short value)
        {
            FlushBufferIfNotEnoughSpace(2);
            var i = idx;
            buffer[i] = (byte)value;
            buffer[i+1] = (byte)(value >> 8);
            
            idx = i + 2;
        }

        public void Write(ushort value)
        {
            FlushBufferIfNotEnoughSpace(2);
            var i = idx;
            buffer[i] = (byte)value;
            buffer[i+1] = (byte)(value >> 8);
            idx = i + 2;
        } 

        public void Write(int value)
        {
            FlushBufferIfNotEnoughSpace(4);
            var i = idx;
            buffer[i] = (byte)value;
            buffer[i + 1] = (byte)(value >> 8);
            buffer[i + 2] = (byte)(value >> 16);
            buffer[i + 3] = (byte)(value >> 24);
            idx = i + 4;

        }

        public void Write(uint value)
        {
            FlushBufferIfNotEnoughSpace(4);
            var i = idx;
            buffer[i] = (byte)value;
            buffer[i+1] = (byte)(value >> 8);
            buffer[i+2] = (byte)(value >> 16);
            buffer[i+3] = (byte)(value >> 24);
            idx = i + 4;
        }

        public void Write(long value)
        {
            FlushBufferIfNotEnoughSpace(8);
            var temp = (ulong)value;
            var i = idx;
            buffer[i] = (byte)temp;
            buffer[i + 1] = (byte)(temp >> 8);
            buffer[i + 2] = (byte)(temp >> 16);
            buffer[i + 3] = (byte)(temp >> 24);
            buffer[i + 4] = (byte)(temp >> 32);
            buffer[i + 5] = (byte)(temp >> 40);
            buffer[i + 6] = (byte)(temp >> 48);
            buffer[i + 7] = (byte)(temp >> 56);
            idx = i + 8;
        }

        public void Write(ulong value)
        {
            FlushBufferIfNotEnoughSpace(8);
            var i = idx;
            buffer[i] = (byte)value;
            buffer[i + 1] = (byte)(value >> 8);
            buffer[i + 2] = (byte)(value >> 16);
            buffer[i + 3] = (byte)(value >> 24);
            buffer[i + 4] = (byte)(value >> 32);
            buffer[i + 5] = (byte)(value >> 40);
            buffer[i + 6] = (byte)(value >> 48);
            buffer[i + 7] = (byte)(value >> 56);
            idx = i + 8;
        }

        public unsafe void Write(float value)
        {
            FlushBufferIfNotEnoughSpace(4);
            var i = idx;
            uint TmpValue = *(uint*)&value;
            buffer[i] = (byte)TmpValue;
            buffer[i + 1] = (byte)(TmpValue >> 8);
            buffer[i + 2] = (byte)(TmpValue >> 16);
            buffer[i + 3] = (byte)(TmpValue >> 24);
            idx = i + 4;
        }

        public unsafe void Write(string value)
        {
            if (Object.ReferenceEquals(value,null))
            {
                Write7BitEncodedLength(-1);
                return;
            }

            var length = value.Length;

            Write7BitEncodedLength(length);

            if (length == 0) return;

            fixed (char* sp = value)
            {
                BlockWrite((byte*)sp, 2 * length);
            }
        }

        public void Dispose() { FlushBuffer(); stream.Flush(); }
        public void Flush() { FlushBuffer(); stream.Flush(); }
    }
}