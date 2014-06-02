using System;
using System.Reflection;
using System.IO;

namespace Nessos.FsPickler.Binary
{
    internal class Utils {

        private readonly static bool is64BitProcess = IntPtr.Size == 8;

        // implementation taken from BCL System.Buffer
        public unsafe static void Memcpy(byte* dest, byte* src, int len) { 
            System.Diagnostics.Debug.Assert(len > 0);

            var is64Bit = is64BitProcess;

            if (len >= 16)
            { 
                do
                {
                    if (is64Bit) 
                    {
                        ((long*)dest)[0] = ((long*)src)[0]; 
                        ((long*)dest)[1] = ((long*)src)[1];
                    } 
                    else 
                    {
                        ((int*)dest)[0] = ((int*)src)[0]; 
                        ((int*)dest)[1] = ((int*)src)[1];
                        ((int*)dest)[2] = ((int*)src)[2]; 
                        ((int*)dest)[3] = ((int*)src)[3];
                    }

                    dest += 16;
                    src += 16; 
                } while ((len -= 16) >= 16);
            } 
            if(len > 0)  // protection against negative len and optimization for len==16*N 
            {
                if ((len & 8) != 0) 
                {
                    if (is64Bit) 
                        ((long*)dest)[0] = ((long*)src)[0];
                    else 
                    {
                        ((int*)dest)[0] = ((int*)src)[0];
                        ((int*)dest)[1] = ((int*)src)[1]; 
                    }

                    dest += 8;
                    src += 8; 
                }
                if ((len & 4) != 0)
                {
                    ((int*)dest)[0] = ((int*)src)[0]; 
                    dest += 4;
                    src += 4; 
                } 
                if ((len & 2) != 0)
                { 
                    ((short*)dest)[0] = ((short*)src)[0];
                    dest += 2;
                    src += 2;
                } 
                if ((len & 1) != 0)
                    *dest++ = *src++; 
            } 

        } 
    }
}