using System;
using System.Reflection;
using System.IO;
using System.Text;
using System.Runtime.Serialization;

using FSP = Nessos.FsPickler;

namespace Nessos.CsPickler
{
    public class BasePickler
    {
        private FSP.BasePickler _pickler;

        internal BasePickler (FSP.BasePickler pickler)
        {
            _pickler = pickler;
        }

        public void Serialize<T>(Stream stream, T value, Object streamingContext = null,
                                    Encoding encoding = null, bool leaveOpen = false)
        {
            var e = FSharpParams.GetEncoding(encoding);
            var sc = FSharpParams.GetStreamingContext(streamingContext);
            var lo = FSharpParams.GetLeaveOpen(leaveOpen);

            _pickler.Serialize<T>(stream, value, sc, e, lo);
        }

        public T Deserialize<T>(Stream stream, Object streamingContext = null,
                                        Encoding encoding = null, bool leaveOpen = false)
        {
            var e = FSharpParams.GetEncoding(encoding);
            var sc = FSharpParams.GetStreamingContext(streamingContext);
            var lo = FSharpParams.GetLeaveOpen(leaveOpen);

            return _pickler.Deserialize<T>(stream, sc, e, lo);
        }

        public byte [] Pickle<T>(T value, Object streamingContext = null, Encoding encoding = null) 
        {
            var e = FSharpParams.GetEncoding(encoding);
            var sc = FSharpParams.GetStreamingContext(streamingContext);

            return _pickler.Pickle<T>(value, sc, e);
        }

        public T UnPickle<T>(byte[] data, Object streamingContext = null, Encoding encoding = null)
        {
            var e = FSharpParams.GetEncoding(encoding);
            var sc = FSharpParams.GetStreamingContext(streamingContext);

            return _pickler.UnPickle<T>(data, sc, e);
        }
    }
}
