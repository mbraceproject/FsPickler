using System;
using System.Reflection;
using System.IO;
using System.Text;
using System.Runtime.Serialization;

using FSP = Nessos.FsPickler;

namespace Nessos.CsPickler
{
    public class TextPickler : BasePickler
    {
        FSP.TextPickler _textPickler;

        internal TextPickler (FSP.TextPickler textPickler) : base(textPickler)
        {
            _textPickler = textPickler;
        }

        public void Serialize<T>(TextWriter writer, T value, Object streamingContext = null, bool leaveOpen = false)
        {
            var sc = FSharpParams.GetStreamingContext(streamingContext);
            var lo = FSharpParams.GetLeaveOpen(leaveOpen);

            _textPickler.Serialize<T>(writer, value, sc, lo);
        }

        public T Deserialize<T>(TextReader reader, Object streamingContext = null, bool leaveOpen = false)
        {
            var sc = FSharpParams.GetStreamingContext(streamingContext);
            var lo = FSharpParams.GetLeaveOpen(leaveOpen);

            return _textPickler.Deserialize<T>(reader, sc, lo);
        }

        public string PickleToString<T>(T value, Object streamingContext = null)
        {
            var sc = FSharpParams.GetStreamingContext(streamingContext);

            return _textPickler.PickleToString<T>(value, sc);
        }

        public T UnPickleOfString<T>(string pickle, Object streamingContext = null)
        {
            var sc = FSharpParams.GetStreamingContext(streamingContext);

            return _textPickler.UnPickleOfString<T>(pickle, sc);
        }
    }
}
