using System;
using System.Reflection;
using System.IO;
using System.Text;
using System.Runtime.Serialization;

using FSP = MBrace.FsPickler;

namespace MBrace.CsPickler
{
    /// <summary>
    ///     Defines a Json serializer instance.
    /// </summary>
    public class JsonSerializer : CsPicklerTextSerializer
    {
        FSP.Json.JsonSerializer _js;

        private JsonSerializer(FSP.Json.JsonSerializer js)
            : base(js)
        {
            _js = js;
        }

        /// <summary>
        ///     Creates a new JsonPickler instance.
        /// </summary>
        /// <param name="indent">indent Json serializations; defaults to false.</param>
        /// <param name="omitHeader">omit FsPickler metadata at the serialization header; defaults to false.</param>
        public static JsonSerializer Create(bool indent = false, bool omitHeader = false)
        {
            var js = new FSP.Json.JsonSerializer();
            js.Indent = indent;
            js.OmitHeader = omitHeader;
            return new JsonSerializer(js);
        }

        /// <summary>
        ///     Gets or sets Json serialization indentation.
        /// </summary>
        public bool Indent
        {
            get { return _js.Indent ; }
            set { _js.Indent = value; }
        }

        /// <summary>
        ///     Gets or sets whether FsPickler metadata should be read at the Json header.
        /// </summary>
        public bool OmitHeader
        {
            get { return _js.OmitHeader ; }
            set { _js.OmitHeader = value; }
        }
    }
}
