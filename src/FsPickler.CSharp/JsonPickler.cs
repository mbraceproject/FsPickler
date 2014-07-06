using System;
using System.Reflection;
using System.IO;
using System.Text;
using System.Runtime.Serialization;

using FSP = Nessos.FsPickler;

namespace Nessos.CsPickler
{
    /// <summary>
    ///     Json FsPickler serializer.
    /// </summary>
    public class JsonPickler : TextPickler
    {
        FSP.Json.JsonPickler _jp;

        private JsonPickler(FSP.Json.JsonPickler jp) : base(jp)
        {
            _jp = jp;
        }

        /// <summary>
        ///     Creates a new Json FsPickler instance.
        /// </summary>
        /// <param name="indent">Indent Json serialization.</param>
        /// <param name="omitHeader">Omit FsPickler metadata at the serialization header.</param>
        /// <returns>a JsonPickler instance.</returns>
        public static JsonPickler Create(bool indent = false, bool omitHeader = false)
        {
            var jp = new FSP.Json.JsonPickler();
            jp.Indent = indent;
            jp.OmitHeader = omitHeader;
            return new JsonPickler(jp);
        }

        /// <summary>
        ///     Gets or sets Json serialization indentation.
        /// </summary>
        public bool Indent
        {
            get { return _jp.Indent ; }
            set { _jp.Indent = value; }
        }

        /// <summary>
        ///     Gets or sets whether FsPickler metadata should be read at the Json header.
        /// </summary>
        public bool OmitHeader
        {
            get { return _jp.OmitHeader ; }
            set { _jp.OmitHeader = value; }
        }
    }
}
