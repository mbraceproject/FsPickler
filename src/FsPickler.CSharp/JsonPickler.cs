using System;
using System.Reflection;
using System.IO;
using System.Text;
using System.Runtime.Serialization;

using FSP = Nessos.FsPickler;

namespace Nessos.CsPickler
{
    public class JsonPickler : TextPickler
    {
        FSP.Json.JsonPickler _jp;

        private JsonPickler(FSP.Json.JsonPickler jp) : base(jp)
        {
            _jp = jp;
        }

        public static JsonPickler Create(bool indent = false, bool omitHeader = false)
        {
            var jp = new FSP.Json.JsonPickler();
            jp.Indent = indent;
            jp.OmitHeader = omitHeader;
            return new JsonPickler(jp);
        }

        public bool Indent
        {
            get { return _jp.Indent ; }
            set { _jp.Indent = value; }
        }

        public bool OmitHeader
        {
            get { return _jp.OmitHeader ; }
            set { _jp.OmitHeader = value; }
        }
    }
}
