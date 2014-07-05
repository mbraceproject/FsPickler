using System;
using System.Reflection;
using System.IO;
using System.Text;
using System.Runtime.Serialization;

using FSP = Nessos.FsPickler;

namespace Nessos.CsPickler
{
    public class XmlPickler : TextPickler
    {
        private FSP.XmlPickler _xp;

        private XmlPickler(FSP.XmlPickler pickler) : base(pickler) 
        {
            _xp = pickler;
        }

        public static XmlPickler Create(bool indent = false)
        {
            var xp = new FSP.XmlPickler();
            xp.Indent = indent;
            return new XmlPickler(xp);
        }

        public bool Indent
        {
            get { return _xp.Indent ; }
            set { _xp.Indent = value; }
        }
    }
}
