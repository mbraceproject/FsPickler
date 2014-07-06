using System;
using System.Reflection;
using System.IO;
using System.Text;
using System.Runtime.Serialization;

using FSP = Nessos.FsPickler;

namespace Nessos.CsPickler
{
    /// <summary>
    ///     Xml FsPickler serializer
    /// </summary>
    public class XmlPickler : TextPickler
    {
        private FSP.XmlPickler _xp;

        private XmlPickler(FSP.XmlPickler pickler) : base(pickler) 
        {
            _xp = pickler;
        }

        /// <summary>
        ///     Creates a new Xml FsPickler instance.
        /// </summary>
        /// <param name="indent">indent xml serializations; defaults to false.</param>
        /// <returns>Xml FsPickler instance.</returns>
        public static XmlPickler Create(bool indent = false)
        {
            var xp = new FSP.XmlPickler();
            xp.Indent = indent;
            return new XmlPickler(xp);
        }

        /// <summary>
        ///     Gets or sets indentation of Xml serializations.
        /// </summary>
        public bool Indent
        {
            get { return _xp.Indent ; }
            set { _xp.Indent = value; }
        }
    }
}
