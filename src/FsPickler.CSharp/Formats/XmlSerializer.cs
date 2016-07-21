using System;
using System.Reflection;
using System.IO;
using System.Text;
using System.Runtime.Serialization;

using FSP = MBrace.FsPickler;

namespace MBrace.CsPickler
{
    /// <summary>
    ///     Defines an Xml serializer instance.
    /// </summary>
    public class XmlSerializer : CsPicklerTextSerializer
    {
        private FSP.XmlSerializer _xs;

        private XmlSerializer(FSP.XmlSerializer xs)
            : base(xs) 
        {
            _xs = xs;
        }

        /// <summary>
        ///     Creates a new XmlPickler instance.
        /// </summary>
        /// <param name="indent">indent xml serializations; defaults to false.</param>
        public static XmlSerializer Create(bool indent = false)
        {
            var xs = new FSP.XmlSerializer();
            xs.Indent = indent;
            return new XmlSerializer(xs);
        }

        /// <summary>
        ///     Gets or sets indentation of Xml serializations.
        /// </summary>
        public bool Indent
        {
            get { return _xs.Indent ; }
            set { _xs.Indent = value; }
        }
    }
}
