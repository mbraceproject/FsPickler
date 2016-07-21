using System;
using System.Reflection;
using System.IO;
using System.Text;
using System.Runtime.Serialization;

using FSP = MBrace.FsPickler;

namespace MBrace.CsPickler
{
    /// <summary>
    ///     Defines a binary serializer instance.
    /// </summary>
    public class BinarySerializer : CsPicklerSerializer
    {
        private FSP.BinarySerializer _bs ;

        private BinarySerializer(FSP.BinarySerializer serializer)
            : base(serializer) 
        {
            _bs = serializer;
        }

        /// <summary>
        ///     Creates a new BinaryPickler instance.
        /// </summary>
        /// <param name="forceLittleEndian">force little-endian encoding in primitive arrays but is slower; defaults to false.</param>
        public static BinarySerializer Create(bool forceLittleEndian = false)
        {
            var bs = new FSP.BinarySerializer();
            bs.ForceLittleEndian = forceLittleEndian;
            return new BinarySerializer(bs);
        }

        /// <summary>
        ///     Gets or sets the ForceLittleEndian setting.
        ///     Uses BinaryWriter rather than Buffer.BlockCopy 
        ///     for array serializations but is slower.
        /// </summary>
        public bool ForceLittleEndian
        {
            get { return _bs.ForceLittleEndian;  }
            set { _bs.ForceLittleEndian = value; }
        }
    }
}
