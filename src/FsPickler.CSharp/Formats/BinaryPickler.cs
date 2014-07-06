using System;
using System.Reflection;
using System.IO;
using System.Text;
using System.Runtime.Serialization;

using FSP = Nessos.FsPickler;

namespace Nessos.CsPickler
{
    /// <summary>
    ///     Binary FsPickler serializer.
    /// </summary>
    public class BinaryPickler : CsPicklerBase
    {
        private FSP.BinaryPickler _bp ;
        
        private BinaryPickler(FSP.BinaryPickler pickler) : base(pickler) 
        {
            _bp = pickler;
        }

        /// <summary>
        ///     Creates a new Binary FsPickler instance.
        /// </summary>
        /// <param name="forceLittleEndian">force little-endian encoding in primitive arrays but is slower; defaults to false.</param>
        /// <returns>Binary Pickler.</returns>
        public static BinaryPickler Create(bool forceLittleEndian = false)
        {
            var bp = new FSP.BinaryPickler();
            bp.ForceLittleEndian = forceLittleEndian;
            return new BinaryPickler(bp);
        }

        /// <summary>
        ///     Gets or sets the ForceLittleEndian setting.
        ///     Uses BinaryWriter rather than Buffer.BlockCopy 
        ///     for array serializations but is slower.
        /// </summary>
        public bool ForceLittleEndian
        {
            get { return _bp.ForceLittleEndian;  }
            set { _bp.ForceLittleEndian = value; }
        }
    }
}
