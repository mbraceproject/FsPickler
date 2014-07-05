using System;
using System.Reflection;
using System.IO;
using System.Text;
using System.Runtime.Serialization;

using FSP = Nessos.FsPickler;

namespace Nessos.CsPickler
{
    public class BinaryPickler : BasePickler
    {
        private FSP.BinaryPickler _bp ;
        
        private BinaryPickler(FSP.BinaryPickler pickler) : base(pickler) 
        {
            _bp = pickler;
        }

        public static BinaryPickler Create(bool forceLittleEndian = false)
        {
            var bp = new FSP.BinaryPickler();
            bp.ForceLittleEndian = forceLittleEndian;
            return new BinaryPickler(bp);
        }

        public bool ForceLittleEndian
        {
            get { return _bp.ForceLittleEndian;  }
            set { _bp.ForceLittleEndian = value; }
        }
    }
}
