using System;
using System.Reflection;
using System.IO;
using System.Text;
using System.Runtime.Serialization;

using FSP = Nessos.FsPickler;

namespace Nessos.CsPickler
{
    /// <summary>
    ///     Defines a Bson serializer instance.
    /// </summary>
    public class BsonPickler : CsPicklerBase
    {
        FSP.Json.BsonPickler _bp;

        private BsonPickler(FSP.Json.BsonPickler bp)
            : base(bp)
        {
            _bp = bp;
        }

        /// <summary>
        ///     Creates a new BsonPickler instance.
        /// </summary>
        public static BsonPickler Create()
        {
            var bp = new FSP.Json.BsonPickler();
            return new BsonPickler(bp);
        }
    }
}
