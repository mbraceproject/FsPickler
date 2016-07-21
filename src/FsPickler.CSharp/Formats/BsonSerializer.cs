using System;
using System.Reflection;
using System.IO;
using System.Text;
using System.Runtime.Serialization;

using FSP = MBrace.FsPickler;

namespace MBrace.CsPickler
{
    /// <summary>
    ///     Defines a Bson serializer instance.
    /// </summary>
    public class BsonSerializer : CsPicklerSerializer
    {
        FSP.Json.BsonSerializer _bs;

        private BsonSerializer(FSP.Json.BsonSerializer bs)
            : base(bs)
        {
            _bs = bs;
        }

        /// <summary>
        ///     Creates a new BsonPickler instance.
        /// </summary>
        public static BsonSerializer Create()
        {
            var bs = new FSP.Json.BsonSerializer();
            return new BsonSerializer(bs);
        }
    }
}
