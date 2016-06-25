using System;
using System.Reflection;
using System.IO;
using System.Text;
using System.Runtime.Serialization;
using Microsoft.FSharp.Core;
using FSP = Nessos.FsPickler;

namespace Nessos.CsPickler
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
        /// <param name="omitHeader">omit FsPickler metadata at the serialization header; defaults to false.</param>
        public static BsonSerializer Create(bool omitHeader = false)
        {
            var omitHeaderOption = FSharpOption<bool>.Some(omitHeader);
            var bs = new FSP.Json.BsonSerializer(omitHeaderOption);
            return new BsonSerializer(bs);
        }
    }
}
