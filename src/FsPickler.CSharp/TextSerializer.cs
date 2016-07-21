using System;
using System.Reflection;
using System.IO;
using System.Text;
using System.Runtime.Serialization;

using FSP = MBrace.FsPickler;

namespace MBrace.CsPickler
{
    /// <summary>
    ///     Provides basic functionality for text-based serialization.
    /// </summary>
    public abstract class CsPicklerTextSerializer : CsPicklerSerializer
    {
        FSP.FsPicklerTextSerializer _textSerializer;

        /// <summary>
        ///     Wraps an FsPickler instance in a CsPickler facade.
        /// </summary>
        /// <param name="textSerializer">FsPickler instance.</param>
        public CsPicklerTextSerializer(FSP.FsPicklerTextSerializer textSerializer) : base(textSerializer)
        {
            _textSerializer = textSerializer;
        }

        /// <summary>
        ///     Serializes given value to stream.
        /// </summary>
        /// <typeparam name="T">serialized value type.</typeparam>
        /// <param name="writer">target text writer.</param>
        /// <param name="value">serialized value.</param>
        /// <param name="streamingContext">payload object for StreamingContext; defaults to null.</param>
        /// <param name="leaveOpen">leave stream open; defaults to false.</param>
        public void Serialize<T>(TextWriter writer, T value, Object streamingContext = null, bool leaveOpen = false)
        {
            var sc = Utils.GetStreamingContext(streamingContext);
            _textSerializer.Serialize<T>(writer, value, streamingContext: sc.ToOption(), leaveOpen: leaveOpen.ToOption());
        }

        /// <summary>
        ///     Deserializes given type from stream.
        /// </summary>
        /// <typeparam name="T">deserialized value type.</typeparam>
        /// <param name="reader">source text reader.</param>
        /// <param name="streamingContext">payload object for StreamingContext; defaults to null.</param>
        /// <param name="leaveOpen">leave stream open; defaults to false.</param>
        /// <returns>deserialized value.</returns>
        public T Deserialize<T>(TextReader reader, Object streamingContext = null, bool leaveOpen = false)
        {
            var sc = Utils.GetStreamingContext(streamingContext);
            return _textSerializer.Deserialize<T>(reader, streamingContext: sc.ToOption(), leaveOpen: leaveOpen.ToOption());
        }


        /// <summary>
        ///     Creates a string pickle out of a given value.
        /// </summary>
        /// <typeparam name="T">serialized value type.</typeparam>
        /// <param name="value">serialized value.</param>
        /// <param name="streamingContext">payload object for StreamingContext; defaults to null.</param>
        /// <returns>binary pickle for object.</returns>
        public string PickleToString<T>(T value, Object streamingContext = null)
        {
            var sc = Utils.GetStreamingContext(streamingContext);

            return _textSerializer.PickleToString<T>(value, streamingContext: sc.ToOption());
        }

        /// <summary>
        ///     Instantiates a new object out of a string pickle.
        /// </summary>
        /// <typeparam name="T">type of value to be unpickled.</typeparam>
        /// <param name="pickle">text pickle of value.</param>
        /// <param name="streamingContext">payload object for StreamingContext; defaults to null.</param>
        /// <returns>unpickled instance.</returns>
        public T UnPickleOfString<T>(string pickle, Object streamingContext = null)
        {
            var sc = Utils.GetStreamingContext(streamingContext);

            return _textSerializer.UnPickleOfString<T>(pickle, streamingContext: sc.ToOption());
        }
    }
}
