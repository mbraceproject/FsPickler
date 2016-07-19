using System;
using System.Reflection;
using System.IO;
using System.Text;
using System.Runtime.Serialization;

using FSP = Nessos.FsPickler;

namespace Nessos.CsPickler
{
    /// <summary>
    ///     Provides basic serialization functionality.
    /// </summary>
    public abstract class CsPicklerSerializer
    {
        private FSP.FsPicklerSerializer _serializer;


        /// <summary>
        /// Declares that dynamic subtype resolution should be disabled during serialization.
        /// This explicitly prohibits serialization/deserialization of any objects whose type
        /// is specified in the serialization payload. Examples of such types are System.Object,
        /// F# functions and delegates. Defaults to false.
        /// </summary>
        public bool DisableSubtypeResolution
        {
            get { return _serializer.DisableSubtypeResolution; }
            set { _serializer.DisableSubtypeResolution = value; }
        }

        /// Declares that FsPickler should make no attempt of its own to load Assemblies
        /// that are specified in the serialization format. Will result in a deserialization
        /// exception if required assembly is missing from the current AppDomain. Defaults to false.
        public bool DisableAssemblyLoading
        {
            get { return _serializer.DisableAssemblyLoading; }
            set { _serializer.DisableAssemblyLoading = value; }
        }

        /// <summary>
        ///     Wraps an FsPickler instance in a C# friendly facade.
        /// </summary>
        /// <param name="serializer">FsPickler serializer instance.</param>
        public CsPicklerSerializer(FSP.FsPicklerSerializer serializer)
        {
            _serializer = serializer;
        }

        /// <summary>
        ///     Serializes given value to stream.
        /// </summary>
        /// <typeparam name="T">serialized value type.</typeparam>
        /// <param name="stream">target stream.</param>
        /// <param name="value">serialized value.</param>
        /// <param name="streamingContext">payload object for StreamingContext; defaults to null.</param>
        /// <param name="encoding">stream encoding; defaults to UTF8.</param>
        /// <param name="leaveOpen">leave stream open; defaults to false.</param>
        public void Serialize<T>(Stream stream, T value, Object streamingContext = null,
                                    Encoding encoding = null, bool leaveOpen = false)
        {
            var sc = Utils.GetStreamingContext(streamingContext);
            _serializer.Serialize<T>(stream, value, streamingContext:sc.ToOption(), 
                                        encoding:encoding.ToOption(), leaveOpen:leaveOpen.ToOption());
        }

        /// <summary>
        ///     Deserializes given type from stream.
        /// </summary>
        /// <typeparam name="T">deserialized value type.</typeparam>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">payload object for StreamingContext; defaults to null.</param>
        /// <param name="encoding">stream encoding; defaults to UTF8.</param>
        /// <param name="leaveOpen">leave stream open; defaults to false.</param>
        /// <returns>deserialized value.</returns>
        public T Deserialize<T>(Stream stream, Object streamingContext = null,
                                        Encoding encoding = null, bool leaveOpen = false)
        {
            var sc = Utils.GetStreamingContext(streamingContext);
            return _serializer.Deserialize<T>(stream, streamingContext: sc.ToOption(), 
                                                encoding: encoding.ToOption(), leaveOpen: leaveOpen.ToOption());
        }

        /// <summary>
        ///     Creates a binary pickle out of a given value.
        /// </summary>
        /// <typeparam name="T">serialized value type.</typeparam>
        /// <param name="value">serialized value.</param>
        /// <param name="streamingContext">payload object for StreamingContext; defaults to null.</param>
        /// <param name="encoding">stream encoding; defaults to UTF8.</param>
        /// <returns>binary pickle for object.</returns>
        public byte [] Pickle<T>(T value, Object streamingContext = null, Encoding encoding = null) 
        {
            var sc = Utils.GetStreamingContext(streamingContext);
            return _serializer.Pickle<T>(value, streamingContext: sc.ToOption(), encoding: encoding.ToOption());
        }

        /// <summary>
        ///     Instantiates a new object out of a binary pickle.
        /// </summary>
        /// <typeparam name="T">type of value to be unpickled.</typeparam>
        /// <param name="pickle">binary pickle of value.</param>
        /// <param name="streamingContext">payload object for StreamingContext; defaults to null.</param>
        /// <param name="encoding">stream encoding; defaults to UTF8.</param>
        /// <returns>unpickled instance.</returns>
        public T UnPickle<T>(byte[] pickle, Object streamingContext = null, Encoding encoding = null)
        {
            var sc = Utils.GetStreamingContext(streamingContext);
            return _serializer.UnPickle<T>(pickle, streamingContext: sc.ToOption(), encoding: encoding.ToOption());
        }
    }
}
