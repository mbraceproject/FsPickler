using System;
using System.Reflection;
using System.IO;
using System.Text;

using FSP = Nessos.FsPickler.FsPickler;
using IObjectVisitor = Nessos.FsPickler.IObjectVisitor;
using HashResult = Nessos.FsPickler.Hashing.HashResult;

namespace Nessos.CsPickler
{
    /// <summary>
    ///     Provides a collection of utilities and factory methods.
    /// </summary>
    public static class CsPickler
    {
        /// <summary>
        ///     Creates a new serializer instance that uses the built-in binary format.
        /// </summary>
        /// <param name="forceLittleEndian">force little-endian encoding in primitive arrays but is slower; defaults to false.</param>
        /// <returns>BinaryPickler instance.</returns>
        public static BinarySerializer CreateBinarySerializer(bool forceLittleEndian = false) 
        {
            return BinarySerializer.Create(forceLittleEndian);
        }

        /// <summary>
        ///     Creates a new serializer instance that uses the XML format.
        /// </summary>
        /// <param name="indent">indent xml serializations; defaults to false.</param>
        public static XmlSerializer CreateXmlSerializer(bool indent = false)
        {
            return XmlSerializer.Create(indent);
        }

        /// <summary>
        ///     Creates a new serializer instances that uses the JSON format.
        /// </summary>
        /// <param name="indent">indent json serializations; defaults to false.</param>
        /// <param name="omitHeader">omit FsPickler metadata at the serialization header; defaults to false.</param>
        public static JsonSerializer CreateJsonSerializer(bool indent = false, bool omitHeader = false)
        {
            return JsonSerializer.Create(indent: indent, omitHeader: omitHeader);
        }

        /// <summary>
        ///     Creates a new serializer instances that uses the BSON format.
        /// </summary>
        /// <param name="omitHeader">omit FsPickler metadata at the serialization header; defaults to false.</param>
        public static BsonSerializer CreateBsonSerializer(bool omitHeader = false)
        {
            return BsonSerializer.Create(omitHeader : omitHeader);
        }

        /// <summary>
        ///     Checks if given type is serializable.
        /// </summary>
        /// <param name="type">input type.</param>
        public static bool IsSerializableType(Type type)
        {
            return FSP.IsSerializableType(type);
        }

        /// <summary>
        ///     Checks if given type is serializable.
        /// </summary>
        /// <typeparam name="T">input type.</typeparam>
        public static bool IsSerializableType<T>()
        {
            return FSP.IsSerializableType<T>();
        }

        /// <summary>
        ///     Compute size in bytes for given value.
        /// </summary>
        /// <typeparam name="T">input value type.</typeparam>
        /// <param name="value">input value.</param>
        /// <returns></returns>
        public static long ComputeSize<T>(T value)
        {
            return FSP.ComputeSize<T>(value);
        }

        /// <summary>
        ///     Computes an 128-bit MurMur3 hash for given value.
        /// </summary>
        /// <typeparam name="T">input value type.</typeparam>
        /// <param name="value">input value.</param>
        /// <returns>128-bit hashcode.</returns>
        public static HashResult ComputeHash<T>(T value)
        {
            return FSP.ComputeHash<T>(value);
        }

        /// <summary>
        ///     Traverses a serializable object graph using an IObjectVisitor implementation.
        /// </summary>
        /// <typeparam name="T">graph type.</typeparam>
        /// <param name="visitor">visitor implementation.</param>
        /// <param name="graph">input graph.</param>
        public static void VisitObject<T>(IObjectVisitor visitor, T graph)
        {
            FSP.VisitObject<T>(visitor, graph);
        }


        /// <summary>
        ///     Performs an in-memory, deep cloning of provided serializable object graph.
        ///     Cloning is performed on a node-to-node basis and does not make use of intermediate
        ///     serialization buffers.
        /// </summary>
        /// <typeparam name="T">graph type.</typeparam>
        /// <param name="graph">object to be cloned.</param>
        /// <param name="streamingContext">payload object for StreamingContext; defaults to null.</param>
        /// <returns>Clone of provided object graph.</returns>
        public static T Clone<T>(T graph, Object streamingContext = null)
        {
            var sc = Utils.GetStreamingContext(streamingContext);
            return FSP.Clone<T>(graph, streamingContext: sc);
        }

        /// <summary>
        ///     Traverses the object graph, returning if serializable
        ///     or raising an exception if not.
        /// </summary>
        /// <typeparam name="T">Type of the object graph to be checked.</typeparam>
        /// <param name="graph">Graph to be checked.</param>
        public static void EnsureSerializable<T>(T graph)
        {
            FSP.EnsureSerializable<T>(graph);
        }
    }
}
