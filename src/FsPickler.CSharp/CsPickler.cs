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
        ///     Creates a new BinaryPickler instance.
        /// </summary>
        /// <param name="forceLittleEndian">force little-endian encoding in primitive arrays but is slower; defaults to false.</param>
        /// <returns>BinaryPickler instance.</returns>
        public static BinarySerializer CreateBinary(bool forceLittleEndian = false) 
        {
            return BinarySerializer.Create(forceLittleEndian);
        }

        /// <summary>
        ///     Creates a new XmlPickler instance.
        /// </summary>
        /// <param name="indent">indent xml serializations; defaults to false.</param>
        public static XmlSerializer CreateXml(bool indent = false)
        {
            return XmlSerializer.Create(indent);
        }

        /// <summary>
        ///     Creates a new JsonPickler instance.
        /// </summary>
        /// <param name="indent">indent json serializations; defaults to false.</param>
        /// <param name="omitHeader">omit FsPickler metadata at the serialization header; defaults to false.</param>
        public static JsonSerializer CreateJson(bool indent = false, bool omitHeader = false)
        {
            return JsonSerializer.Create(indent: indent, omitHeader: omitHeader);
        }

        /// <summary>
        ///     Creates a new BsonPickler instance.
        /// </summary>
        public static BsonSerializer CreateBson()
        {
            return BsonSerializer.Create();
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
    }
}
