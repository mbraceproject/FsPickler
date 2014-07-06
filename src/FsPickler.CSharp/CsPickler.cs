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
    ///     Public API.
    /// </summary>
    public static class CsPickler
    {
        /// <summary>
        ///     Creates a new Binary FsPickler instance.
        /// </summary>
        /// <param name="forceLittleEndian">force little-endian encoding in primitive arrays but is slower; defaults to false.</param>
        /// <returns>Binary Pickler.</returns>
        public static BinaryPickler CreateBinary(bool forceLittleEndian = false) 
        {
            return BinaryPickler.Create(forceLittleEndian);
        }

        /// <summary>
        ///     Creates a new Xml FsPickler instance.
        /// </summary>
        /// <param name="indent">indent xml serializations; defaults to false.</param>
        /// <returns>Xml FsPickler instance.</returns>
        public static XmlPickler CreateXml(bool indent = false)
        {
            return XmlPickler.Create(indent);
        }

        /// <summary>
        ///     Creates a new Json FsPickler instance.
        /// </summary>
        /// <param name="indent">Indent Json serialization.</param>
        /// <param name="omitHeader">Omit FsPickler metadata at the serialization header.</param>
        /// <returns>a JsonPickler instance.</returns>
        public static JsonPickler CreateJson(bool indent = false, bool omitHeader = false)
        {
            return JsonPickler.Create(indent:indent, omitHeader:omitHeader);
        }

        /// <summary>
        ///     Checks if given type is serializable.
        /// </summary>
        /// <param name="type">input type.</param>
        /// <returns></returns>
        public static bool IsSerializableType(Type type)
        {
            return FSP.IsSerializableType(type);
        }

        /// <summary>
        ///     Checks if given type is serializable.
        /// </summary>
        /// <typeparam name="T">input type.</typeparam>
        /// <returns></returns>
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
    }
}
