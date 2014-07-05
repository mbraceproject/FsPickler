using System;
using System.Reflection;
using System.IO;
using System.Text;

using FSP = Nessos.FsPickler.FsPickler;
using IObjectVisitor = Nessos.FsPickler.IObjectVisitor;
using HashResult = Nessos.FsPickler.Hashing.HashResult;

namespace Nessos.CsPickler
{
    public static class CsPickler
    {
        public static BinaryPickler CreateBinary(bool forceLittleEndian = false) 
        {
            return BinaryPickler.Create(forceLittleEndian);
        }

        public static XmlPickler CreateXml(bool indent = false)
        {
            return XmlPickler.Create(indent);
        }

        public static JsonPickler CreateJson(bool indent = false, bool omitHeader = false)
        {
            return JsonPickler.Create(indent:indent, omitHeader:omitHeader);
        }

        public static bool IsSerializableType(Type type)
        {
            return FSP.IsSerializableType(type);
        }

        public static bool IsSerializableType<T>()
        {
            return FSP.IsSerializableType<T>();
        }

        public static long ComputeSize<T>(T value)
        {
            return FSP.ComputeSize<T>(value);
        }

        public static HashResult ComputeHash<T>(T value)
        {
            return FSP.ComputeHash<T>(value);
        }

        public static void VisitObject<T>(IObjectVisitor visitor, T graph)
        {
            FSP.VisitObject<T>(visitor, graph);
        }
    }
}
