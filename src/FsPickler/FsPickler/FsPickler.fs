namespace Nessos.FsPickler

    open System
    
    open Nessos.FsPickler.Hashing
    open Nessos.FsPickler.TypeCache

    type FsPickler =

        static member CreateBinary(?tyConv) = 
            new DefaultBinaryPickler(?tyConv = tyConv) :> BinaryPickler

        static member CreateBclBinary(?tyConv) = 
            new BclBinaryPickler(?tyConv = tyConv) :> BinaryPickler

        static member CreateXml(?tyConv, ?indent) = 
            new XmlPickler(?tyConv = tyConv, ?indent = indent)

        static member CreateJson(?tyConv, ?indent, ?omitHeader) = 
            new JsonPickler(?tyConv = tyConv, ?indent = indent, ?omitHeader = omitHeader)

        /// Decides if given type is serializable by FsPickler
        static member IsSerializableType (t : Type) = 
            (PicklerCache.Instance :> IPicklerResolver).IsSerializable t

        /// Decides if given type is serializable by FsPickler
        static member IsSerializableType<'T> () = 
            (PicklerCache.Instance :> IPicklerResolver).IsSerializable<'T> ()

        /// Auto generates a pickler for given type variable
        static member GeneratePickler<'T> () = 
            (PicklerCache.Instance :> IPicklerResolver).Resolve<'T> ()
        
        /// Auto generates a pickler for given type
        static member GeneratePickler (t : Type) = 
            (PicklerCache.Instance :> IPicklerResolver).Resolve t