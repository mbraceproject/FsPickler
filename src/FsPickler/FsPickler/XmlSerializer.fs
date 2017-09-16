namespace MBrace.FsPickler

/// <summary>
///     XML pickler instance.
/// </summary>
[<Sealed; AutoSerializable(false)>]
type XmlSerializer =
    inherit FsPicklerTextSerializer
        
    val private format : XmlPickleFormatProvider

    /// <summary>
    ///     Define a new Xml pickler instance.
    /// </summary>
    /// <param name="indent">Enable indentation of output XML pickles.</param>
    /// <param name="typeConverter">Define a custom type name converter.</param>
    /// <param name="picklerResolver">Specify a custom pickler resolver/cache for serialization. Defaults to the singleton pickler cache.</param>
    new ([<O;D(null)>] ?indent : bool, [<O;D(null)>] ?typeConverter : ITypeNameConverter, [<O;D(null)>]?picklerResolver : IPicklerResolver) =
        let xml = new XmlPickleFormatProvider(defaultArg indent false)
        { 
            inherit FsPicklerTextSerializer(xml, ?typeConverter = typeConverter, ?picklerResolver = picklerResolver)
            format = xml    
        }

    /// Gets or sets indentation of serialized pickles.
    member x.Indent
        with get () = x.format.Indent
        and set b = x.format.Indent <- b