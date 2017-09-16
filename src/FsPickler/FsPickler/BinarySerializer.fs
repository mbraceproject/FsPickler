namespace MBrace.FsPickler

/// <summary>
///     Binary pickler instance.
/// </summary>
[<Sealed; AutoSerializable(false)>]
type BinarySerializer =
    inherit FsPicklerSerializer

    val private format : BinaryPickleFormatProvider

    /// <summary>
    ///     Initializes a new Binary pickler instance.
    /// </summary>
    /// <param name="forceLittleEndian">Force little-endian encoding in primitive arrays but is slower. Defaults to false.</param>
    /// <param name="typeConverter">Define a custom type name converter.</param>
    /// <param name="picklerResolver">Specify a custom pickler resolver/cache for serialization. Defaults to the singleton pickler cache.</param>
    new ([<O;D(null)>]?forceLittleEndian : bool, [<O;D(null)>] ?typeConverter : ITypeNameConverter, [<O;D(null)>]?picklerResolver : IPicklerResolver) =
        let forceLittleEndian = defaultArg forceLittleEndian false
        let format = new BinaryPickleFormatProvider(forceLittleEndian)
        { 
            inherit FsPicklerSerializer(format, ?typeConverter = typeConverter, ?picklerResolver = picklerResolver) 
            format = format    
        }

    /// Gets or sets the ForceLittleEndian setting.
    /// Uses BinaryWriter rather than Buffer.BlockCopy 
    /// for array serializations but is slower.
    member b.ForceLittleEndian
        with get () = b.format.ForceLittleEndian
        and set f = b.format.ForceLittleEndian <- f