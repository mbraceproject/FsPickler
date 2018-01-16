namespace MBrace.FsPickler.Json

open System

open MBrace.FsPickler

#nowarn "44"

type internal OAttribute = System.Runtime.InteropServices.OptionalAttribute
type internal DAttribute = System.Runtime.InteropServices.DefaultParameterValueAttribute

/// <summary>
///     Json pickler instance.
/// </summary>
type JsonSerializer =
    inherit FsPicklerTextSerializer
        
    val private format : JsonPickleFormatProvider

    /// <summary>
    ///     Initializes a new Json pickler instance.
    /// </summary>
    /// <param name="indent">indent out Json pickles.</param>
    /// <param name="omitHeader">omit FsPickler header in Json pickles.</param>
    /// <param name="typeConverter">specify a custom type name converter.</param>
    /// <param name="picklerResolver">Specify a custom pickler resolver/cache for serialization. Defaults to the singleton pickler cache.</param>
    new ([<O;D(null)>] ?indent, [<O;D(null)>] ?omitHeader, [<O;D(null)>] ?typeConverter, [<O;D(null)>] ?picklerResolver) =
        let indent = defaultArg indent false
        let omitHeader = defaultArg omitHeader false
        let json = new JsonPickleFormatProvider(indent, omitHeader)
        { 
            inherit FsPicklerTextSerializer(json, ?typeConverter = typeConverter, ?picklerResolver = picklerResolver)
            format = json    
        }

    /// <summary>
    ///     Gets or sets whether Json output should be indented.
    /// </summary>
    member x.Indent
        with get () = x.format.Indent
        and set b = x.format.Indent <- b

    /// <summary>
    ///     Gets or sets whether FsPickler headers should be ignored in pickle format.
    /// </summary>
    member x.OmitHeader
        with get () = x.format.OmitHeader
        and set b = x.format.OmitHeader <- b

    /// <summary>
    ///     Gets or sets a non-null whitespace string that serves as a custom, top-level sequence separator.
    /// </summary>
    member x.SequenceSeparator
        with get () = x.format.SequenceSeparator
        and set sep = x.format.SequenceSeparator <- sep

    /// <summary>
    ///     Gets or sets whether top-level sequences should be serialized using the custom separator.
    /// </summary>
    member x.UseCustomTopLevelSequenceSeparator
        with get () = x.format.UseCustomTopLevelSequenceSeparator
        and set e = x.format.UseCustomTopLevelSequenceSeparator <- e

/// <summary>
///     BSON pickler instance.
/// </summary>
[<Obsolete("BSON format has been deprecated by Newtonsoft")>]
type BsonSerializer([<O;D(null)>] ?typeConverter, [<O;D(null)>] ?picklerResolver) =
    inherit FsPicklerSerializer(new BsonPickleFormatProvider(), ?typeConverter = typeConverter, ?picklerResolver = picklerResolver)


/// FsPickler static methods.
type FsPickler =

    /// <summary>
    ///     Initializes a new FsPickler serializer instance that uses the JSON format.
    /// </summary>
    /// <param name="indent">indent out Json pickles.</param>
    /// <param name="omitHeader">omit FsPickler header in Json pickles.</param>
    /// <param name="typeConverter">specify a custom type name converter.</param>
    /// <param name="picklerResolver">Specify a custom pickler resolver/cache for serialization. Defaults to the singleton pickler cache.</param>
    static member CreateJsonSerializer([<O;D(null)>] ?indent, [<O;D(null)>] ?omitHeader, [<O;D(null)>] ?typeConverter, [<O;D(null)>] ?picklerResolver) = 
        new JsonSerializer(?indent = indent, ?omitHeader = omitHeader, ?typeConverter = typeConverter, ?picklerResolver = picklerResolver)

    /// <summary>
    ///     Initializes a new FsPickler serializer instance that uses the BSON format.
    /// </summary>
    /// <param name="typeConverter">specify a custom type name converter.</param>
    [<Obsolete("BSON format has been deprecated by Newtonsoft")>]
    static member CreateBsonSerializer([<O;D(null)>] ?typeConverter, [<O;D(null)>] ?picklerResolver) = 
        new BsonSerializer(?typeConverter = typeConverter, ?picklerResolver = picklerResolver)