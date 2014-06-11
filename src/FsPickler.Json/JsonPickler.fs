namespace Nessos.FsPickler

    type internal OAttribute = System.Runtime.InteropServices.OptionalAttribute
    type internal DAttribute = System.Runtime.InteropServices.DefaultParameterValueAttribute

    /// <summary>
    ///     Json pickler instance.
    /// </summary>
    type JsonPickler =
        inherit TextPickler
        
        val private format : JsonPickleFormatProvider

        /// <summary>
        ///     Initializes a new Json pickler instance.
        /// </summary>
        /// <param name="indent">indent out Json pickles.</param>
        /// <param name="omitHeader">omit FsPickler header in Json pickles.</param>
        /// <param name="typeConverter">specify a custom type name converter.</param>
        new ([<O;D(null)>] ?indent, [<O;D(null)>] ?omitHeader, [<O;D(null)>] ?typeConverter) =
            let indent = defaultArg indent false
            let omitHeader = defaultArg omitHeader false
            let json = new JsonPickleFormatProvider(indent, omitHeader)
            { 
                inherit TextPickler(json, ?typeConverter = typeConverter)
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


    type FsPickler =

        /// <summary>
        ///     Initializes a new Json pickler instance.
        /// </summary>
        /// <param name="indent">indent out Json pickles.</param>
        /// <param name="omitHeader">omit FsPickler header in Json pickles.</param>
        /// <param name="typeConverter">specify a custom type name converter.</param>
        static member CreateJson([<O;D(null)>] ?indent, [<O;D(null)>] ?omitHeader, [<O;D(null)>] ?typeConverter) = 
            new JsonPickler(?indent = indent, ?omitHeader = omitHeader, ?typeConverter = typeConverter)


    [<RequireQualifiedAccess>]
    module Json =

        let private jsonPickler = lazy(FsPickler.CreateJson(omitHeader = true))

        /// <summary>
        ///     Pickles a value to json.
        /// </summary>
        /// <param name="pickler">utilized pickler.</param>
        /// <param name="value">input value.</param>
        let pickle (pickler : Pickler<'T>) (value : 'T) : string =
            jsonPickler.Value.PickleToString (pickler, value)

        /// <summary>
        ///     Unpickles a values from json.
        /// </summary>
        /// <param name="pickler">utilized pickler.</param>
        /// <param name="pickle">input pickle.</param>
        let unpickle (pickler : Pickler<'T>) (pickle : string) : 'T =
            jsonPickler.Value.UnPickleOfString (pickler, pickle)