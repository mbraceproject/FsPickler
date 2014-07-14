namespace Nessos.FsPickler.Json

    open System

    open Nessos.FsPickler

    type internal OAttribute = System.Runtime.InteropServices.OptionalAttribute
    type internal DAttribute = System.Runtime.InteropServices.DefaultParameterValueAttribute

    /// <summary>
    ///     Json pickler instance.
    /// </summary>
    type JsonPickler =
        inherit FsPicklerText
        
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
                inherit FsPicklerText(json, ?typeConverter = typeConverter)
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
            and set sep =
                if sep <> null && String.IsNullOrWhiteSpace sep then
                    x.format.SequenceSeparator <- sep
                else
                    invalidArg "SequenceSeparator" "should be non-null whitespace."

        /// <summary>
        ///     Gets or sets whether top-level sequences should be serialized using the custom separator.
        /// </summary>
        member x.UseCustomTopLevelSequenceSeparator
            with get () = x.format.UseCustomTopLevelSequenceSeparator
            and set e = x.format.UseCustomTopLevelSequenceSeparator <- e


    /// FsPickler static methods.
    type FsPickler =

        /// <summary>
        ///     Initializes a new Json pickler instance.
        /// </summary>
        /// <param name="indent">indent out Json pickles.</param>
        /// <param name="omitHeader">omit FsPickler header in Json pickles.</param>
        /// <param name="typeConverter">specify a custom type name converter.</param>
        static member CreateJson([<O;D(null)>] ?indent, [<O;D(null)>] ?omitHeader, [<O;D(null)>] ?typeConverter) = 
            new JsonPickler(?indent = indent, ?omitHeader = omitHeader, ?typeConverter = typeConverter)


namespace Nessos.FsPickler.Combinators

    open Nessos.FsPickler
    open Nessos.FsPickler.Json

    /// Json pickling methods
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