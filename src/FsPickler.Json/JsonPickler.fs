namespace Nessos.FsPickler

    type JsonPickler =
        inherit TextPickler
        
        val private format : JsonPickleFormatProvider

        new (?tyConv, ?indent, ?omitHeader) =
            let indent = defaultArg indent false
            let omitHeader = defaultArg omitHeader false
            let json = new JsonPickleFormatProvider(indent, omitHeader)
            { 
                inherit TextPickler(json, ?tyConv = tyConv)
                format = json    
            }

        member x.Indent
            with get () = x.format.Indent
            and set b = x.format.Indent <- b

        member x.OmitHeader
            with get () = x.format.OmitHeader
            and set b = x.format.OmitHeader <- b


    type FsPickler =

        /// <summary>
        ///     Create a new JSON pickler instance.
        /// </summary>
        /// <param name="tyConv">optional type name converter implementation.</param>
        static member CreateJson(?tyConv, ?indent, ?omitHeader) = 
            new JsonPickler(?tyConv = tyConv, ?indent = indent, ?omitHeader = omitHeader)


    [<RequireQualifiedAccess>]
    module Json =
        let private jsonPickler = lazy(FsPickler.CreateJson(omitHeader = true))

        /// pickles a value
        let pickle (pickler : Pickler<'T>) (value : 'T) : string =
            jsonPickler.Value.PickleToString (pickler, value)

        /// upickles a value
        let unpickle (pickler : Pickler<'T>) (pickle : string) =
            jsonPickler.Value.UnPickleOfString (pickler, pickle)