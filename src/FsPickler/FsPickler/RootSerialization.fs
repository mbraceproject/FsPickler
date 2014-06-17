module internal Nessos.FsPickler.RootSerialization

    open System
    open System.Collections
    open System.Collections.Generic
    open System.Text
    open System.IO
    open System.Runtime.Serialization
    
    open Nessos.FsPickler
    open Nessos.FsPickler.TypeCache
    open Nessos.FsPickler.SequenceUtils

    let initStreamWriter (formatP : IPickleFormatProvider) stream encoding leaveOpen =
        let leaveOpen = defaultArg leaveOpen false
        let encoding = defaultArg encoding formatP.DefaultEncoding
        formatP.CreateWriter(stream, encoding, leaveOpen)

    let initStreamReader (formatP : IPickleFormatProvider) stream encoding leaveOpen =
        let leaveOpen = defaultArg leaveOpen false
        let encoding = defaultArg encoding formatP.DefaultEncoding
        formatP.CreateReader(stream, encoding, leaveOpen)

    let initTextWriter (formatP : ITextPickleFormatProvider) writer leaveOpen =
        let leaveOpen = defaultArg leaveOpen false
        formatP.CreateWriter(writer, leaveOpen)

    let initTextReader (formatP : ITextPickleFormatProvider) reader leaveOpen =
        let leaveOpen = defaultArg leaveOpen false
        formatP.CreateReader(reader, leaveOpen)

    let writeRootObject resolver reflectionCache formatter streamingContext (pickler : Pickler<'T>) (value : 'T) =
        let writeState = new WriteState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qualifiedName = reflectionCache.GetQualifiedName pickler.Type
        formatter.BeginWriteRoot qualifiedName
        pickler.Write writeState "value" value
        formatter.EndWriteRoot ()

    let readRootObject resolver reflectionCache formatter streamingContext (pickler : Pickler<'T>) =
        let readState = new ReadState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qualifiedName = reflectionCache.GetQualifiedName pickler.Type

        try formatter.BeginReadRoot qualifiedName
        with 
        | :? FsPicklerException -> reraise () 
        | e -> raise <| new InvalidPickleException("error reading from pickle.", e)

        let value = pickler.Read readState "value"
        formatter.EndReadRoot ()
        value

    let writeRootObjectUntyped resolver reflectionCache formatter streamingContext (pickler : Pickler) (value : obj) =
        let writeState = new WriteState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qualifiedName = reflectionCache.GetQualifiedName pickler.Type
        formatter.BeginWriteRoot qualifiedName
        pickler.UntypedWrite writeState "value" value
        formatter.EndWriteRoot ()

    let readRootObjectUntyped resolver reflectionCache formatter streamingContext (pickler : Pickler) =
        let readState = new ReadState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qualifiedName = reflectionCache.GetQualifiedName pickler.Type
        try formatter.BeginReadRoot qualifiedName
        with 
        | :? FsPicklerException -> reraise () 
        | e -> raise <| new InvalidPickleException("error reading from pickle.", e)

        let value = pickler.UntypedRead readState "value"
        formatter.EndReadRoot ()
        value

    //
    //  top-level sequence serialization
    //

    /// serializes a sequence of objects to stream

    let writeTopLevelSequence resolver reflectionCache formatter streamingContext (pickler : Pickler<'T>) (values : seq<'T>) : int =
        // write state initialization
        let state = new WriteState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qn = reflectionCache.GetQualifiedName pickler.Type + " seq"

        state.Formatter.BeginWriteRoot qn
        let length = writeUnboundedSequence pickler state "values" values
        state.Formatter.EndWriteRoot ()
        length

    let readTopLevelSequence resolver reflectionCache formatter streamingContext (pickler : Pickler<'T>) : seq<'T> =
        
        // read state initialization
        let state = new ReadState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qn = reflectionCache.GetQualifiedName pickler.Type + " seq"

        // read stream header
        try formatter.BeginReadRoot qn
        with 
        | :? FsPicklerException -> reraise () 
        | e -> raise <| new InvalidPickleException("error reading from pickle.", e)

        readUnboundedSequenceLazy pickler state "values"



    let writeTopLevelSequenceUntyped resolver reflectionCache formatter streamingContext (pickler : Pickler) (values : IEnumerable) : int =
        let unpacker =
            {
                new IPicklerUnpacker<int> with
                    member __.Apply (p : Pickler<'T>) =
                        writeTopLevelSequence resolver reflectionCache formatter
                            streamingContext p (values :?> IEnumerable<'T>)
            }

        pickler.Unpack unpacker

    let readTopLevelSequenceUntyped resolver reflectionCache formatter streamingContext (pickler : Pickler) : IEnumerable =
        let unpacker =
            {
                new IPicklerUnpacker<IEnumerable> with
                    member __.Apply (p : Pickler<'T>) =
                        readTopLevelSequence resolver reflectionCache formatter 
                            streamingContext p :> IEnumerable
            }

        pickler.Unpack unpacker