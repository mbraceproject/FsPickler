module internal Nessos.FsPickler.RootSerialization

open System
open System.Collections
open System.Collections.Generic
open System.Text
open System.IO
open System.Runtime.Serialization
    
open Nessos.FsPickler
open Nessos.FsPickler.ReflectionCache
open Nessos.FsPickler.SequenceUtils

/// Initializes an IPicklerFormatWriter instance using a targeted System.IO.Stream and parameters
let initStreamWriter (formatP : IPickleFormatProvider) stream encoding isSeq leaveOpen =
    let leaveOpen = defaultArg leaveOpen false
    let encoding = defaultArg encoding formatP.DefaultEncoding
    formatP.CreateWriter(stream, encoding, isSeq, leaveOpen)

/// Initializes an IPickleFormatReader instance using a targeted System.IO.Stream and parameters
let initStreamReader (formatP : IPickleFormatProvider) stream encoding isSeq leaveOpen =
    let leaveOpen = defaultArg leaveOpen false
    let encoding = defaultArg encoding formatP.DefaultEncoding
    formatP.CreateReader(stream, encoding, isSeq, leaveOpen)

/// Initializes an IPickleFormatWriter instance using a targeted System.IO.TextWriter and parameters
let initTextWriter (formatP : ITextPickleFormatProvider) writer isSeq leaveOpen =
    let leaveOpen = defaultArg leaveOpen false
    formatP.CreateWriter(writer, isSeq, leaveOpen)

/// Initializes an IPickleFormatReader instance using a targeted System.IO.TextReader and parameters
let initTextReader (formatP : ITextPickleFormatProvider) reader isSeq leaveOpen =
    let leaveOpen = defaultArg leaveOpen false
    formatP.CreateReader(reader, isSeq, leaveOpen)

/// Initializes a WriteState instance and write value to stream
let writeRootObject resolver reflectionCache formatter streamingContext sifter isHash (pickler : Pickler<'T>) (value : 'T) =
    try
        let writeState = new WriteState(formatter, resolver, reflectionCache, isHash, ?streamingContext = streamingContext, ?sifter = sifter)
        let typeName = reflectionCache.GetTypeSignature pickler.Type
        formatter.BeginWriteRoot typeName
        pickler.Write writeState "value" value
        formatter.EndWriteRoot ()
        writeState

    with e ->
        raise <| new FsPicklerException(sprintf "Error serializing object of type '%O'." typeof<'T>, e)

/// Initializes a ReadState instance and read value from stream
let readRootObject resolver reflectionCache formatter streamingContext sifted (pickler : Pickler<'T>) =
    try
        let readState = new ReadState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext, ?sifted = sifted)
        let typeName = reflectionCache.GetTypeSignature pickler.Type

        formatter.BeginReadRoot typeName
        let value = pickler.Read readState "value"
        formatter.EndReadRoot ()
        value

    with e ->
        raise <| new FsPicklerException(sprintf "Error deserializing object of type '%O'." typeof<'T>, e)

/// Initializes a WriteState instance and write untyped value to stream
let writeRootObjectUntyped resolver reflectionCache formatter streamingContext sifter isHash (pickler : Pickler) (value : obj) =
    try
        let writeState = new WriteState(formatter, resolver, reflectionCache, isHash, ?streamingContext = streamingContext, ?sifter = sifter)
        let typeName = reflectionCache.GetTypeSignature pickler.Type
        formatter.BeginWriteRoot typeName
        pickler.UntypedWrite writeState "value" value
        formatter.EndWriteRoot ()
        writeState

    with e ->
        raise <| new FsPicklerException(sprintf "Error serializing object of type '%O'." pickler.Type, e)

/// Initializes a ReadState instance and read untyped value from stream
let readRootObjectUntyped resolver reflectionCache formatter streamingContext sifted (pickler : Pickler) =
    try
        let readState = new ReadState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext, ?sifted = sifted)
        let typeName = reflectionCache.GetTypeSignature pickler.Type
        formatter.BeginReadRoot typeName

        let value = pickler.UntypedRead readState "value"
        formatter.EndReadRoot ()
        value

    with e ->
        raise <| new FsPicklerException(sprintf "Error deserializing object of type '%O'." pickler.Type, e)

//
//  top-level sequence serialization
//

/// serializes a sequence of objects to stream
let writeTopLevelSequence resolver reflectionCache formatter streamingContext isHash (pickler : Pickler<'T>) (values : seq<'T>) : int =
    try
        // write state initialization
        let state = new WriteState(formatter, resolver, reflectionCache, isHash, ?streamingContext = streamingContext)
        let typeName = reflectionCache.GetTypeSignature pickler.Type + " seq"

        state.Formatter.BeginWriteRoot typeName
        let length = writeUnboundedSequence true pickler state "values" values
        state.Formatter.EndWriteRoot ()
        length

    with e ->
        raise <| new FsPicklerException(sprintf "Error serializing sequence of type '%O'." typeof<'T>, e)

/// deserializes a sequence of objects from stream
let readTopLevelSequence resolver reflectionCache formatter streamingContext (pickler : Pickler<'T>) : seq<'T> =
    try
        // read state initialization
        let state = new ReadState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let typeName = reflectionCache.GetTypeSignature pickler.Type + " seq"

        // read stream header
        formatter.BeginReadRoot typeName
        readUnboundedSequenceLazy true pickler state "values"
    with e ->
        raise <| new FsPicklerException(sprintf "Error deserializing sequence of type '%O'." typeof<'T>, e)

/// serializes a sequence of untyped objects to stream
let writeTopLevelSequenceUntyped resolver reflectionCache formatter streamingContext isHash (pickler : Pickler) (values : IEnumerable) : int =
    let unpacker =
        {
            new IPicklerUnpacker<int> with
                member __.Apply (p : Pickler<'T>) =
                    writeTopLevelSequence resolver reflectionCache formatter
                        streamingContext isHash p (values :?> IEnumerable<'T>)
        }

    pickler.Unpack unpacker

/// deserializes a sequence of untyped objects from stream
let readTopLevelSequenceUntyped resolver reflectionCache formatter streamingContext (pickler : Pickler) : IEnumerable =
    let unpacker =
        {
            new IPicklerUnpacker<IEnumerable> with
                member __.Apply (p : Pickler<'T>) =
                    readTopLevelSequence resolver reflectionCache formatter 
                        streamingContext p :> IEnumerable
        }

    pickler.Unpack unpacker