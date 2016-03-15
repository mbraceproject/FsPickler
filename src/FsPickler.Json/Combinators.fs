namespace Nessos.FsPickler.Combinators

open Nessos.FsPickler
open Nessos.FsPickler.Json

/// Json pickling methods
[<RequireQualifiedAccess>]
module Json =

    let private jsonSerializer = lazy(FsPickler.CreateJsonSerializer(omitHeader = true))

    /// <summary>
    ///     Pickles a value to Json.
    /// </summary>
    /// <param name="pickler">utilized pickler.</param>
    /// <param name="value">input value.</param>
    let pickle (pickler : Pickler<'T>) (value : 'T) : string =
        jsonSerializer.Value.PickleToString (value, pickler = pickler)

    /// <summary>
    ///     Unpickles a value from Json.
    /// </summary>
    /// <param name="pickler">utilized pickler.</param>
    /// <param name="pickle">input pickle.</param>
    let unpickle (pickler : Pickler<'T>) (pickle : string) : 'T =
        jsonSerializer.Value.UnPickleOfString (pickle, pickler = pickler)


/// Bson pickling methods
[<RequireQualifiedAccess>]
module Bson =

    let private bsonPickler = lazy(FsPickler.CreateBsonSerializer())

    /// <summary>
    ///     Pickles a value to BSON.
    /// </summary>
    /// <param name="pickler">utilized pickler.</param>
    /// <param name="value">input value.</param>
    let pickle (pickler : Pickler<'T>) (value : 'T) : byte [] =
        bsonPickler.Value.Pickle (value, pickler = pickler)

    /// <summary>
    ///     Unpickles a value from BSON.
    /// </summary>
    /// <param name="pickler">utilized pickler.</param>
    /// <param name="pickle">input pickle.</param>
    let unpickle (pickler : Pickler<'T>) (pickle : byte []) : 'T =
        bsonPickler.Value.UnPickle (pickle, pickler = pickler)