namespace Nessos.FsPickler

open System
open System.IO
open System.Text

open Nessos.FsPickler.Hashing
open Nessos.FsPickler.RootSerialization
open Nessos.FsPickler.ReflectionCache

type internal OAttribute = System.Runtime.InteropServices.OptionalAttribute
type internal DAttribute = System.Runtime.InteropServices.DefaultParameterValueAttribute

/// Computes the accumulated size for a collection of user-provided serializable objects
[<AutoSerializable(false)>]
type ObjectSizeCounter internal (formatProvider : IPickleFormatProvider, resolver : IPicklerResolver, reflectionCache : ReflectionCache, encoding : Encoding option, resetInterval : int64 option) =
    let lengthCounter = new LengthCounterStream()
    let mutable resetCount = 0L
    let writer = initStreamWriter formatProvider lengthCounter encoding true None
    let writeState = new WriteState(writer, resolver, reflectionCache, true)

    /// Gets accumulated object size in bytes
    member __.Count = writer.Flush() ; lengthCounter.Count
    /// Resets the serialization state object id generators
    member __.Reset() = writeState.Reset()
    /// Appends a value to the size count
    member __.Append<'T>(value : 'T, [<O;D(null)>] ?pickler:Pickler<'T>) =
        match resetInterval with
        | Some ri ->
            if resetCount = ri then
                writeState.Reset()
                resetCount <- 0L
            else
                resetCount <- resetCount + 1L
        | None -> ()

        let pickler = match pickler with None -> resolver.Resolve<'T>() | Some p -> p
        pickler.Write writeState "elem" value

    interface IDisposable with
        member __.Dispose() = (writer :> IDisposable).Dispose()