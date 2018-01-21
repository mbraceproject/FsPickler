namespace MBrace.FsPickler.Json

open System
open System.IO
open System.Text

open Newtonsoft.Json

open MBrace.FsPickler

/// <summary>
///     Factory methods for the Json serialization format.
/// </summary>
type JsonPickleFormatProvider internal (indent, omitHeader) as self =

    let isCustomSeq isTopLevelSequence = 
        isTopLevelSequence && self.OmitHeader && self.UseCustomTopLevelSequenceSeparator

    let mutable sequenceSeparator = " "

    member val Indent = indent with get,set
    member val OmitHeader = omitHeader with get,set
    member val UseCustomTopLevelSequenceSeparator = false with get,set

    member __.SequenceSeparator
        with get () = sequenceSeparator
        and set (sep:string) =
            if sep <> null && String.IsNullOrEmpty(sep.Trim()) then
                sequenceSeparator <- sep
            else
                invalidArg "SequenceSeparator" "should be non-null whitespace."

    interface ITextPickleFormatProvider with
        member __.Name = "Json"

        // see discussion : https://github.com/mbraceproject/FsPickler/issues/17
        member __.DefaultEncoding = new UTF8Encoding(false) :> Encoding

        member __.CreateWriter (stream, encoding, isTopLevelSequence, leaveOpen) =
            let sw = new StreamWriter(stream, encoding, 1024, leaveOpen)
            let jw = new JsonTextWriter(sw)
            new JsonPickleWriter(jw, __.OmitHeader, __.Indent, isCustomSeq isTopLevelSequence, sequenceSeparator, leaveOpen) :> _

        member __.CreateReader (stream, encoding, isTopLevelSequence, leaveOpen) =
            let sr = new StreamReader(stream, encoding, true, 1024, leaveOpen)
            let jr = new JsonTextReader(sr)
            new JsonPickleReader(jr, __.OmitHeader, __.UseCustomTopLevelSequenceSeparator, isCustomSeq isTopLevelSequence, leaveOpen) :> _

        member __.CreateWriter (textWriter, isTopLevelSequence, leaveOpen) =
            let jw = new JsonTextWriter(textWriter)
            new JsonPickleWriter(jw, __.OmitHeader, __.Indent, isCustomSeq isTopLevelSequence, sequenceSeparator, leaveOpen) :> _

        member __.CreateReader (textReader, isTopLevelSequence, leaveOpen) =
            let jr = new JsonTextReader(textReader)
            new JsonPickleReader(jr, __.OmitHeader, __.UseCustomTopLevelSequenceSeparator, isCustomSeq isTopLevelSequence, leaveOpen) :> _