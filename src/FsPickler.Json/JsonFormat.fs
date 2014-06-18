namespace Nessos.FsPickler

    open System
    open System.IO
    open System.Text

    type JsonPickleFormatProvider internal (indent, omitHeader) as self =

        let isCustomSeq isTopLevelSequence = 
            isTopLevelSequence && self.OmitHeader && self.UseCustomTopLevelSequenceSeparator

        member val Indent = indent with get,set
        member val OmitHeader = omitHeader with get,set
        member val SequenceSeparator = " " with get,set
        member val UseCustomTopLevelSequenceSeparator = false with get,set

        interface ITextPickleFormatProvider with
            member __.Name = "Json"

            // see discussion : https://github.com/nessos/FsPickler/issues/17
            member __.DefaultEncoding = new UTF8Encoding(false) :> Encoding

            member __.CreateWriter (stream, encoding, isTopLevelSequence, leaveOpen) =
                let sw = new StreamWriter(stream, encoding, 1024, leaveOpen)
                new JsonPickleWriter(sw, __.OmitHeader, __.Indent, isCustomSeq isTopLevelSequence, __.SequenceSeparator, leaveOpen) :> _

            member __.CreateReader (stream, encoding, isTopLevelSequence, leaveOpen) =
                let sr = new StreamReader(stream, encoding, true, 1024, leaveOpen)
                new JsonPickleReader(sr, __.OmitHeader, isCustomSeq isTopLevelSequence, leaveOpen) :> _

            member __.CreateWriter (textWriter, isTopLevelSequence, leaveOpen) =
                new JsonPickleWriter(textWriter, __.OmitHeader, __.Indent, isCustomSeq isTopLevelSequence, __.SequenceSeparator, leaveOpen) :> _

            member __.CreateReader (textReader, isTopLevelSequence, leaveOpen) =
                new JsonPickleReader(textReader, __.OmitHeader, isCustomSeq isTopLevelSequence, leaveOpen) :> _