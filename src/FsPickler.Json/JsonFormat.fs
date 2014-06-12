namespace Nessos.FsPickler

    open System.IO
    open System.Text

    type JsonPickleFormatProvider (indent, omitHeader) =

        static let defaultEnc e = match e with None -> UTF8Encoding(false) :> Encoding | Some e -> e
        
        member val Indent = indent with get,set
        member val OmitHeader = omitHeader with get,set

        interface ITextPickleFormatProvider with
            member __.Name = "Json"

            member __.CreateWriter (stream, leaveOpen, ?encoding) =
                let sw = new StreamWriter(stream, defaultEnc encoding, 1024, leaveOpen)
                new JsonPickleWriter(sw, __.OmitHeader, __.Indent, leaveOpen) :> _

            member __.CreateReader (stream, leaveOpen, ?encoding) =
                let sr = new StreamReader(stream, defaultEnc encoding, true, 1024, leaveOpen)
                new JsonPickleReader(sr, __.OmitHeader, leaveOpen) :> _

            member __.CreateWriter (textWriter, leaveOpen) = 
                new JsonPickleWriter(textWriter, __.OmitHeader, __.Indent, leaveOpen) :> _

            member __.CreateReader (textReader, leaveOpen) = 
                new JsonPickleReader(textReader, __.OmitHeader, leaveOpen) :> _