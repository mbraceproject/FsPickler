namespace MBrace.FsPickler.Json

open System
open System.IO
open System.Text

open Newtonsoft.Json
open Newtonsoft.Json.Bson

open MBrace.FsPickler

#nowarn "44" // BsonWriter

/// <summary>
///     BSON format factory methods.
/// </summary>
[<Obsolete("BSON format has been deprecated by Newtonsoft")>]
type BsonPickleFormatProvider() =
        
    interface IPickleFormatProvider with

        member __.Name = "Bson"
        member __.DefaultEncoding = Encoding.UTF8

        member __.CreateWriter(stream : Stream, encoding : Encoding, _ : bool, leaveOpen : bool) =
            let bw = new BinaryWriter(stream, encoding, leaveOpen)
            let bsonWriter = new BsonWriter(bw)
            new JsonPickleWriter(bsonWriter, false, false, false, null, leaveOpen) :> _

        member __.CreateReader(stream : Stream, encoding : Encoding, _ : bool, leaveOpen : bool) =
            let br = new BinaryReader(stream, encoding, leaveOpen)
            let bsonReader = new BsonReader(br)
            new JsonPickleReader(bsonReader, false, false, false, leaveOpen) :> _