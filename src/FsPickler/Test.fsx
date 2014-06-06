#I "../../bin/"
#r "FsPickler.dll"

open Nessos.FsPickler
open Nessos.FsPickler.Combinators

let qPickler = Pickler.auto<Quotations.Expr<int>>

let bp = Binary.pickle qPickler <@ 1 + 1 @>
let xp = Xml.pickle qPickler <@ 1 + 1 @>
let jp = Json.pickle qPickler <@ 1 + 1 @>

Binary.unpickle qPickler bp
Xml.unpickle qPickler xp
Json.unpickle qPickler jp

let fPickler = Pickler.func<int, int>
let bp' = Binary.pickle fPickler (fun x -> x + 1)
let xp' = Xml.pickle fPickler (fun x -> x + 1)
let jp' = Json.pickle fPickler (fun x -> x + 1)

Binary.unpickle fPickler bp' 41
Xml.unpickle fPickler xp' 41
Json.unpickle fPickler jp' 41


#r "Newtonsoft.Json.dll"

open Newtonsoft.Json
open System.IO

let sw = new StringWriter()
let jw = new JsonTextWriter(sw)

jw.WriteValue 129381029381230981231M

jw.Flush()

let text = sw.ToString()

let sr = new StringReader(text)
let jr = new JsonTextReader(sr)

jr.Read()
jr.ReadAsString()
jr.TokenType
jr.Value