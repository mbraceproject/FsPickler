namespace MBrace.FsPickler

open System
open System.IO
open System.Collections.Generic

open Microsoft.FSharp.Reflection

open MBrace.FsPickler.Reflection

#if EMIT_IL
open System.Reflection.Emit
open MBrace.FsPickler.Emit
#endif

//
//  Provides facility for serializing union cases
//

type internal UnionCaseSerializationHelper(caseNames : string []) =
    let dict = new Dictionary<string, int>(caseNames.Length)
    do 
        for i = 0 to caseNames.Length - 1 do
            dict.Add(caseNames.[i], i)

    member __.WriteTag (formatter : IPickleFormatWriter, tag : int) =
        if formatter.SerializeUnionCaseNames then
            formatter.WriteString "Case" <| caseNames.[tag]
        else
            formatter.WriteInt32 "Tag" tag

    member __.ReadTag (formatter : IPickleFormatReader) =
        if formatter.SerializeUnionCaseNames then
            let case = formatter.ReadString "Case"
            if isNull case then
                raise <| new FormatException("invalid union case 'null'.")

            let mutable tag = Unchecked.defaultof<int>
            let ok = dict.TryGetValue(case, &tag)
            if ok then tag
            else
                raise <| new FormatException(sprintf "invalid union case '%s'." case)
        else
            formatter.ReadInt32 "Tag"


    static member OfUnionType<'Union> () =
        let cases =
            FSharpType.GetUnionCases(typeof<'Union>, allMembers)
            |> Array.map(fun u -> u.Name)

        new UnionCaseSerializationHelper(cases)

#if EMIT_IL
    static member InvokeTagWriter (c : EnvItem<UnionCaseSerializationHelper>)
                                    (w : EnvItem<WriteState>) (tag : EnvItem<int>) 
                                    (ilGen : ILGenerator) =
        c.Load()
        w.Load()
        let m = typeof<WriteState>.GetMethod("get_Formatter", allMembers)
        ilGen.EmitCall(OpCodes.Call, m, null)
        tag.Load()
        let m = typeof<UnionCaseSerializationHelper>.GetMethod("WriteTag", allMembers)
        ilGen.EmitCall(OpCodes.Call, m, null)


    static member InvokeTagReader (c : EnvItem<UnionCaseSerializationHelper>) 
                                    (r : EnvItem<ReadState>) (ilGen : ILGenerator) =
            
        c.Load()
        r.Load()
        let m = typeof<ReadState>.GetMethod("get_Formatter", allMembers)
        ilGen.EmitCall(OpCodes.Call, m, null)

        let m = typeof<UnionCaseSerializationHelper>.GetMethod("ReadTag", allMembers)
        ilGen.EmitCall(OpCodes.Call, m, null)
#endif