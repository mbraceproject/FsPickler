namespace Nessos.FsPickler

    open System
    open System.IO
    open System.Collections.Generic

#if EMIT_IL
    open System.Reflection.Emit
    open Nessos.FsPickler.Emit
    open Nessos.FsPickler.Reflection
#endif

    type internal UnionCaseSerializationHelper(caseNames : string []) =
        let n = caseNames.Length
        let dict = new Dictionary<string, int>()
        do 
            for i = 0 to n - 1 do
                dict.Add(caseNames.[i], i)

        member __.WriteTag (state : WriteState, tag : int) =
            if state.Formatter.SerializeUnionCaseNames then
                state.Formatter.WriteString "Case" <| caseNames.[tag]
            else
                state.Formatter.WriteInt32 "Tag" tag

        member __.ReadTag (state : ReadState) =
            if state.Formatter.SerializeUnionCaseNames then
                let case = state.Formatter.ReadString "Case"
                if obj.ReferenceEquals(case, null) then
                    raise <| new FormatException("invalid union case 'null'.")

                let ok, tag = dict.TryGetValue case
                if ok then tag
                else
                    raise <| new FormatException(sprintf "invalid union case '%s'." case)
            else
                let tag = state.Formatter.ReadInt32 "Tag"
                if tag < 0 || tag >= n then
                    raise <| new InvalidDataException(sprintf "invalid case tag '%d'" tag)

                tag

#if EMIT_IL
        static member InvokeTagWriter (c : EnvItem<UnionCaseSerializationHelper>) 
                                        (w : EnvItem<WriteState>) (tag : EnvItem<int>) 
                                        (ilGen : ILGenerator) =
            c.Load()
            w.Load()
            tag.Load()
            let m = typeof<UnionCaseSerializationHelper>.GetMethod("WriteTag", allMembers)
            ilGen.EmitCall(OpCodes.Call, m, null)


        static member InvokeTagReader (c : EnvItem<UnionCaseSerializationHelper>) 
                                        (r : EnvItem<ReadState>) (ilGen : ILGenerator) =
            
            c.Load()
            r.Load()
            let m = typeof<UnionCaseSerializationHelper>.GetMethod("ReadTag", allMembers)
            ilGen.EmitCall(OpCodes.Call, m, null)
#endif