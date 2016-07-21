namespace MBrace.FsPickler

open MBrace.FsPickler.SequenceUtils

// F# list pickler combinator

type internal ListPickler =

    static member Create (ep : Pickler<'T>) =

        let writer (w : WriteState) (tag : string) (list : 'T list) =

            let formatter = w.Formatter

            if ep.Kind <= Kind.Enum && formatter.IsPrimitiveArraySerializationSupported then
                // use primitive serialization support by format to bulk-serialize array
                // serialize as object and prefix with length
                formatter.BeginWriteObject tag ObjectFlags.None

                let arr = List.toArray list
                formatter.WriteInt32 "length" arr.Length
                formatter.WritePrimitiveArray "data" arr

                formatter.EndWriteObject ()

            elif formatter.PreferLengthPrefixInSequences then
                // format specifies that it prefers length prefixing
                // serialize as object and prefix list length
                formatter.BeginWriteObject tag ObjectFlags.None
                formatter.WriteInt32 "length" list.Length
                formatter.BeginWriteObject "list" ObjectFlags.IsSequenceHeader

                let rec writeL (ts : 'T list) =
                    match ts with
                    | t :: tl -> ep.Write w elemTag t ; writeL tl
                    | [] -> ()

                do writeL list

                formatter.EndWriteObject () // end sequence
                formatter.EndWriteObject () // end encapsulating object

            else
                // serialize list without length prefixing ; used by e.g. FsPickler.Json
                formatter.BeginWriteObject tag ObjectFlags.IsSequenceHeader

                let rec writeL (ts : 'T list) =
                    match ts with
                    | t :: tl -> 
                        formatter.WriteNextSequenceElement true
                        ep.Write w elemTag t
                        writeL tl

                    | [] -> formatter.WriteNextSequenceElement false
                    
                writeL list
                formatter.EndWriteObject ()


        let reader (r : ReadState) (_ : string) =
            let formatter = r.Formatter

            if ep.Kind <= Kind.Enum && formatter.IsPrimitiveArraySerializationSupported then
                // use primitive serialization support by format to bulk-deserialize array
                let length = r.Formatter.ReadInt32 "length"
                let array = Array.zeroCreate<'T> length
                formatter.ReadPrimitiveArray "data" array
                Array.toList array

            elif formatter.PreferLengthPrefixInSequences then
                // format specifies that it prefers length prefixing
                let length = formatter.ReadInt32 "length"
                let array = Array.zeroCreate<'T> length

                do beginReadSequence formatter "list"
                for i = 0 to length - 1 do 
                    array.[i] <- ep.Read r elemTag

                formatter.EndReadObject ()
                Array.toList array
            else
                // deserialize list without length prefixing ; used by e.g. FsPickler.Json
                let ra = new ResizeArray<'T> ()

                while formatter.ReadNextSequenceElement() do
                    ra.Add <| ep.Read r elemTag

                Seq.toList ra

        let cloner (c : CloneState) (list : 'T list) =
            list |> List.map (ep.Clone c)

        let accepter (v : VisitState) (list : 'T list) =
            let rec aux = function [] -> () | t :: rest -> ep.Accept v t ; aux rest
            aux list

        CompositePickler.Create<_>(reader, writer, cloner, accepter, PicklerInfo.FSharpValue, useWithSubtypes = true, skipHeaderWrite = true)

    static member Create<'T>(resolver : IPicklerResolver) =
        let ep = resolver.Resolve<'T> ()
        ListPickler.Create<'T> ep