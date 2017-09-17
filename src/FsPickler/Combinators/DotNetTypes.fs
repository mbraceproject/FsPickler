namespace MBrace.FsPickler

open System
open System.Reflection

open MBrace.FsPickler

/// abstract type pickler factory

type internal AbstractPickler =
    static member Create<'T> () =
        let writer _ _ = invalidOp <| sprintf "internal error: attempting to consume abstract pickler '%O'." typeof<'T>
        let reader _ = invalidOp <| sprintf "internal error: attempting to consume abstract pickler '%O'." typeof<'T>
        let cloner _ _ = invalidOp <| sprintf "internal error: attempting to consume abstract pickler '%O'." typeof<'T>
        let accepter _ _ = invalidOp <| sprintf "internal error: attempting to consume abstract pickler '%O'." typeof<'T>

        CompositePickler.Create<'T>(reader, writer, cloner, accepter, PicklerInfo.FieldSerialization, cacheByRef = true, useWithSubtypes = false)

/// Enum types combinator

type internal EnumPickler =

    static member Create<'Enum, 'Underlying when 'Enum : enum<'Underlying>> (resolver : IPicklerResolver) =
        let pickler = resolver.Resolve<'Underlying> ()

        let writer (w : WriteState) (tag : string) (x : 'Enum) =
            if w.Formatter.UseNamedEnumSerialization then
                let value = x.ToString()
                w.Formatter.WriteString tag value
            else
                let value = Microsoft.FSharp.Core.LanguagePrimitives.EnumToValue<'Enum, 'Underlying> x
                pickler.Write w tag value

        let reader (r : ReadState) (tag : string) =
            if r.Formatter.UseNamedEnumSerialization then
                let value = r.Formatter.ReadString tag
                Enum.Parse(typeof<'Enum>, value) :?> 'Enum
            else
                let value = pickler.Read r tag
                Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<'Underlying, 'Enum> value

        let cloner (_ : CloneState) (x : 'Enum) = x

        CompositePickler.Create(reader, writer, cloner, ignore2, PicklerInfo.FieldSerialization, cacheByRef = false, useWithSubtypes = false, bypass = true)


/// Nullable Pickler combinator

type internal NullablePickler =

    static member Create<'T when 
                            'T : (new : unit -> 'T) and 
                            'T : struct and 
                            'T :> ValueType> (pickler : Pickler<'T>) =

        let writer (w : WriteState) (_ : string) (x : Nullable<'T>) =
            pickler.Write w "value" x.Value

        let reader (r : ReadState) (_ : string) =
            let value = pickler.Read r "value"
            Nullable<'T>(value)

        let cloner (c : CloneState) (x : Nullable<'T>) =
            if x.HasValue then Nullable<'T>(pickler.Clone c x.Value)
            else x

        let accepter (v : VisitState) (x : Nullable<'T>) =
            if x.HasValue then pickler.Accept v x.Value

        CompositePickler.Create(reader, writer, cloner, accepter, PicklerInfo.FieldSerialization)

    static member Create<'T when 
                            'T : (new : unit -> 'T) and 
                            'T : struct and 
                            'T :> ValueType> (resolver : IPicklerResolver) =

        NullablePickler.Create<'T>(resolver.Resolve())

/// Delegate Pickler combinator

type internal DelegatePickler =

    static member Create<'Delegate when 'Delegate :> Delegate> (resolver : IPicklerResolver) =
        let objPickler = resolver.Resolve<obj> ()
        let memberInfoPickler = resolver.Resolve<MethodInfo> ()
        let delePickler = resolver.Resolve<System.Delegate> ()

        let writer (w : WriteState) (_ : string) (dele : 'Delegate) =
            if w.DisableSubtypeResolution then
                let msg = sprintf "Subtype serialization has been disabled. Value %O is of delegate type %O." dele typeof<'Delegate>
                raise <| new FsPicklerException(msg)

            match dele.GetInvocationList() with
            | [| _ |] ->
                w.Formatter.WriteBoolean "isLinked" false
                memberInfoPickler.Write w "method" dele.Method
                if not dele.Method.IsStatic then objPickler.Write w "target" dele.Target
            | deleList ->
                w.Formatter.WriteBoolean "isLinked" true
                w.Formatter.WriteInt32 "length" deleList.Length
                for i = 0 to deleList.Length - 1 do
                    delePickler.Write w "linked" deleList.[i]

        let reader (r : ReadState) (_ : string) =
            if r.DisableSubtypeResolution then
                let msg = sprintf "Subtype deserialization has been disabled. Attempting to deserialize delegate type %O." typeof<'Delegate>
                raise <| new FsPicklerException(msg)

            if not <| r.Formatter.ReadBoolean "isLinked" then
                let meth = memberInfoPickler.Read r "method"
                if not meth.IsStatic then
                    let target = objPickler.Read r "target"
                    Delegate.CreateDelegate(typeof<'Delegate>, target, meth, throwOnBindFailure = true) |> fastUnbox<'Delegate>
                else
                    Delegate.CreateDelegate(typeof<'Delegate>, meth, throwOnBindFailure = true) |> fastUnbox<'Delegate>
            else
                let n = r.Formatter.ReadInt32 "length"
                let deleList = Array.zeroCreate<System.Delegate> n
                for i = 0 to n - 1 do deleList.[i] <- delePickler.Read r "linked"
                Delegate.Combine deleList |> fastUnbox<'Delegate>

        let cloner (c : CloneState) (dele : 'Delegate) =
            match dele.GetInvocationList() with
            | [| _ |] ->
                if dele.Method.IsStatic then dele.Clone() |> fastUnbox<'Delegate>
                else
                    let target' = objPickler.Clone c dele.Target
                    Delegate.CreateDelegate(typeof<'Delegate>, target', dele.Method, throwOnBindFailure = true) |> fastUnbox<'Delegate>

            | deleList ->
                let deleList' = Array.zeroCreate<System.Delegate> deleList.Length
                for i = 0 to deleList.Length - 1 do deleList'.[i] <- delePickler.Clone c deleList.[i]
                Delegate.Combine deleList |> fastUnbox<'Delegate>

        let accepter (v : VisitState) (dele : 'Delegate) =
            match dele.GetInvocationList () with
            | [| _ |] ->
                memberInfoPickler.Accept v dele.Method

                if not <| dele.Method.IsStatic then
                    objPickler.Accept v dele.Target

            | deleList ->
                for i = 0 to deleList.Length - 1 do delePickler.Accept v deleList.[i]

        CompositePickler.Create(reader, writer, cloner, accepter, PicklerInfo.Delegate)