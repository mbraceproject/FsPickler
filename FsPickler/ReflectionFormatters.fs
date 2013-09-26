module internal FsPickler.ReflectionFormatters

    open System
    open System.Reflection
    open System.Runtime.Serialization
#if EMIT_IL
    open System.Linq.Expressions
#endif

    open Microsoft.FSharp.Reflection

    open FsPickler
    open FsPickler.Utils
    open FsPickler.FormatterUtils

    //
    //  reflection - based serializers
    //

    let isUnSupportedType (t : Type) =
        t.IsPointer
        || t.IsByRef
        || t.IsCOMObject
        || t.IsImport
        || t.IsMarshalByRef
        || t.IsPrimitive // supported primitives are already stored in the formatter cache


    // creates a placeholder formatter instance

    type UninitializedFormatter =
        static member Create<'T>() = new Formatter<'T>()
        static member CreateUntyped (t : Type) =
            if isUnSupportedType t then raise <| new NonSerializableTypeException(t)

            let m =
                typeof<UninitializedFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            try m.Invoke(null, null) :?> Formatter
            with :? TargetInvocationException as e -> raise e.InnerException

    // creates a formatter for abstract types

    type AbstractFormatter =
        static member Create<'T> () =
            let writer = fun _ _ -> invalidOp <| sprintf "Attempting to use formatter for abstract type '%O'." typeof<'T>
            let reader = fun _ -> invalidOp <| sprintf "Attempting to use formatter for abstract type '%O'." typeof<'T>

            new Formatter<'T>(reader, writer, FormatterInfo.ReflectionDerived, true, false)

        static member CreateUntyped(t : Type) =
            let m = 
                typeof<AbstractFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            m.GuardedInvoke(null, null) :?> Formatter

    // formatter rules for enum types

    type EnumFormatter =
        static member Create<'Enum, 'Underlying when 'Enum : enum<'Underlying>> (resolver : IFormatterResolver) =
            let fmt = resolver.Resolve<'Underlying> ()
            let writer_func = fmt.Write
            let reader_func = fmt.Read

            let writer (w : Writer) (x : 'Enum) =
                let value = Microsoft.FSharp.Core.LanguagePrimitives.EnumToValue<'Enum, 'Underlying> x
                writer_func w value

            let reader (r : Reader) =
                let value = reader_func r
                Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<'Underlying, 'Enum> value

            new Formatter<'Enum>(reader, writer, FormatterInfo.ReflectionDerived, false, false)

        static member CreateUntyped(enum : Type, resolver : IFormatterResolver) =
            let underlying = enum.GetEnumUnderlyingType()
            // reflection call typed method
            let m = 
                typeof<EnumFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| enum ; underlying |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Formatter

    // formatter builder for struct types

    type StructFormatter =
        static member Create<'T when 'T : struct>(resolver : IFormatterResolver) =
            let fields = typeof<'T>.GetFields(allFields)
            if fields |> Array.exists(fun f -> f.IsInitOnly) then
                raise <| new NonSerializableTypeException(typeof<'T>, "type is marked with read-only instance fields.")
            
            let formatters = fields |> Array.map (fun f -> resolver.Resolve f.FieldType)

#if EMIT_IL
            
            let writer =
                if fields.Length = 0 then (fun _ _ -> ())
                else
                    let action =
                        Expression.compileAction2<Writer, 'T>(fun writer instance ->
                            Expression.zipWriteFields fields formatters writer instance |> Expression.Block :> _)

                    fun w t -> action.Invoke(w,t)

            let reader =
                Expression.compileFunc1<Reader, 'T>(fun reader ->

                    let instance = Expression.Variable(typeof<'T>, "instance")

                    let body =
                        seq {
                            yield Expression.Assign(instance, Expression.initializeObject<'T> ()) :> Expression

                            yield! Expression.zipReadFields fields formatters reader instance

                            yield instance :> _
                        }

                    Expression.Block([| instance |], body) :> _).Invoke

#else
            let writer (w : Writer) (t : 'T) =
                for i = 0 to fields.Length - 1 do
                    let o = fields.[i].GetValue(t)
                    formatters.[i].ManagedWrite w o

            let reader (r : Reader) =
                let t = FormatterServices.GetUninitializedObject(typeof<'T>)
                for i = 0 to fields.Length - 1 do
                    let o = formatters.[i].ManagedRead r
                    fields.[i].SetValue(t, o)
                
                fastUnbox<'T> t
#endif

            new Formatter<'T>(reader, writer, FormatterInfo.ReflectionDerived, false, false)

        static member CreateUntyped(t : Type, resolver : IFormatterResolver) =
            let m = 
                typeof<StructFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Formatter
                    

    // reflection-based serialization rules for reference types

    type ClassFormatter =
        static member Create<'T when 'T : not struct>(resolver : IFormatterResolver) =

            let fields = 
                typeof<'T>.GetFields(allFields) 
                |> Array.filter (fun f -> not (containsAttr<NonSerializedAttribute> f))

            if fields |> Array.exists(fun f -> f.IsInitOnly) then
                raise <| new NonSerializableTypeException(typeof<'T>, "type is marked with read-only instance fields.") 

            let formatters = fields |> Array.map (fun f -> resolver.Resolve f.FieldType)

            let isDeserializationCallback = typeof<IDeserializationCallback>.IsAssignableFrom typeof<'T>

            let allMethods = typeof<'T>.GetMethods(allMembers)
            let onSerializing = allMethods |> getSerializationMethods<OnSerializingAttribute>
            let onSerialized = allMethods |> getSerializationMethods<OnSerializedAttribute>
            let onDeserializing = allMethods |> getSerializationMethods<OnDeserializingAttribute>
            let onDeserialized = allMethods |> getSerializationMethods<OnDeserializedAttribute>

#if EMIT_IL

            let writer =
                if onSerializing.Length = 0 && fields.Length = 0 && onSerialized.Length = 0 then
                    fun _ _ -> ()
                else
                    let writer =
                        Expression.compileAction2<Writer, 'T>(fun writer instance ->
                            seq {
                                yield! Expression.runSerializationActions onSerializing writer instance

                                yield! Expression.zipWriteFields fields formatters writer instance

                                yield! Expression.runSerializationActions onSerialized writer instance

                            } |> Expression.Block :> Expression)

                    fun w t -> writer.Invoke(w,t)

            let reader =
                Expression.compileFunc1<Reader, 'T>(fun reader ->

                    let instance = Expression.Variable(typeof<'T>, "instance")

                    let body =
                        seq {
                            yield Expression.Assign(instance, Expression.initializeObject<'T> ()) :> Expression
                
                            yield Expression.earlyRegisterObject reader instance

                            yield! Expression.runDeserializationActions onDeserializing reader instance

                            yield! Expression.zipReadFields fields formatters reader instance

                            yield! Expression.runDeserializationActions onDeserialized reader instance

                            if isDeserializationCallback then
                                yield Expression.runDeserializationCallback instance

                            yield instance :> _
                        } 

                    Expression.Block([| instance |], body) :> Expression).Invoke

#else
            let inline run (ms : MethodInfo []) (x : obj) w =
                for i = 0 to ms.Length - 1 do 
                    ms.[i].Invoke(x, [| getStreamingContext w :> obj |]) |> ignore

            let writer (w : Writer) (t : 'T) =
                run onSerializing t w

                for i = 0 to fields.Length - 1 do
                    let o = fields.[i].GetValue(t)
                    formatters.[i].ManagedWrite w o

                run onSerialized t w

            let reader (r : Reader) =
                let t = FormatterServices.GetUninitializedObject(typeof<'T>) |> fastUnbox<'T>
                do r.EarlyRegisterObject t
                run onDeserializing t r

                for i = 0 to fields.Length - 1 do
                    let o = formatters.[i].ManagedRead r
                    fields.[i].SetValue(t, o)

                run onDeserialized t r
                if isDeserializationCallback then (fastUnbox<IDeserializationCallback> t).OnDeserialization null
                t
#endif

            new Formatter<'T>(reader, writer, FormatterInfo.ReflectionDerived, cacheObj = true, useWithSubtypes = false)


        static member CreateUntyped(t : Type, resolver : IFormatterResolver) =
            let m =
                typeof<ClassFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Formatter


    type DelegateFormatter =
        static member Create<'Delegate when 'Delegate :> Delegate> (resolver : IFormatterResolver) =
            let memberInfoFormatter = resolver.Resolve<MethodInfo> ()
            let writer (w : Writer) (dele : 'Delegate) =
                match dele.GetInvocationList() with
                | [| _ |] ->
                    w.BW.Write true
                    w.Write(memberInfoFormatter, dele.Method)
                    if not dele.Method.IsStatic then w.Write<obj> dele.Target
                | deleList ->
                    w.BW.Write false
                    w.BW.Write deleList.Length
                    for i = 0 to deleList.Length - 1 do
                        w.Write<System.Delegate> (deleList.[i])

            let reader (r : Reader) =
                if r.BR.ReadBoolean() then
                    let meth = r.Read memberInfoFormatter
                    if not meth.IsStatic then
                        let target = r.Read<obj> ()
                        Delegate.CreateDelegate(typeof<'Delegate>, target, meth, throwOnBindFailure = true) |> fastUnbox<'Delegate>
                    else
                        Delegate.CreateDelegate(typeof<'Delegate>, meth, throwOnBindFailure = true) |> fastUnbox<'Delegate>
                else
                    let n = r.BR.ReadInt32()
                    let deleList = Array.zeroCreate<System.Delegate> n
                    for i = 0 to n - 1 do deleList.[i] <- r.Read<System.Delegate> ()
                    Delegate.Combine deleList |> fastUnbox<'Delegate>

            new Formatter<'Delegate>(reader, writer, FormatterInfo.Custom, true, false)

        static member CreateUntyped(t : Type, resolver : IFormatterResolver) =
            let m =
                typeof<DelegateFormatter>
                    .GetMethod("Create", BindingFlags.NonPublic ||| BindingFlags.Static)
                    .MakeGenericMethod [| t |]

            m.GuardedInvoke(null, [| resolver :> obj |]) :?> Formatter