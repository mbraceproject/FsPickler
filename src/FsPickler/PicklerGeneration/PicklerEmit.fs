namespace MBrace.FsPickler

#if EMIT_IL

open System
open System.Reflection
open System.Runtime.Serialization

open System.Reflection.Emit

open MBrace.FsPickler.Reflection
open MBrace.FsPickler.Emit

module internal PicklerEmit =
    
    [<AutoOpen>]
    module private ReflectedPicklerAPI =

        let typeFromHandle = typeof<Type>.GetMethod("GetTypeFromHandle")
        let writerCtx = typeof<WriteState>.GetProperty("StreamingContext").GetGetMethod(true)
        let readerCtx = typeof<ReadState>.GetProperty("StreamingContext").GetGetMethod(true)
        let clonerCtx  = typeof<CloneState>.GetProperty("StreamingContext").GetGetMethod(true)
        let visitCtx  = typeof<VisitState>.GetProperty("StreamingContext").GetGetMethod(true)
        let objInitializer = typeof<FormatterServices>.GetMethod("GetUninitializedObject", BindingFlags.Public ||| BindingFlags.Static)
        let deserializationCallBack = typeof<IDeserializationCallback>.GetMethod("OnDeserialization")
        let getRealObject = typeof<IObjectReference>.GetMethod("GetRealObject")
        let picklerT = typedefof<Pickler<_>>
        let getTypedPickler (t : Type) = picklerT.MakeGenericType [|t|]
        let getPicklerWriter (t : Type) = getTypedPickler(t).GetMethod("Write")
        let getPicklerReader (t : Type) = getTypedPickler(t).GetMethod("Read")
        let getPicklerCloner (t : Type) = getTypedPickler(t).GetMethod("Clone")
        let getPicklerAccepter (t : Type) = getTypedPickler(t).GetMethod("Accept")
        let getWriteFormatter = typeof<WriteState>.GetProperty("Formatter", allMembers).GetGetMethod(true)
        let getReadFormatter = typeof<ReadState>.GetProperty("Formatter", allMembers).GetGetMethod(true)
        let stringWriter = typeof<IPickleFormatWriter>.GetMethod("WriteString")
        let stringReader = typeof<IPickleFormatReader>.GetMethod("ReadString")

    /// emits typed pickler from array of untyped picklers
    let emitLoadPickler (picklers : EnvItem<Pickler []>) (t : Type) (idx : int) (ilGen : ILGenerator) =
        let picklerType = getTypedPickler t
        picklers.Load ()
        ilGen.Emit(OpCodes.Ldc_I4, idx)
        ilGen.Emit(OpCodes.Ldelem_Ref)
        ilGen.Emit(OpCodes.Castclass, picklerType)

    /// emit IL that serializes last object in stack
    /// last 4 items in stack: Pickler<'T>; WriteState ; string ; 'T
    let emitSerialize (t : Type) (ilGen : ILGenerator) =
        let writer = getPicklerWriter t
        ilGen.EmitCall(OpCodes.Callvirt, writer, null)

    /// emit IL that deserializes an object
    /// last 3 items in stack: Pickler<'T> ; ReadState ; string
    let emitDeserialize (t : Type) (ilGen : ILGenerator) =
        let reader = getPicklerReader t
        ilGen.EmitCall(OpCodes.Callvirt, reader, null)

    /// emit IL that clones an object
    /// last 3 items in stack: Pickler<'T> ; CloneState ; 'T
    let emitClone (t : Type) (ilGen : ILGenerator) =
        let cloner = getPicklerCloner t
        ilGen.EmitCall(OpCodes.Callvirt, cloner, null)

    /// emit IL that accepts a visitor
    /// last 3 items in stack: Pickler<'T> ; VisitState ; 'T
    let emitAccept (t : Type) (ilGen : ILGenerator) =
        let accepter = getPicklerAccepter t
        ilGen.EmitCall(OpCodes.Callvirt, accepter, null)

    /// emits code for serializing field or property values
    let emitSerializeMembers (members : 'MemberInfo [] when 'MemberInfo :> MemberInfo)
                                (tags : string []) 
                                (writer : EnvItem<WriteState>) 
                                (picklers : EnvItem<Pickler []>) 
                                (parent : EnvItem<'T>) (ilGen : ILGenerator) =

        let isStruct = typeof<'T>.IsValueType

        for i = 0 to members.Length - 1 do
            match members.[i] :> MemberInfo with
            | :? FieldInfo as f ->
                // load typed pickler to the stack
                emitLoadPickler picklers f.FieldType i ilGen
                // load writer to the stack
                writer.Load ()
                // load field name
                ilGen.Emit(OpCodes.Ldstr, tags.[i])
                // load field value to the stack
                if isStruct then parent.LoadAddress() else parent.Load()
                ilGen.Emit(OpCodes.Ldfld, f)
                // perform serialization
                emitSerialize f.FieldType ilGen

            | :? PropertyInfo as p ->
                let m = p.GetGetMethod(true)
                // load typed pickler to the stack
                emitLoadPickler picklers m.ReturnType i ilGen
                // load writer to the stack
                writer.Load ()
                // load tag
                ilGen.Emit(OpCodes.Ldstr, tags.[i])
                // load property value to the stack
                if isStruct then parent.LoadAddress () else parent.Load()
                ilGen.EmitCall(OpCodes.Call, m, null)
                // perform serialization
                emitSerialize m.ReturnType ilGen

            | _ -> invalidOp "invalid memberInfo instance."

    /// emits code for deserializing field or property values
    let emitDeserializeMembers (members : 'MemberInfo [] when 'MemberInfo :> MemberInfo)
                                (tags : string [])
                                (reader : EnvItem<ReadState>)
                                (picklers : EnvItem<Pickler []>)
                                (parent : EnvItem<'T>) (ilGen : ILGenerator) =

        let isStruct = typeof<'T>.IsValueType

        for i = 0 to members.Length - 1 do
            match members.[i] :> MemberInfo with
            | :? FieldInfo as f ->
                // load parent object to the stack
                if isStruct then parent.LoadAddress ()
                else
                    parent.Load ()

                // load typed pickler to the stack
                emitLoadPickler picklers f.FieldType i ilGen
                // load reader to the stack
                reader.Load ()
                // load field name
                ilGen.Emit(OpCodes.Ldstr, tags.[i])
                // deserialize and load to the stack
                emitDeserialize f.FieldType ilGen
                // assign value to the field
                ilGen.Emit(OpCodes.Stfld, f)

            | :? PropertyInfo as p ->
                let m = p.GetSetMethod(true)
                // load parent object to the stack
                if isStruct then parent.LoadAddress ()
                else
                    parent.Load ()

                // load typed pickler to the stack
                emitLoadPickler picklers p.PropertyType i ilGen
                // load reader to the stack
                reader.Load ()
                // load field name
                ilGen.Emit(OpCodes.Ldstr, tags.[i])
                // deserialize and load to the stack
                emitDeserialize p.PropertyType ilGen
                // call the property setter
                ilGen.EmitCall(OpCodes.Call, m, null)

            | _ -> invalidOp "invalid memberInfo instance."

    /// emits code for deserializing field or property values
    let emitCloneMembers (members : 'MemberInfo [] when 'MemberInfo :> MemberInfo)
                            (cloner : EnvItem<CloneState>)
                            (picklers : EnvItem<Pickler []>)
                            (source : EnvItem<'T>) (target : EnvItem<'T>) (ilGen : ILGenerator) =

        let isStruct = typeof<'T>.IsValueType

        for i = 0 to members.Length - 1 do
            match members.[i] :> MemberInfo with
            | :? FieldInfo as f ->
                // load target object to the stack
                if isStruct then target.LoadAddress ()
                else
                    target.Load ()

                // load typed pickler to the stack
                emitLoadPickler picklers f.FieldType i ilGen
                // load cloner state to the stack
                cloner.Load ()
                // load source object to the stack
                if isStruct then source.LoadAddress ()
                else
                    source.Load ()

                // load field value
                ilGen.Emit(OpCodes.Ldfld, f)
                // clone value
                emitClone f.FieldType ilGen
                // assign value to the field
                ilGen.Emit(OpCodes.Stfld, f)

            | :? PropertyInfo as p ->
                let g = p.GetGetMethod(true)
                let s = p.GetSetMethod(true)
                // load target object to the stack
                if isStruct then target.LoadAddress ()
                else
                    target.Load ()

                // load typed pickler to the stack
                emitLoadPickler picklers p.PropertyType i ilGen
                // load cloner state to the stack
                cloner.Load ()
                // load source object to the stack
                if isStruct then source.LoadAddress ()
                else
                    source.Load ()

                // load source value
                ilGen.EmitCall(OpCodes.Call, g, null)
                // clone value
                emitClone p.PropertyType ilGen
                // call the property setter
                ilGen.EmitCall(OpCodes.Call, s, null)

            | _ -> invalidOp "invalid memberInfo instance."

    /// emits code for accepting visitors for field or property values
    let emitAcceptMembers (members : 'MemberInfo [] when 'MemberInfo :> MemberInfo)
                            (visitor : EnvItem<VisitState>)
                            (picklers : EnvItem<Pickler []>)
                            (source : EnvItem<'T>) (ilGen : ILGenerator) =

        let isStruct = typeof<'T>.IsValueType

        for i = 0 to members.Length - 1 do
            match members.[i] :> MemberInfo with
            | :? FieldInfo as f ->
                // load typed pickler to the stack
                emitLoadPickler picklers f.FieldType i ilGen
                // load visitor state to the stack
                visitor.Load ()
                // load source object to the stack
                if isStruct then source.LoadAddress ()
                else
                    source.Load ()

                // load field value
                ilGen.Emit(OpCodes.Ldfld, f)
                // visit value
                emitAccept f.FieldType ilGen

            | :? PropertyInfo as p ->
                let g = p.GetGetMethod(true)

                // load typed pickler to the stack
                emitLoadPickler picklers p.PropertyType i ilGen
                // load cloner state to the stack
                visitor.Load ()
                // load source object to the stack
                if isStruct then source.LoadAddress ()
                else
                    source.Load ()

                // load source value
                ilGen.EmitCall(OpCodes.Call, g, null)
                // clone value
                emitAccept p.PropertyType ilGen

            | _ -> invalidOp "invalid memberInfo instance."

    /// deserialize fields, pass to factory method and push to stack
    let emitDeserializeAndConstruct (factory : Choice<MethodInfo,ConstructorInfo>)
                                    (fparams : Type [])
                                    (tags : string [])
                                    (reader : EnvItem<ReadState>)
                                    (picklers : EnvItem<Pickler []>)
                                    (ilGen : ILGenerator) =

        for i = 0 to fparams.Length - 1 do
            let t = fparams.[i]
            // load typed pickler to the stack
            emitLoadPickler picklers t i ilGen
            // load reader to the stack
            reader.Load ()
            // load tag
            ilGen.Emit(OpCodes.Ldstr, tags.[i])
            // perform deserialization and push to the stack
            emitDeserialize t ilGen

        // call factory method
        match factory with
        | Choice1Of2 f -> ilGen.EmitCall(OpCodes.Call, f, null)
        | Choice2Of2 c -> ilGen.Emit(OpCodes.Newobj, c)

    /// deserialize fields, pass to factory method and push to stack
    let emitCloneAndConstruct (factory : Choice<MethodInfo,ConstructorInfo>)
                                    (fields :  PropertyInfo [])
                                    (cloner : EnvItem<CloneState>)
                                    (picklers : EnvItem<Pickler []>)
                                    (source : EnvItem<'T>) (ilGen : ILGenerator) =

        let isStruct = typeof<'T>.IsValueType

        for i = 0 to fields.Length - 1 do
            let f = fields.[i]
            let getter = f.GetGetMethod(true)

            // load typed pickler to the stack
            emitLoadPickler picklers f.PropertyType i ilGen
            // load cloner state to the stack
            cloner.Load ()
            // load source object to the stack
            if isStruct then source.LoadAddress () else source.Load()
            // load source value
            ilGen.EmitCall(OpCodes.Call, getter, null)
            // clone value
            emitClone f.PropertyType ilGen

        // call factory method
        match factory with
        | Choice1Of2 f -> ilGen.EmitCall(OpCodes.Call, f, null)
        | Choice2Of2 c -> ilGen.Emit(OpCodes.Newobj, c)


    /// push an uninitialized object of type 't' to the stack
    let emitObjectInitializer (target : EnvItem<'T>) (ilGen : ILGenerator) =
        if typeof<'T>.IsValueType then
            target.LoadAddress()
            ilGen.Emit(OpCodes.Initobj, typeof<'T>)
        else
            // reify type instance
            ilGen.Emit(OpCodes.Ldtoken, typeof<'T>)
            ilGen.EmitCall(OpCodes.Call, typeFromHandle, null)
            // call the object initializer
            ilGen.EmitCall(OpCodes.Call, objInitializer, null)
            // unbox value
            ilGen.Emit(OpCodes.Castclass, typeof<'T>)
            target.Store()

    /// calls a predefined collection of serialization methods on given value
    let emitSerializationMethodCalls (methods : MethodInfo []) (wOr : Choice<EnvItem<WriteState>, EnvItem<ReadState>, EnvItem<CloneState>, EnvItem<VisitState>>)
                                        (value : EnvItem<'T>) (ilGen : ILGenerator) =

        if methods.Length = 0 then () else

        // evaluate the streaming context
        let ctx = EnvItem<StreamingContext>(ilGen)

        match wOr with
        | Choice1Of4 w -> w.Load () ; ilGen.EmitCall(OpCodes.Call, writerCtx, null)
        | Choice2Of4 r -> r.Load () ; ilGen.EmitCall(OpCodes.Call, readerCtx, null)
        | Choice3Of4 c -> c.Load () ; ilGen.EmitCall(OpCodes.Call, clonerCtx, null)
        | Choice4Of4 v -> v.Load () ; ilGen.EmitCall(OpCodes.Call, visitCtx, null)

        ctx.Store ()

        // call the methods
        for m in methods do
            value.Load ()
            ctx.Load ()
            ilGen.EmitCall(OpCodes.Call, m, null)

    /// emit a call to the 'OnDeserialization' method on given value
    let emitDeserializationCallback (value : EnvItem<'T>) (ilGen : ILGenerator) =
        value.Load ()
        ilGen.Emit(OpCodes.Castclass, typeof<IDeserializationCallback>)
        ilGen.Emit OpCodes.Ldnull
        ilGen.EmitCall(OpCodes.Callvirt, deserializationCallBack, null)

    /// emit a call to the 'OnDeserialization' method on given value
    let emitObjectReferenceResolver<'T, 'S> (value : EnvItem<'T>) (state : Choice<EnvItem<ReadState>, EnvItem<CloneState>>) (ilGen : ILGenerator) =
        value.Load ()
        ilGen.Emit(OpCodes.Castclass, typeof<IObjectReference>)
        match state with
        | Choice1Of2 reader -> reader.Load() ; ilGen.EmitCall(OpCodes.Call, readerCtx, null)
        | Choice2Of2 cloner -> cloner.Load() ; ilGen.EmitCall(OpCodes.Call, clonerCtx, null)
        ilGen.EmitCall(OpCodes.Callvirt, getRealObject, null)
        ilGen.Emit(OpCodes.Castclass, typeof<'S>)

    /// wraps call to ISerializable constructor in a dynamic method
    let wrapISerializableConstructor<'T> (ctor : ConstructorInfo) =
        DynamicMethod.compileFunc2<SerializationInfo, StreamingContext, 'T> "ISerializableCtor" (fun sI sC ilGen ->
            sI.Load ()
            sC.Load ()

            ilGen.Emit(OpCodes.Newobj, ctor)
            ilGen.Emit OpCodes.Ret
        )

    /// writes a string
    let writeString (writer : EnvItem<WriteState>) (tag : string) (value : string) (ilGen : ILGenerator) =
        writer.Load ()
        ilGen.EmitCall(OpCodes.Call, getWriteFormatter, null) // load write formatter
        ilGen.Emit(OpCodes.Ldstr, tag) // load tag
        ilGen.Emit(OpCodes.Ldstr, value) // load value
        ilGen.EmitCall(OpCodes.Callvirt, stringWriter, null) // perform write

    /// reads a string and pushes to stack
    let readString (reader : EnvItem<ReadState>) (tag : string) (ilGen : ILGenerator) =
        reader.Load ()
        ilGen.EmitCall(OpCodes.Call, getReadFormatter, null) // load read formatter
        ilGen.Emit(OpCodes.Ldstr, tag) // load tag
        ilGen.EmitCall(OpCodes.Callvirt, stringReader, null) // perform read, push result to stack
        
#endif