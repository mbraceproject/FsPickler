namespace Nessos.FsPickler

#if EMIT_IL

    open System
    open System.Reflection
    open System.Runtime.Serialization

    open System.Reflection.Emit

    open Nessos.FsPickler.Reflection
    open Nessos.FsPickler.Emit

    module internal PicklerEmit =
    
        [<AutoOpen>]
        module private ReflectedPicklerAPI =

            let typeFromHandle = typeof<Type>.GetMethod("GetTypeFromHandle")
            let writerCtx = typeof<WriteState>.GetProperty("StreamingContext").GetGetMethod(true)
            let readerCtx = typeof<ReadState>.GetProperty("StreamingContext").GetGetMethod(true)
            let objInitializer = typeof<FormatterServices>.GetMethod("GetUninitializedObject", BindingFlags.Public ||| BindingFlags.Static)
            let deserializationCallBack = typeof<IDeserializationCallback>.GetMethod("OnDeserialization")
            let picklerT = typedefof<Pickler<_>>
            let getTypedPickler (t : Type) = picklerT.MakeGenericType [|t|]
            let getPicklerWriter (t : Type) = getTypedPickler(t).GetMethod("Write")
            let getPicklerReader (t : Type) = getTypedPickler(t).GetMethod("Read")
            let bw = typeof<WriteState>.GetProperty("Formatter", allMembers).GetGetMethod(true)
            let br = typeof<ReadState>.GetProperty("Formatter", allMembers).GetGetMethod(true)
            let bwIntWriter = typeof<IPickleFormatWriter>.GetMethod("WriteInt32")
            let brIntReader = typeof<IPickleFormatReader>.GetMethod("ReadInt32")
            let bwStringWriter = typeof<IPickleFormatWriter>.GetMethod("WriteString")
            let brStringReader = typeof<IPickleFormatReader>.GetMethod("ReadString")

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

        /// emits IL that serializes a collection of fields
        let emitSerializeFields (fields : FieldInfo [])
                                (tags : string [])
                                (writer : EnvItem<WriteState>) 
                                (picklers : EnvItem<Pickler []>) 
                                (parent : EnvItem<'T>) (ilGen : ILGenerator) =

            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
                // load typed pickler to the stack
                emitLoadPickler picklers f.FieldType i ilGen
                // load writer to the stack
                writer.Load ()
                // load field name
                ilGen.Emit(OpCodes.Ldstr, tags.[i])
                // load field value to the stack
                parent.Load ()
                ilGen.Emit(OpCodes.Ldfld, f)
                // perform serialization
                emitSerialize f.FieldType ilGen

        /// deserialize a collection of fields and store to parent object
        let emitDeserializeFields (fields : FieldInfo [])
                                    (tags : string [])
                                    (reader : EnvItem<ReadState>)
                                    (picklers : EnvItem<Pickler []>)
                                    (parent : EnvItem<'T>) (ilGen : ILGenerator) =

            let isStruct = fields.Length > 0 && fields.[0].DeclaringType.IsValueType

            for i = 0 to fields.Length - 1 do
                let f = fields.[i]
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

        /// serialize properties to the underlying stack
        let emitSerializeProperties (properties : PropertyInfo [])
                                    (tags : string [])
                                    (writer : EnvItem<WriteState>)
                                    (picklers : EnvItem<Pickler []>)
                                    (parent : EnvItem<'T>) (ilGen : ILGenerator) =

            for i = 0 to properties.Length - 1 do
                let p = properties.[i]
                let m = p.GetGetMethod(true)
                // load typed pickler to the stack
                emitLoadPickler picklers m.ReturnType i ilGen
                // load writer to the stack
                writer.Load ()
                // load tag
                ilGen.Emit(OpCodes.Ldstr, tags.[i])
                // load property value to the stack
                parent.Load ()
                ilGen.EmitCall(OpCodes.Call, m, null)
                // perform serialization
                emitSerialize m.ReturnType ilGen

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


        /// push an uninitialized object of type 't' to the stack
        let emitObjectInitializer (t : Type) (ilGen : ILGenerator) =
            // reify type instance
            ilGen.Emit(OpCodes.Ldtoken, t)
            ilGen.EmitCall(OpCodes.Call, typeFromHandle, null)
            // call the object initializer
            ilGen.EmitCall(OpCodes.Call, objInitializer, null)
            // unbox value
            if t.IsValueType then
                ilGen.Emit(OpCodes.Unbox_Any, t)
            else
                ilGen.Emit(OpCodes.Castclass, t)

        /// calls a predefined collection of serialization methods on given value
        let emitSerializationMethodCalls (methods : MethodInfo []) (wOr : Choice<EnvItem<WriteState>, EnvItem<ReadState>>)
                                            (value : EnvItem<'T>) (ilGen : ILGenerator) =

            if methods.Length = 0 then () else

            // evaluate the streaming context
            let ctx = EnvItem<StreamingContext>(ilGen)

            match wOr with
            | Choice1Of2 w -> w.Load () ; ilGen.EmitCall(OpCodes.Call, writerCtx, null)
            | Choice2Of2 r -> r.Load () ; ilGen.EmitCall(OpCodes.Call, readerCtx, null)

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

        /// wraps call to ISerializable constructor in a dynamic method
        let wrapISerializableConstructor<'T> (ctor : ConstructorInfo) =
            DynamicMethod.compileFunc2<SerializationInfo, StreamingContext, 'T> "ISerializableCtor" (fun sI sC ilGen ->
                sI.Load ()
                sC.Load ()

                ilGen.Emit(OpCodes.Newobj, ctor)
                ilGen.Emit OpCodes.Ret
            )

        /// writes an integer
        let writeInt (writer : EnvItem<WriteState>) (tag : string) (n : EnvItem<int>) (ilGen : ILGenerator) =
            writer.Load ()
            ilGen.EmitCall(OpCodes.Call, bw, null) // load BinaryWriter
            ilGen.Emit(OpCodes.Ldstr, tag) // load tag
            n.Load () // load value
            ilGen.EmitCall(OpCodes.Callvirt, bwIntWriter, null) // perform write

        /// reads an integer and pushes to stack
        let readInt (reader : EnvItem<ReadState>) (tag : string) (ilGen : ILGenerator) =
            reader.Load ()
            ilGen.EmitCall(OpCodes.Call, br, null) // load BinaryReader
            ilGen.Emit(OpCodes.Ldstr, tag) // load tag
            ilGen.EmitCall(OpCodes.Callvirt, brIntReader, null) // perform read, push to stack

        /// writes a string
        let writeString (writer : EnvItem<WriteState>) (tag : string) (value : string) (ilGen : ILGenerator) =
            writer.Load ()
            ilGen.EmitCall(OpCodes.Call, bw, null) // load BinaryWriter
            ilGen.Emit(OpCodes.Ldstr, tag) // load tag
            ilGen.Emit(OpCodes.Ldstr, value) // load tag
            ilGen.EmitCall(OpCodes.Callvirt, bwStringWriter, null) // perform write

        /// reads a string and pushes to stack
        let readString (reader : EnvItem<ReadState>) (tag : string) (ilGen : ILGenerator) =
            reader.Load ()
            ilGen.EmitCall(OpCodes.Call, br, null) // load BinaryReader
            ilGen.Emit(OpCodes.Ldstr, tag) // load tag
            ilGen.EmitCall(OpCodes.Callvirt, brStringReader, null) // perform read, push to stack
        
#endif