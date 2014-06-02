namespace Nessos.FsPickler

#if EMIT_IL

    module internal Emit =

        open System
        open System.Reflection
        open System.Reflection.Emit
        open System.Runtime.Serialization

        /// a descriptor for local variables or parameters in emitted IL

        type EnvItem<'T>(ilGen : ILGenerator, ?argument : int16) = 
            inherit EnvItem(typeof<'T>, ilGen, ?argument = argument)

        and EnvItem(ty : Type, ilGen : ILGenerator, ?argument : int16) =

            let env = 
                match argument with
                | Some argId -> Arg argId
                | None -> LocalVar <| ilGen.DeclareLocal ty

            member __.Type = ty

            member e.Load () =
                match env with
                | Arg 0s -> ilGen.Emit OpCodes.Ldarg_0
                | Arg 1s -> ilGen.Emit OpCodes.Ldarg_1
                | Arg 2s -> ilGen.Emit OpCodes.Ldarg_2
                | Arg 3s -> ilGen.Emit OpCodes.Ldarg_3
                | Arg i -> ilGen.Emit (OpCodes.Ldarg_S, i)
                | LocalVar v -> ilGen.Emit(OpCodes.Ldloc, v)

            member e.LoadAddress () =
                match env with
                | Arg i -> ilGen.Emit (OpCodes.Ldarg_S, i)
                | LocalVar v -> ilGen.Emit(OpCodes.Ldloca, v)

            member e.Store () =
                match env with
                | LocalVar v -> ilGen.Emit(OpCodes.Stloc, v)
                | _ -> invalidOp "cannot store to arg param."

            member e.LocalBuilder =
                match env with
                | LocalVar v -> v
                | _ -> invalidArg "EnvItem" "is not a local variable."

        and EnvDescriptor =
            | Arg of int16
            | LocalVar of LocalBuilder


        // wrappers for defining dynamic methods

        module DynamicMethod =

            type private Marker = class end

            let private voidType = Type.GetType("System.Void")

            let private createDynamicMethod (name : string) (argTypes : Type []) (returnType : Type) =
                let dyn =
                    new DynamicMethod(name, 
                        MethodAttributes.Static ||| MethodAttributes.Public, CallingConventions.Standard, 
                        returnType, argTypes, typeof<Marker>, skipVisibility = true)

                dyn, dyn.GetILGenerator()

            let private compileDynamicMethod<'Dele when 'Dele :> Delegate> (dyn : DynamicMethod) =
                dyn.CreateDelegate(typeof<'Dele>) :?> 'Dele

            let compileFunc<'T> (name : string) (builderF : ILGenerator -> unit) =
                let dyn, ilGen = createDynamicMethod name [| |] typeof<'T>
                do builderF ilGen
                compileDynamicMethod<Func<'T>> dyn

            let compileFunc1<'U1,'V> (name : string) (builderF : EnvItem<'U1> -> ILGenerator -> unit) =
                let dyn, ilGen = createDynamicMethod name [| typeof<'U1> |] typeof<'V>
                let arg0 = EnvItem<'U1>(ilGen, 0s)
                do builderF arg0 ilGen
                compileDynamicMethod<Func<'U1,'V>> dyn

            let compileFunc2<'U1,'U2,'V> (name : string) (builderF : EnvItem<'U1> -> EnvItem<'U2> -> ILGenerator -> unit) =
                let dyn, ilGen = createDynamicMethod name [| typeof<'U1> ; typeof<'U2> |] typeof<'V>
                let arg0 = EnvItem<'U1>(ilGen, 0s)
                let arg1 = EnvItem<'U2>(ilGen, 1s)
                do builderF arg0 arg1 ilGen
                compileDynamicMethod<Func<'U1,'U2,'V>> dyn

            let compileFunc3<'U1,'U2,'U3,'V> (name : string) (builderF : EnvItem<'U1> -> EnvItem<'U2> -> EnvItem<'U3> -> ILGenerator -> unit) =
                let dyn, ilGen = createDynamicMethod name [| typeof<'U1> ; typeof<'U2> ; typeof<'U3> |] typeof<'V>
                let arg0 = EnvItem<'U1>(ilGen, 0s)
                let arg1 = EnvItem<'U2>(ilGen, 1s)
                let arg2 = EnvItem<'U3>(ilGen, 2s)
                do builderF arg0 arg1 arg2 ilGen
                compileDynamicMethod<Func<'U1,'U2,'U3,'V>> dyn

            let compileAction1<'U1> (name : string) (builderF : EnvItem<'U1> -> ILGenerator -> unit) =
                let dyn, ilGen = createDynamicMethod name [| typeof<'U1> |] voidType
                let arg0 = EnvItem<'U1>(ilGen, 0s)
                do builderF arg0 ilGen
                compileDynamicMethod<Action<'U1>> dyn

            let compileAction2<'U1,'U2> (name : string) (builderF : EnvItem<'U1> -> EnvItem<'U2> -> ILGenerator -> unit) =
                let dyn, ilGen = createDynamicMethod name [| typeof<'U1> ; typeof<'U2> |] voidType
                let arg0 = EnvItem<'U1>(ilGen, 0s)
                let arg1 = EnvItem<'U2>(ilGen, 1s)
                do builderF arg0 arg1 ilGen
                compileDynamicMethod<Action<'U1,'U2>> dyn

            let compileAction3<'U1,'U2,'U3> (name : string) (builderF : EnvItem<'U1> -> EnvItem<'U2> -> EnvItem<'U3> -> ILGenerator -> unit) =
                let dyn, ilGen = createDynamicMethod name [| typeof<'U1> ; typeof<'U2> ; typeof<'U3> |] voidType
                let arg0 = EnvItem<'U1>(ilGen, 0s)
                let arg1 = EnvItem<'U2>(ilGen, 1s)
                let arg2 = EnvItem<'U3>(ilGen, 2s)
                do builderF arg0 arg1 arg2 ilGen
                compileDynamicMethod<Action<'U1,'U2,'U3>> dyn


//        [<AutoOpen>]
//        module private ReflectedPicklerAPI =
//
//            let allMembers = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static
//
//            let typeFromHandle = typeof<Type>.GetMethod("GetTypeFromHandle")
//            let writerCtx = typeof<WriteState>.GetProperty("StreamingContext").GetGetMethod(true)
//            let readerCtx = typeof<ReadState>.GetProperty("StreamingContext").GetGetMethod(true)
//            let objInitializer = typeof<FormatterServices>.GetMethod("GetUninitializedObject", allMembers)
//            let deserializationCallBack = typeof<IDeserializationCallback>.GetMethod("OnDeserialization")
//            let picklerT = typedefof<Pickler<_>>
//            let getTypedPickler (t : Type) = picklerT.MakeGenericType [|t|]
//            let getPicklerWriter (t : Type) = getTypedPickler(t).GetMethod("Write")
//            let getPicklerReader (t : Type) = getTypedPickler(t).GetMethod("Read")
//            let formatWriter = typeof<WriteState>.GetProperty("Formatter", allMembers).GetGetMethod(true)
//            let formatReader = typeof<ReadState>.GetProperty("Formatter", allMembers).GetGetMethod(true)
//            let bwIntWriter = typeof<IPickleFormatWriter>.GetMethod("WriteInt32")
//            let brIntReader = typeof<IPickleFormatReader>.GetMethod("ReadInt32")
//
//        /// emits typed pickler from array of untyped picklers
//        let emitLoadPickler (picklers : EnvItem<Pickler []>) (t : Type) (idx : int) (ilGen : ILGenerator) =
//            let picklerType = getTypedPickler t
//            picklers.Load ()
//            ilGen.Emit(OpCodes.Ldc_I4, idx)
//            ilGen.Emit(OpCodes.Ldelem_Ref)
//            ilGen.Emit(OpCodes.Castclass, picklerType)
//
//        /// emit IL that serializes last object in stack
//        /// last 4 items in stack: Pickler<'T>; WriteState ; string ; 'T
//        let emitSerialize (t : Type) (ilGen : ILGenerator) =
//            let writer = getPicklerWriter t
//            ilGen.EmitCall(OpCodes.Callvirt, writer, null)
//
//        /// emit IL that deserializes an object
//        /// last 3 items in stack: Pickler<'T> ; ReadState ; string
//        let emitDeserialize (t : Type) (ilGen : ILGenerator) =
//            let reader = getPicklerReader t
//            ilGen.EmitCall(OpCodes.Callvirt, reader, null)
//
//        /// emits IL that serializes a collection of fields
//        let emitSerializeFields (fields : FieldInfo []) 
//                                (writer : EnvItem<WriteState>) 
//                                (picklers : EnvItem<Pickler []>) 
//                                (parent : EnvItem<'T>) (ilGen : ILGenerator) =
//
//            for i = 0 to fields.Length - 1 do
//                let f = fields.[i]
//                // load typed pickler to the stack
//                emitLoadPickler picklers f.FieldType i ilGen
//                // load writer to the stack
//                writer.Load ()
//                // load field name
//                ilGen.Emit(OpCodes.Ldstr, f.Name)
//                // load field value to the stack
//                parent.Load ()
//                ilGen.Emit(OpCodes.Ldfld, f)
//                // perform serialization
//                emitSerialize f.FieldType ilGen
//
//        /// deserialize a collection of fields and store to parent object
//        let emitDeserializeFields (fields : FieldInfo [])
//                                  (reader : EnvItem<ReadState>)
//                                  (picklers : EnvItem<Pickler []>)
//                                  (parent : EnvItem<'T>) (ilGen : ILGenerator) =
//
//            let isStruct = fields.Length > 0 && fields.[0].DeclaringType.IsValueType
//
//            for i = 0 to fields.Length - 1 do
//                let f = fields.[i]
//                // load parent object to the stack
//                if isStruct then parent.LoadAddress ()
//                else
//                    parent.Load ()
//
//                // load typed pickler to the stack
//                emitLoadPickler picklers f.FieldType i ilGen
//                // load reader to the stack
//                reader.Load ()
//                // load field name
//                ilGen.Emit(OpCodes.Ldstr, f.Name)
//                // deserialize and load to the stack
//                emitDeserialize f.FieldType ilGen
//                // assign value to the field
//                ilGen.Emit(OpCodes.Stfld, f)
//
//        /// serialize properties to the underlying stack
//        let emitSerializeProperties (properties : PropertyInfo [])
//                                    (writer : EnvItem<WriteState>)
//                                    (picklers : EnvItem<Pickler []>)
//                                    (parent : EnvItem<'T>) (ilGen : ILGenerator) =
//
//            for i = 0 to properties.Length - 1 do
//                let p = properties.[i]
//                let m = p.GetGetMethod(true)
//                // load typed pickler to the stack
//                emitLoadPickler picklers m.ReturnType i ilGen
//                // load writer to the stack
//                writer.Load ()
//                // load tag
//                ilGen.Emit(OpCodes.Ldstr, p.Name)
//                // load property value to the stack
//                parent.Load ()
//                ilGen.EmitCall(OpCodes.Call, m, null)
//                // perform serialization
//                emitSerialize m.ReturnType ilGen
//
//        /// deserialize fields, pass to factory method and push to stack
//        let emitDeserializeAndConstruct (factory : Choice<MethodInfo,ConstructorInfo>)
//                                        (fparams : (Type * string) [])
//                                        (reader : EnvItem<ReadState>)
//                                        (picklers : EnvItem<Pickler []>)
//                                        (ilGen : ILGenerator) =
//
//            for i = 0 to fparams.Length - 1 do
//                let p,tag = fparams.[i]
//                // load typed pickler to the stack
//                emitLoadPickler picklers p i ilGen
//                // load reader to the stack
//                reader.Load ()
//                // load tag
//                ilGen.Emit(OpCodes.Ldstr, tag)
//                // perform deserialization and push to the stack
//                emitDeserialize p ilGen
//
//            // call factory method
//            match factory with
//            | Choice1Of2 f -> ilGen.EmitCall(OpCodes.Call, f, null)
//            | Choice2Of2 c -> ilGen.Emit(OpCodes.Newobj, c)
//
//
//        /// push an uninitialized object of type 't' to the stack
//        let emitObjectInitializer (t : Type) (ilGen : ILGenerator) =
//            // reify type instance
//            ilGen.Emit(OpCodes.Ldtoken, t)
//            ilGen.EmitCall(OpCodes.Call, typeFromHandle, null)
//            // call the object initializer
//            ilGen.EmitCall(OpCodes.Call, objInitializer, null)
//            // unbox value
//            if t.IsValueType then
//                ilGen.Emit(OpCodes.Unbox_Any, t)
//            else
//                ilGen.Emit(OpCodes.Castclass, t)
//
//        /// calls a predefined collection of serialization methods on given value
//        let emitSerializationMethodCalls (methods : MethodInfo []) (wOr : Choice<EnvItem<WriteState>, EnvItem<ReadState>>)
//                                            (value : EnvItem<'T>) (ilGen : ILGenerator) =
//
//            if methods.Length = 0 then () else
//
//            // evaluate the streaming context
//            let ctx = EnvItem<StreamingContext>(ilGen)
//
//            match wOr with
//            | Choice1Of2 w -> w.Load () ; ilGen.EmitCall(OpCodes.Call, writerCtx, null)
//            | Choice2Of2 r -> r.Load () ; ilGen.EmitCall(OpCodes.Call, readerCtx, null)
//
//            ctx.Store ()
//
//            // call the methods
//            for m in methods do
//                value.Load ()
//                ctx.Load ()
//                ilGen.EmitCall(OpCodes.Call, m, null)
//
//        /// emit a call to the 'OnDeserialization' method on given value
//        let emitDeserializationCallback (value : EnvItem<'T>) (ilGen : ILGenerator) =
//            value.Load ()
//            ilGen.Emit(OpCodes.Castclass, typeof<IDeserializationCallback>)
//            ilGen.Emit OpCodes.Ldnull
//            ilGen.EmitCall(OpCodes.Callvirt, deserializationCallBack, null)
//
//        /// wraps call to ISerializable constructor in a dynamic method
//        let wrapISerializableConstructor<'T> (ctor : ConstructorInfo) =
//            DynamicMethod.compileFunc2<SerializationInfo, StreamingContext, 'T> "ISerializableCtor" (fun sI sC ilGen ->
//                sI.Load ()
//                sC.Load ()
//
//                ilGen.Emit(OpCodes.Newobj, ctor)
//                ilGen.Emit OpCodes.Ret
//            )
//
//        /// writes and integer
//        let writeInt (writer : EnvItem<WriteState>) (tag : string) (n : EnvItem<int>) (ilGen : ILGenerator) =
//            writer.Load ()
//            ilGen.EmitCall(OpCodes.Call, formatWriter, null) // load format writer
//            ilGen.Emit(OpCodes.Ldstr, tag) // load tag
//            n.Load () // load value
//            ilGen.EmitCall(OpCodes.Callvirt, bwIntWriter, null) // perform write
//
//        /// reads an integer and push to stack
//        let readInt (reader : EnvItem<ReadState>) (tag : string) (ilGen : ILGenerator) =
//            reader.Load ()
//            ilGen.EmitCall(OpCodes.Call, formatReader, null) // load format reader
//            ilGen.Emit(OpCodes.Ldstr, tag) // load tag
//            ilGen.EmitCall(OpCodes.Callvirt, brIntReader, null) // perform read, push to stack
        
#endif