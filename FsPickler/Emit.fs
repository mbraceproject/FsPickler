module internal FsPickler.Emit

#if EMIT_IL

    open System
    open System.Reflection
    open System.Reflection.Emit
    open System.Runtime.Serialization

    open FsPickler.Utils
    open FsPickler.PicklerUtils

    module DynamicMethod =

        type private Marker = class end

        let private voidType = Type.GetType("System.Void")

        let compile<'Dele when 'Dele :> Delegate> (name : string) (argTypes : Type []) (returnType : Type) (builderF : ILGenerator -> unit) =

            let dMeth = new DynamicMethod(name, MethodAttributes.Static ||| MethodAttributes.Public, 
                            CallingConventions.Standard, returnType, argTypes, typeof<Marker>, skipVisibility = true)

            let ilGen = dMeth.GetILGenerator()
            do builderF ilGen
            dMeth.CreateDelegate(typeof<'Dele>) :?> 'Dele

        let compileFunc<'T> (name : string) (builderF : ILGenerator -> unit) =
            compile<Func<'T>> name [| |] typeof<'T> builderF

        let compileFunc1<'U,'V> (name : string) (builderF : ILGenerator -> unit) =
            compile<Func<'U,'V>> name [| typeof<'U> |] typeof<'V> builderF

        let compileFunc2<'U1,'U2,'V> (name : string) (builderF : ILGenerator -> unit) =
            compile<Func<'U1,'U2,'V>> name [| typeof<'U1> ; typeof<'U2> |] typeof<'V> builderF

        let compileFunc3<'U1,'U2,'U3,'V> (name : string) (builderF : ILGenerator -> unit) =
            compile<Func<'U1,'U2,'U3,'V>> name [| typeof<'U1> ; typeof<'U2> ; typeof<'U3> |] typeof<'V> builderF

        let compileAction1<'U> (name : string) (builderF : ILGenerator -> unit) =
            compile<Action<'U>> name [| typeof<'U> |] voidType builderF

        let compileAction2<'U1,'U2> (name : string) (builderF : ILGenerator -> unit) =
            compile<Action<'U1,'U2>> name [| typeof<'U1> ; typeof<'U2> |] voidType builderF

        let compileAction3<'U1,'U2,'U3> (name : string) (builderF : ILGenerator -> unit) =
            compile<Action<'U1,'U2,'U3>> name [| typeof<'U1> ; typeof<'U2> ; typeof<'U3> |] voidType builderF


    type EnvItem<'T> =
        | Arg0
        | Arg1
        | Arg2
        | Arg3
        | Arg of int16
        | LocalVar of LocalBuilder
    with
        member e.Load (ilGen : ILGenerator) =
            match e with
            | Arg0 -> ilGen.Emit OpCodes.Ldarg_0
            | Arg1 -> ilGen.Emit OpCodes.Ldarg_1
            | Arg2 -> ilGen.Emit OpCodes.Ldarg_2
            | Arg3 -> ilGen.Emit OpCodes.Ldarg_3
            | Arg i -> ilGen.Emit (OpCodes.Ldarg_S, i)
            | LocalVar v -> ilGen.Emit(OpCodes.Ldloc, v)

        member e.LoadAddress (ilGen : ILGenerator) =
            match e with
            | Arg0 -> ilGen.Emit(OpCodes.Ldarga_S, 0s)
            | Arg1 -> ilGen.Emit(OpCodes.Ldarga_S, 1s)
            | Arg2 -> ilGen.Emit(OpCodes.Ldarga_S, 2s)
            | Arg3 -> ilGen.Emit(OpCodes.Ldarga_S, 3s)
            | Arg i -> ilGen.Emit (OpCodes.Ldarg_S, i)
            | LocalVar v -> ilGen.Emit(OpCodes.Ldloca, v)

        member e.Store (ilGen : ILGenerator) =
            match e with
            | LocalVar v -> ilGen.Emit(OpCodes.Stloc, v)
            | _ -> invalidOp "cannot store to arg param."

        member e.LocalBuilder =
            match e with
            | LocalVar v -> v
            | _ -> invalidArg "EnvItem" "is not a local variable."

        static member InitVar (ilGen : ILGenerator, ?reflectedType : Type) =
            let t = defaultArg reflectedType typeof<'T>
            let v = ilGen.DeclareLocal t
            EnvItem<'T>.LocalVar v
            

    [<AutoOpen>]
    module private ReflectedPicklerAPI =

        let typeFromHandle = typeof<Type>.GetMethod("GetTypeFromHandle")
        let writerCtx = typeof<Writer>.GetProperty("StreamingContext").GetGetMethod(true)
        let readerCtx = typeof<Reader>.GetProperty("StreamingContext").GetGetMethod(true)
        let objInitializer = typeof<FormatterServices>.GetMethod("GetUninitializedObject", BindingFlags.Public ||| BindingFlags.Static)
        let deserializationCallBack = typeof<IDeserializationCallback>.GetMethod("OnDeserialization")
        let picklerT = typedefof<Pickler<_>>
        let writerM = typeof<Writer>.GetGenericMethod(false, "Write", 1, 2)
        let readerM = typeof<Reader>.GetGenericMethod(false, "Read", 1, 1)
        let bw = typeof<Writer>.GetProperty("BinaryWriter", BindingFlags.Public ||| BindingFlags.Instance).GetGetMethod(true)
        let br = typeof<Reader>.GetProperty("BinaryReader", BindingFlags.Public ||| BindingFlags.Instance).GetGetMethod(true)
        let bwIntWriter = typeof<System.IO.BinaryWriter>.GetMethod("Write", [| typeof<int> |])
        let brIntReader = typeof<System.IO.BinaryReader>.GetMethod("ReadInt32")

    /// emits typed pickler from array of untyped picklers
    let emitLoadPickler (picklers : EnvItem<Pickler []>) (t : Type) (idx : int) (ilGen : ILGenerator) =
        picklers.Load ilGen
        ilGen.Emit(OpCodes.Ldc_I4, idx)
        ilGen.Emit(OpCodes.Ldelem_Ref)
        ilGen.Emit(OpCodes.Castclass, picklerT.MakeGenericType [| t |])

    /// emit IL that serializes last object in stack
    /// last 3 items in stack: Writer; Pickler<'T>; 'T
    let emitSerialize (t : Type) (ilGen : ILGenerator) =
        let m = writerM.MakeGenericMethod [| t |]
        ilGen.EmitCall(OpCodes.Call, m, null)

    /// emit IL that deserializes an object
    /// last 2 items in stack: Reader; Pickler<'T>
    let emitDeserialize (t : Type) (ilGen : ILGenerator) =
        let m = readerM.MakeGenericMethod [| t |]
        ilGen.EmitCall(OpCodes.Call, m, null)

    /// emits IL that serializes a collection of fields
    let emitSerializeFields (fields : FieldInfo []) 
                            (writer : EnvItem<Writer>) 
                            (picklers : EnvItem<Pickler []>) 
                            (parent : EnvItem<'T>) (ilGen : ILGenerator) =

        for i = 0 to fields.Length - 1 do
            let f = fields.[i]
            // load writer to the stack
            writer.Load ilGen
            // load typed pickler to the stack
            emitLoadPickler picklers f.FieldType i ilGen
            // load field value to the stack
            parent.Load ilGen
            ilGen.Emit(OpCodes.Ldfld, f)
            // perform serialization
            emitSerialize f.FieldType ilGen

    /// deserialize a collection of fields and store to parent object
    let emitDeserializeFields (fields : FieldInfo [])
                              (reader : EnvItem<Reader>)
                              (picklers : EnvItem<Pickler []>)
                              (parent : EnvItem<'T>) (ilGen : ILGenerator) =

        let isStruct = fields.Length > 0 && fields.[0].DeclaringType.IsValueType

        for i = 0 to fields.Length - 1 do
            let f = fields.[i]
            // load parent object to the stack
            if isStruct then parent.LoadAddress ilGen
            else
                parent.Load ilGen
            // load reader to the stack
            reader.Load ilGen
            // load typed pickler to the stack
            emitLoadPickler picklers f.FieldType i ilGen
            // deserialize and load to the stack
            emitDeserialize f.FieldType ilGen
            // assign value to the field
            ilGen.Emit(OpCodes.Stfld, f)

    /// serialize properties to the underlying stack
    let emitSerializeProperties (properties : PropertyInfo [])
                                (writer : EnvItem<Writer>)
                                (picklers : EnvItem<Pickler []>)
                                (parent : EnvItem<'T>) (ilGen : ILGenerator) =

        for i = 0 to properties.Length - 1 do
            let m = properties.[i].GetGetMethod(true)
            // load writer to the stack
            writer.Load ilGen
            // load typed pickler to the stack
            emitLoadPickler picklers m.ReturnType i ilGen
            // load property value to the stack
            parent.Load ilGen
            ilGen.EmitCall(OpCodes.Call, m, null)
            // perform serialization
            emitSerialize m.ReturnType ilGen

    /// deserialize fields, pass to factory method and push to stack
    let emitDeserializeAndConstruct (factory : Choice<MethodInfo,ConstructorInfo>)
                                    (fparams : Type [])
                                    (reader : EnvItem<Reader>)
                                    (picklers : EnvItem<Pickler []>)
                                    (ilGen : ILGenerator) =

        for i = 0 to fparams.Length - 1 do
            let p = fparams.[i]
            // load reader to the stack
            reader.Load ilGen
            // load typed pickler to the stack
            emitLoadPickler picklers p i ilGen
            // perform deserialization and push to the stack
            emitDeserialize p ilGen

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
    let emitSerializationMethodCalls (methods : MethodInfo []) (wOr : Choice<EnvItem<Writer>, EnvItem<Reader>>)
                                        (value : EnvItem<'T>) (ilGen : ILGenerator) =

        if methods.Length = 0 then () else

        // evaluate the streaming context
        let ctx = EnvItem<StreamingContext>.InitVar ilGen

        match wOr with
        | Choice1Of2 w -> w.Load ilGen ; ilGen.EmitCall(OpCodes.Call, writerCtx, null)
        | Choice2Of2 r -> r.Load ilGen ; ilGen.EmitCall(OpCodes.Call, readerCtx, null)

        ctx.Store ilGen

        // call the methods
        for m in methods do
            value.Load ilGen
            ctx.Load ilGen
            ilGen.EmitCall(OpCodes.Call, m, null)

    /// emit a call to the 'OnDeserialization' method on given value
    let emitDeserializationCallback (value : EnvItem<'T>) (ilGen : ILGenerator) =
        value.Load ilGen
        ilGen.Emit(OpCodes.Castclass, typeof<IDeserializationCallback>)
        ilGen.Emit OpCodes.Ldnull
        ilGen.EmitCall(OpCodes.Callvirt, deserializationCallBack, null)

    /// wraps call to ISerializable constructor in a dynamic method
    let wrapISerializableConstructor<'T> (ctor : ConstructorInfo) =
        DynamicMethod.compileFunc2<SerializationInfo, StreamingContext, 'T> "ISerializableCtor" (fun ilGen ->
            ilGen.Emit OpCodes.Ldarg_0
            ilGen.Emit OpCodes.Ldarg_1

            ilGen.Emit(OpCodes.Newobj, ctor)
            ilGen.Emit OpCodes.Ret
        )

    /// writes and integer
    let writeInt (writer : EnvItem<Writer>) (n : EnvItem<int>) (ilGen : ILGenerator) =
        writer.Load ilGen
        ilGen.EmitCall(OpCodes.Call, bw, null) // load BinaryWriter
        n.Load ilGen
        ilGen.EmitCall(OpCodes.Call, bwIntWriter, null) // perform write

    /// reads an integer and push to stack
    let readInt (reader : EnvItem<Reader>) (ilGen : ILGenerator) =
        reader.Load ilGen
        ilGen.EmitCall(OpCodes.Call, br, null) // load BinaryReader
        ilGen.EmitCall(OpCodes.Call, brIntReader, null) // perform read, push to stacks
        
#endif