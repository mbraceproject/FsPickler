namespace Nessos.FsPickler

    open System
    open System.IO
    open System.Reflection
    open System.Runtime.Serialization
    
    open Nessos.FsPickler
    open Nessos.FsPickler.Utils

    module internal PicklerUtils =

        let keepEndianness = (Config.optimizeForLittleEndian = BitConverter.IsLittleEndian)

        // initialize a pickler from a typed set of lambdas
        let inline mkPickler<'T> (info:PicklerInfo) (useWithSubtypes:bool) (cache:bool) 
                                            (reader : Reader -> 'T) (writer : Writer -> 'T -> unit) =

            new Pickler<'T>(reader, writer, info, cacheByRef = cache, useWithSubtypes = useWithSubtypes)

        /// filter a collection of methods that carry serialization attributes
        let getSerializationMethods<'Attr when 'Attr :> Attribute> (ms : MethodInfo []) =
            let isSerializationMethod(m : MethodInfo) =
                not m.IsStatic && 
                containsAttr<'Attr> m &&
                m.ReturnType = typeof<System.Void> &&

                    match m.GetParameters() with
                    | [| p |] when p.ParameterType = typeof<StreamingContext> -> true
                    | _ -> false

            ms |> Array.filter isSerializationMethod

        let rec isISerializable(t : Type) =
            if typeof<ISerializable>.IsAssignableFrom t then true
            else
                match t.BaseType with
                | null -> false
                | bt -> isISerializable bt

        let mkDelegates<'T> (ms : MethodInfo []) =
            let wrap m = Delegate.CreateDelegate(typeof<Action<'T, StreamingContext>>, m) :?> Action<'T, StreamingContext>
            Array.map wrap ms

        let inline getStreamingContext (x : ^T when ^T : (member StreamingContext : StreamingContext)) =
            ( ^T : (member StreamingContext : StreamingContext) x)


        /// set pickler id based on set of source picklers
        /// will result in error if source picklers have conflicting source ids
        /// used with external combinator library
        let setPicklerId<'T when 'T :> Pickler> (sourcePicklers : seq<Pickler>) (targetPickler : ^T) =
            let mutable current = null
            for p in sourcePicklers do
                match p.CacheId with
                | null -> ()
                | source when current = null -> current <- source
                | source when current = source -> ()
                | source -> 
                    let msg = "attempting to generate pickler using incompatible sources."
                    raise <| new PicklerGenerationException(p.Type, msg)

            targetPickler.CacheId <- current
            targetPickler

        // checks pickler compatibility at runtime
        let checkPicklerCompat (uuid : string) (p : Pickler) =
            match p.CacheId with
            | null -> ()
            | id when id <> uuid ->
                let msg = sprintf "Attempting to use pickler of type '%O' generated from incompatible cache." p.Type
                raise <| new SerializationException(msg)
            | _ -> ()

        let isNullableType(t : Type) =
            t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Nullable<_>>

        //
        //  internal read/write combinators
        //

        let inline isPrimitive (f : Pickler) = f.TypeKind <= TypeKind.String

        let inline write bypass (w : Writer) (p : Pickler<'T>) tag (x : 'T) =
            if bypass then p.Write w x
            else w.Write(p, tag, x)

        let inline read bypass (r : Reader) (p : Pickler<'T>) tag =
            if bypass then p.Read r
            else r.Read (p, tag)

//        /// safely serialize strings, including nulls
//        let inline writeStringSafe (bw : BinaryWriter) (x : string) =
//            if obj.ReferenceEquals(x, null) then bw.Write true
//            else
//                bw.Write false ; bw.Write x
//
//        /// safely deserialize strings, including nulls
//        let inline readStringSafe (br : BinaryReader) =
//            if br.ReadBoolean() then null
//            else
//                br.ReadString()

        let inline writeArray (w : Writer) (p : Pickler<'T>) (ts : 'T []) =
            let isPrimitive = isPrimitive p
            w.Formatter.WriteInt32 "length" ts.Length
            for t in ts do write isPrimitive w p "item" t

        let inline readArray (r : Reader) (p : Pickler<'T>) =
            let isPrimitive = isPrimitive p
            let n = r.Formatter.ReadInt32 "length"
            let arr = Array.zeroCreate<'T> n
            for i = 0 to n - 1 do
                arr.[i] <- read isPrimitive r p "item"
            arr

        // length passed as argument to avoid unecessary evaluations of sequence
        let inline writeSeq (w : Writer) (p : Pickler<'T>) (length : int) (ts : seq<'T>) =
            let isPrimitive = isPrimitive p
            w.Formatter.WriteInt32 "length" length
            for t in ts do write isPrimitive w p "item" t

        // TODO : value types should probably be block deserialized
        let inline readSeq (r : Reader) (p : Pickler<'T>) =
            let isPrimitive = isPrimitive p
            let length = r.Formatter.ReadInt32 "length"
            let ts = Array.zeroCreate<'T> length
            for i = 0 to length - 1 do
                ts.[i] <- read isPrimitive r p "item"
            ts

        // length passed as argument to avoid unecessary evaluations of sequence
        let inline writeKVPairs (w : Writer) (kp : Pickler<'K>) (vp : Pickler<'V>) (length : int) (xs : ('K * 'V) seq) =
            let kIsPrim = isPrimitive kp
            let vIsPrim = isPrimitive vp
            w.Formatter.WriteInt32 "length" length
            for k,v in xs do
                write kIsPrim w kp "key" k
                write vIsPrim w vp "val" v

        let inline readKVPairs (r : Reader) (kf : Pickler<'K>) (vf : Pickler<'V>) =
            let kIsPrim = isPrimitive kf
            let vIsPrim = isPrimitive vf
            let length = r.Formatter.ReadInt32 "length"
            let xs = Array.zeroCreate<'K * 'V> length

            for i = 0 to length - 1 do
                let k = read kIsPrim r kf "key"
                let v = read vIsPrim r vf "val"
                xs.[i] <- k,v

            xs


        // equivalent implementations for client facade

        let writeSeq' (p : Pickler<'T>) (w : Writer) (ts : 'T seq) : unit =
            let isPrimitive = isPrimitive p
            match ts with
            | :? ('T []) as arr ->
                w.Formatter.WriteBoolean "isMaterialized" true
                w.Formatter.WriteInt32 "length" arr.Length
                for i = 0 to arr.Length - 1 do
                    write isPrimitive w p "item" arr.[i]

            | :? ('T list) as list ->
                w.Formatter.WriteBoolean "isMaterialized" true
                w.Formatter.WriteInt32 "length" list.Length

                let rec iter rest =
                    match rest with
                    | [] -> ()
                    | t :: tl ->
                        write isPrimitive w p "item" t
                        iter tl

                iter list
            | _ ->
                w.Formatter.WriteBoolean "isMaterialized" false
                use e = ts.GetEnumerator()
                while e.MoveNext() do
                    w.Formatter.WriteBoolean "done" false
                    write isPrimitive w p "item" e.Current

                w.Formatter.WriteBoolean "done" true

        let readSeq' (p : Pickler<'T>) (r : Reader) : 'T seq =
            let isPrimitive = isPrimitive p

            if r.Formatter.ReadBoolean "isMaterialied" then
                let length = r.Formatter.ReadInt32 "length"
                let array = Array.zeroCreate<'T> length
                for i = 0 to length - 1 do
                    array.[i] <- read isPrimitive r p "item"
                array :> _
            else
                let ra = new ResizeArray<'T> ()
                while not <| r.Formatter.ReadBoolean "done" do
                    let next = read isPrimitive r p "item"
                    ra.Add next

                ra :> _

        let writeKVPairs' (kp : Pickler<'K>) (vp : Pickler<'V>) (w : Writer) (xs : ('K * 'V) seq) : unit =
            let kIsPrim = isPrimitive kp
            let vIsPrim = isPrimitive vp
            match xs with
            | :? (('K * 'V) []) as arr ->
                w.Formatter.WriteBoolean "isMaterialized" true
                w.Formatter.WriteInt32 "length" arr.Length

                for i = 0 to arr.Length - 1 do
                    let k,v = arr.[i]
                    write kIsPrim w kp "key" k
                    write vIsPrim w vp "val" v

            | :? (('K * 'V) list) as list ->
                w.Formatter.WriteBoolean "isMaterialized" true
                w.Formatter.WriteInt32 "length" list.Length

                let rec iter rest =
                    match rest with
                    | [] -> ()
                    | (k,v) :: tl ->
                        write kIsPrim w kp "key" k
                        write vIsPrim w vp "val" v
                        iter tl

                iter list

            | _ ->
                w.Formatter.WriteBoolean "isMaterialized" false
                let e = xs.GetEnumerator()

                while e.MoveNext() do
                    w.Formatter.WriteBoolean "done" false
                    let k,v = e.Current
                    write kIsPrim w kp "key" k
                    write vIsPrim w vp "val" v

                w.Formatter.WriteBoolean "done" true


        /// Deserializes a sequence of key/value pairs from the underlying stream
        let readKVPairs' (kp : Pickler<'K>) (vp : Pickler<'V>) (r : Reader) =
            let kIsPrim = isPrimitive kp
            let vIsPrim = isPrimitive vp

            if r.Formatter.ReadBoolean "isMaterialied" then
                let length = r.Formatter.ReadInt32 "length"
                let arr = Array.zeroCreate<'K * 'V> length
                for i = 0 to length - 1 do
                    let k = read kIsPrim r kp "key"
                    let v = read vIsPrim r vp "val"
                    arr.[i] <- k,v
                arr :> seq<'K * 'V>
            else
                let ra = new ResizeArray<'K * 'V> ()
                while not <| r.Formatter.ReadBoolean "done" do
                    let k = read kIsPrim r kp "key"
                    let v = read vIsPrim r vp "val"
                    ra.Add (k,v)

                ra :> seq<'K * 'V>

#if EMIT_IL

        open System.Reflection.Emit
        open Nessos.FsPickler.Emit
    
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
            picklers.Load ()
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
                writer.Load ()
                // load typed pickler to the stack
                emitLoadPickler picklers f.FieldType i ilGen
                // load field value to the stack
                parent.Load ()
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
                if isStruct then parent.LoadAddress ()
                else
                    parent.Load ()
                // load reader to the stack
                reader.Load ()
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
                writer.Load ()
                // load typed pickler to the stack
                emitLoadPickler picklers m.ReturnType i ilGen
                // load property value to the stack
                parent.Load ()
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
                reader.Load ()
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

        /// writes and integer
        let writeInt (writer : EnvItem<Writer>) (n : EnvItem<int>) (ilGen : ILGenerator) =
            writer.Load ()
            ilGen.EmitCall(OpCodes.Call, bw, null) // load BinaryWriter
            n.Load ()
            ilGen.EmitCall(OpCodes.Call, bwIntWriter, null) // perform write

        /// reads an integer and push to stack
        let readInt (reader : EnvItem<Reader>) (ilGen : ILGenerator) =
            reader.Load ()
            ilGen.EmitCall(OpCodes.Call, br, null) // load BinaryReader
            ilGen.EmitCall(OpCodes.Call, brIntReader, null) // perform read, push to stacks
        
#endif
