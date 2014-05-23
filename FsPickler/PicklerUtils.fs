namespace Nessos.FsPickler

    open System
    open System.IO
    open System.Collections
    open System.Collections.Generic
    open System.Reflection
    open System.Runtime.Serialization
    
    open Nessos.FsPickler
    open Nessos.FsPickler.Utils

    module internal PicklerUtils =

        let keepEndianness = (Config.optimizeForLittleEndian = BitConverter.IsLittleEndian)

        // initialize a pickler from a typed set of lambdas
        let inline mkPickler<'T> (info:PicklerInfo) (useWithSubtypes:bool) (cache:bool) 
                                            (reader : ReadState -> 'T) (writer : WriteState -> 'T -> unit) =

            CompositePickler.Create<'T>(reader, writer, info, cacheByRef = cache, useWithSubtypes = useWithSubtypes)

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
//
//        let inline isPrimitive (f : Pickler) = f.TypeKind <= TypeKind.String
//
//        let inline write bypass (w : WriteState) (p : Pickler<'T>) tag (x : 'T) =
//            if bypass then p.Write w x
//            else w.Write(p, tag, x)
//
//        let inline read bypass (r : ReadState) (p : Pickler<'T>) tag =
//            if bypass then p.Read r
//            else r.Read (p, tag)

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
#if DEBUG
        let writeArray (w : WriteState) (p : Pickler<'T>) (ts : 'T []) =
#else
        let inline writeArray (w : WriteState) (p : Pickler<'T>) (ts : 'T []) =
#endif
            w.Formatter.WriteInt32 "length" ts.Length
            for t in ts do p.Write w "item" t

#if DEBUG
        let readArray (r : ReadState) (p : Pickler<'T>) =
#else
        let inline readArray (r : ReadState) (p : Pickler<'T>) =
#endif
            let length = r.Formatter.ReadInt32 "length"
            let array = Array.zeroCreate<'T> length
            for i = 0 to length - 1 do
                array.[i] <- p.Read r "item"
            array

        // length passed as argument to avoid unecessary evaluations of sequence
#if DEBUG
        let writeSeq (w : WriteState) (p : Pickler<'T>) (length : int) (ts : seq<'T>) =
#else
        let inline writeSeq (w : WriteState) (p : Pickler<'T>) (length : int) (ts : seq<'T>) =
#endif
            w.Formatter.WriteInt32 "length" length
            for t in ts do p.Write w "item" t

#if DEBUG
        let readSeq (r : ReadState) (p : Pickler<'T>) =
#else
        let inline readSeq (r : ReadState) (p : Pickler<'T>) =
#endif
            let length = r.Formatter.ReadInt32 "length"
            let ts = Array.zeroCreate<'T> length
            for i = 0 to length - 1 do
                ts.[i] <- p.Read r "item"
            ts

        // length passed as argument to avoid unecessary evaluations of sequence
#if DEBUG
        let writeKVPairs (w : WriteState) (kp : Pickler<'K>) (vp : Pickler<'V>) (length : int) (xs : ('K * 'V) seq) =
#else
        let inline writeKVPairs (w : WriteState) (kp : Pickler<'K>) (vp : Pickler<'V>) (length : int) (xs : ('K * 'V) seq) =
#endif
            w.Formatter.WriteInt32 "length" length
            for k,v in xs do
                kp.Write w "key" k
                vp.Write w "val" v

#if DEBUG
        let readKVPairs (r : ReadState) (kp : Pickler<'K>) (vp : Pickler<'V>) =
#else
        let inline readKVPairs (r : ReadState) (kp : Pickler<'K>) (vp : Pickler<'V>) =
#endif
            let length = r.Formatter.ReadInt32 "length"
            let xs = Array.zeroCreate<'K * 'V> length

            for i = 0 to length - 1 do
                let k = kp.Read r "key"
                let v = vp.Read r "val"
                xs.[i] <- k,v

            xs

        // equivalent implementations for client facade

        let writeSeq' (p : Pickler<'T>) (w : WriteState) (ts : 'T seq) : unit =
            match ts with
            | :? ('T []) as arr ->
                w.Formatter.WriteBoolean "isMaterialized" true
                w.Formatter.WriteInt32 "length" arr.Length
                for i = 0 to arr.Length - 1 do
                    p.Write w "item" arr.[i]

            | :? ('T list) as list ->
                w.Formatter.WriteBoolean "isMaterialized" true
                w.Formatter.WriteInt32 "length" list.Length

                let rec iter rest =
                    match rest with
                    | [] -> ()
                    | t :: tl ->
                        p.Write w "item" t
                        iter tl

                iter list
            | _ ->
                w.Formatter.WriteBoolean "isMaterialized" false
                use e = ts.GetEnumerator()
                while e.MoveNext() do
                    w.Formatter.WriteBoolean "done" false
                    p.Write w "item" e.Current

                w.Formatter.WriteBoolean "done" true

        let readSeq' (p : Pickler<'T>) (r : ReadState) : 'T seq =

            if r.Formatter.ReadBoolean "isMaterialied" then
                let length = r.Formatter.ReadInt32 "length"
                let array = Array.zeroCreate<'T> length
                for i = 0 to length - 1 do
                    array.[i] <- p.Read r "item"
                array :> _
            else
                let ra = new ResizeArray<'T> ()
                while not <| r.Formatter.ReadBoolean "done" do
                    let next = p.Read r "item"
                    ra.Add next

                ra :> _

        let writeKVPairs' (kp : Pickler<'K>) (vp : Pickler<'V>) (w : WriteState) (xs : ('K * 'V) seq) : unit =
            match xs with
            | :? (('K * 'V) []) as arr ->
                w.Formatter.WriteBoolean "isMaterialized" true
                w.Formatter.WriteInt32 "length" arr.Length

                for i = 0 to arr.Length - 1 do
                    let k,v = arr.[i]
                    kp.Write w "key" k
                    vp.Write w "val" v

            | :? (('K * 'V) list) as list ->
                w.Formatter.WriteBoolean "isMaterialized" true
                w.Formatter.WriteInt32 "length" list.Length

                let rec iter rest =
                    match rest with
                    | [] -> ()
                    | (k,v) :: tl ->
                        kp.Write w "key" k
                        vp.Write w "val" v
                        iter tl

                iter list

            | _ ->
                w.Formatter.WriteBoolean "isMaterialized" false
                let e = xs.GetEnumerator()

                while e.MoveNext() do
                    w.Formatter.WriteBoolean "done" false
                    let k,v = e.Current
                    kp.Write w "key" k
                    vp.Write w "val" v

                w.Formatter.WriteBoolean "done" true


        /// Deserializes a sequence of key/value pairs from the underlying stream
        let readKVPairs' (kp : Pickler<'K>) (vp : Pickler<'V>) (r : ReadState) =

            if r.Formatter.ReadBoolean "isMaterialied" then
                let length = r.Formatter.ReadInt32 "length"
                let arr = Array.zeroCreate<'K * 'V> length
                for i = 0 to length - 1 do
                    let k = kp.Read r "key"
                    let v = vp.Read r "val"
                    arr.[i] <- k,v
                arr :> seq<'K * 'V>
            else
                let ra = new ResizeArray<'K * 'V> ()
                while not <| r.Formatter.ReadBoolean "done" do
                    let k = kp.Read r "key"
                    let v = vp.Read r "val"
                    ra.Add (k,v)

                ra :> seq<'K * 'V>


        let sequenceCounterResetThreshold = Nessos.FsPickler.Header.sequenceCounterResetThreshold

        let writeTopLevelSequence (pickler : Pickler<'T>) (state : WriteState) (tag : string) (values : seq<'T>) : int =

            let isValue = pickler.TypeKind <= TypeKind.Value

#if DEBUG
            let write idx t =
#else
            let inline write idx t =
#endif
                if isValue && idx % sequenceCounterResetThreshold = 0 then
                    state.ResetCounters()

                pickler.Write state "item" t

            state.Formatter.BeginWriteObject pickler tag ObjectFlags.IsSequenceHeader
            
            // specialize enumeration
            let length =
                match values with
                | :? ('T []) as array ->
                    let n = array.Length
                    for i = 0 to n - 1 do
                        write i array.[i]
                    n

                | :? ('T list) as list ->
                    let rec writeLst i lst =
                        match lst with
                        | [] -> i
                        | t :: ts -> write i t ; writeLst (i+1) ts

                    writeLst 0 list
                | _ ->
                    let mutable i = 0
                    for t in values do
                        write i t
                        i <- i + 1
                    i

            state.Formatter.EndWriteObject()
            length

        let readTopLevelSequence (pickler : Pickler<'T>) (state : ReadState) (tag : string) (length : int) : IEnumerator<'T> =

            let isValue = pickler.TypeKind <= TypeKind.Value

            let read idx =
                if isValue && idx % sequenceCounterResetThreshold = 0 then
                    state.ResetCounters()
                
                pickler.Read state tag

            // read id
            let flags = state.Formatter.BeginReadObject pickler tag
            
            // read object header
            if flags <> ObjectFlags.IsSequenceHeader then
                let msg = "FsPickler: invalid stream data; expected sequence serialization."
                raise <| new SerializationException(msg)

            let cnt = ref 0
            let curr = ref Unchecked.defaultof<'T>
            {
                new System.Collections.Generic.IEnumerator<'T> with
                    member __.Current = !curr
                    member __.Current = box !curr
                    member __.Dispose () = (state :> IDisposable).Dispose()
                    member __.MoveNext () =
                        if !cnt < length then
                            curr := read !cnt
                            incr cnt
                            true
                        else
                            false

                    member __.Reset () = raise <| NotSupportedException()
            }

        let writeTopLevelSequenceUntyped (pickler : Pickler) (state : WriteState) (tag : string) (values : IEnumerable) : int =
            let unpacker =
                {
                    new IPicklerUnpacker<int> with
                        member __.Apply (p : Pickler<'T>) =
                            writeTopLevelSequence p state tag (values :?> IEnumerable<'T>)
                }

            pickler.Unpack unpacker

        let readTopLevelSequenceUntyped (pickler : Pickler) (state : ReadState) (tag : string) (length : int) : IEnumerator =
            let unpacker =
                {
                    new IPicklerUnpacker<IEnumerator> with
                        member __.Apply (p : Pickler<'T>) =
                            readTopLevelSequence p state tag length :> _
                }

            pickler.Unpack unpacker

#if EMIT_IL

        open System.Reflection.Emit
        open Nessos.FsPickler.Emit
    
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
                ilGen.Emit(OpCodes.Ldstr, f.Name)
                // load field value to the stack
                parent.Load ()
                ilGen.Emit(OpCodes.Ldfld, f)
                // perform serialization
                emitSerialize f.FieldType ilGen

        /// deserialize a collection of fields and store to parent object
        let emitDeserializeFields (fields : FieldInfo [])
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
                ilGen.Emit(OpCodes.Ldstr, f.Name)
                // deserialize and load to the stack
                emitDeserialize f.FieldType ilGen
                // assign value to the field
                ilGen.Emit(OpCodes.Stfld, f)

        /// serialize properties to the underlying stack
        let emitSerializeProperties (properties : PropertyInfo [])
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
                ilGen.Emit(OpCodes.Ldstr, p.Name)
                // load property value to the stack
                parent.Load ()
                ilGen.EmitCall(OpCodes.Call, m, null)
                // perform serialization
                emitSerialize m.ReturnType ilGen

        /// deserialize fields, pass to factory method and push to stack
        let emitDeserializeAndConstruct (factory : Choice<MethodInfo,ConstructorInfo>)
                                        (fparams : (Type * string) [])
                                        (reader : EnvItem<ReadState>)
                                        (picklers : EnvItem<Pickler []>)
                                        (ilGen : ILGenerator) =

            for i = 0 to fparams.Length - 1 do
                let p,tag = fparams.[i]
                // load typed pickler to the stack
                emitLoadPickler picklers p i ilGen
                // load reader to the stack
                reader.Load ()
                // load tag
                ilGen.Emit(OpCodes.Ldstr, tag)
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

        /// writes and integer
        let writeInt (writer : EnvItem<WriteState>) (tag : string) (n : EnvItem<int>) (ilGen : ILGenerator) =
            writer.Load ()
            ilGen.EmitCall(OpCodes.Call, bw, null) // load BinaryWriter
            ilGen.Emit(OpCodes.Ldstr, tag) // load tag
            n.Load () // load value
            ilGen.EmitCall(OpCodes.Callvirt, bwIntWriter, null) // perform write

        /// reads an integer and push to stack
        let readInt (reader : EnvItem<ReadState>) (tag : string) (ilGen : ILGenerator) =
            reader.Load ()
            ilGen.EmitCall(OpCodes.Call, br, null) // load BinaryReader
            ilGen.Emit(OpCodes.Ldstr, tag) // load tag
            ilGen.EmitCall(OpCodes.Callvirt, brIntReader, null) // perform read, push to stack
        
#endif
