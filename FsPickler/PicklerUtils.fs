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