namespace FsPickler

    open System
    open System.IO
    open System.Reflection
    open System.Runtime.Serialization
    
    open FsPickler
    open FsPickler.Utils

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

        let inline isValue (f : Pickler) = f.TypeKind <= TypeKind.Value

        let inline write bypass (w : Writer) (f : Pickler<'T>) (x : 'T) =
            if bypass then f.Write w x
            else w.Write(f, x)

        let inline read bypass (r : Reader) (f : Pickler<'T>) =
            if bypass then f.Read r
            else r.Read f

        /// safely serialize strings, including nulls
        let inline writeStringSafe (bw : BinaryWriter) (x : string) =
            if obj.ReferenceEquals(x, null) then bw.Write true
            else
                bw.Write false ; bw.Write x

        /// safely deserialize strings, including nulls
        let inline readStringSafe (br : BinaryReader) =
            if br.ReadBoolean() then null
            else
                br.ReadString()

        let inline writeArray (w : Writer) (p : Pickler<'T>) (xs : 'T []) =
            w.BinaryWriter.Write xs.Length
            for x in xs do w.Write(p, x)

        let inline readArray (r : Reader) (p : Pickler<'T>) =
            let n = r.BinaryReader.ReadInt32 ()
            let arr = Array.zeroCreate<'T> n
            for i = 0 to n - 1 do
                arr.[i] <- r.Read p
            arr

        // length passed as argument to avoid unecessary evaluations of sequence
        let inline writeSeq (w : Writer) (ef : Pickler<'T>) (length : int) (xs : seq<'T>) =
            let isValue = isValue ef
            w.BinaryWriter.Write length
            for x in xs do write isValue w ef x

        // TODO : value types should probably be block deserialized
        let inline readSeq (r : Reader) (ef : Pickler<'T>) =
            let isValue = isValue ef
            let length = r.BinaryReader.ReadInt32()
            let xs = Array.zeroCreate<'T> length
            for i = 0 to length - 1 do
                xs.[i] <- read isValue r ef
            xs

        // length passed as argument to avoid unecessary evaluations of sequence
        let inline writeKVPairs (w : Writer) (kf : Pickler<'K>) (vf : Pickler<'V>) (length : int) (xs : ('K * 'V) seq) =
            let kIsValue = isValue kf
            let vIsValue = isValue vf
            w.BinaryWriter.Write length
            for k,v in xs do
                write kIsValue w kf k
                write vIsValue w vf v

        let inline readKVPairs (r : Reader) (kf : Pickler<'K>) (vf : Pickler<'V>) =
            let kIsValue = isValue kf
            let vIsValue = isValue vf
            let length = r.BinaryReader.ReadInt32()
            let xs = Array.zeroCreate<'K * 'V> length
            for i = 0 to length - 1 do
                let k = read kIsValue r kf
                let v = read vIsValue r vf
                xs.[i] <- k,v

            xs


        // equivalent implementations for client facade

        let writeSeq' (ef : Pickler<'T>) (w : Writer) (xs : 'T seq) : unit =
            let isValue = isValue ef
            match xs with
            | :? ('T []) as arr ->
                w.BinaryWriter.Write true
                w.BinaryWriter.Write arr.Length
                for i = 0 to arr.Length - 1 do
                    write isValue w ef (arr.[i])
            | :? ('T list) as list ->
                w.BinaryWriter.Write true
                w.BinaryWriter.Write list.Length
                let rec iter rest =
                    match rest with
                    | [] -> ()
                    | hd :: tl ->
                        write isValue w ef hd
                        iter tl

                iter list
            | _ ->
                w.BinaryWriter.Write false
                use e = xs.GetEnumerator()
                while e.MoveNext() do
                    w.BinaryWriter.Write true
                    write isValue w ef e.Current

                w.BinaryWriter.Write false

        let readSeq' (ef : Pickler<'T>) (r : Reader) : 'T seq =
            let isValue = isValue ef

            if r.BinaryReader.ReadBoolean() then
                let length = r.BinaryReader.ReadInt32()
                let arr = Array.zeroCreate<'T> length
                for i = 0 to length - 1 do
                    arr.[i] <- read isValue r ef
                arr :> _
            else
                let ra = new ResizeArray<'T> ()
                while r.BinaryReader.ReadBoolean() do
                    let next = read isValue r ef
                    ra.Add next

                ra :> _

        let writeKVPairs' (kf : Pickler<'K>) (vf : Pickler<'V>) (w : Writer) (xs : ('K * 'V) seq) : unit =
            let kIsValue = isValue kf
            let vIsValue = isValue vf
            match xs with
            | :? (('K * 'V) []) as arr ->
                w.BinaryWriter.Write true
                w.BinaryWriter.Write arr.Length
                for i = 0 to arr.Length - 1 do
                    let k,v = arr.[i]
                    write kIsValue w kf k
                    write vIsValue w vf v
            | :? (('K * 'V) list) as list ->
                w.BinaryWriter.Write true
                w.BinaryWriter.Write list.Length
                let rec iter rest =
                    match rest with
                    | [] -> ()
                    | (k,v) :: tl ->
                        write kIsValue w kf k
                        write vIsValue w vf v
                        iter tl

                iter list
            | _ ->
                w.BinaryWriter.Write false
                let e = xs.GetEnumerator()
                while e.MoveNext() do
                    w.BinaryWriter.Write true
                    let k,v = e.Current
                    write kIsValue w kf k
                    write vIsValue w vf v

                w.BinaryWriter.Write false


        /// Deserializes a sequence of key/value pairs from the underlying stream
        let readKVPairs' (kf : Pickler<'K>) (vf : Pickler<'V>) (r : Reader) =
            let kIsValue = isValue kf
            let vIsValue = isValue vf

            if r.BinaryReader.ReadBoolean() then
                let length = r.BinaryReader.ReadInt32()
                let arr = Array.zeroCreate<'K * 'V> length
                for i = 0 to length - 1 do
                    let k = read kIsValue r kf
                    let v = read vIsValue r vf
                    arr.[i] <- k,v
                arr :> seq<'K * 'V>
            else
                let ra = new ResizeArray<'K * 'V> ()
                while r.BinaryReader.ReadBoolean() do
                    let k = read kIsValue r kf
                    let v = read vIsValue r vf
                    ra.Add (k,v)

                ra :> seq<'K * 'V>