namespace FsCoreSerializer

    open System
    open System.Reflection
    open System.Runtime.Serialization
    
    open FsCoreSerializer
    open FsCoreSerializer.Utils

    module internal FormatterUtils =

        // initialize a formatter from a typed set of lambdas
        let inline mkFormatter<'T> (info:FormatterInfo) (useWithSubtypes:bool) (cache:bool) 
                                            (reader : Reader -> 'T) (writer : Writer -> 'T -> unit) =

            new Formatter<'T>(reader, writer, info, cacheObj = cache, useWithSubtypes = useWithSubtypes)

        let fieldBindings = 
            BindingFlags.NonPublic ||| BindingFlags.Public ||| 
                BindingFlags.Instance ||| BindingFlags.FlattenHierarchy 

        let memberBindings =
            BindingFlags.NonPublic ||| BindingFlags.Public |||
                BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.FlattenHierarchy

        let ctorBindings = BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public

        type Type with
            member t.GetGenericMethod(isStatic, name : string, genericArgCount : int, paramCount : int) =
                t.GetMethods(memberBindings)
                |> Array.find(fun m ->
                    m.Name = name && m.IsStatic = isStatic 
                        && genericArgCount = m.GetGenericArguments().Length
                        && paramCount = m.GetParameters().Length)

            member t.TryGetConstructor(args : Type []) = denull <| t.GetConstructor(ctorBindings,null,args, [||])



        let containsAttr<'T when 'T :> Attribute> (m : MemberInfo) =
            m.GetCustomAttributes(typeof<'T>, true) |> Seq.isEmpty |> not

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

        let inline getStreamingContext (x : ^T when ^T : (member StreamingContext : StreamingContext)) =
            ( ^T : (member StreamingContext : StreamingContext) x)

        //
        //  internal read/write combinators
        //

        let inline isValue (f : Formatter) = f.TypeInfo <= TypeInfo.Value

        let inline write bypass (w : Writer) (f : Formatter<'T>) (x : 'T) =
            if bypass then f.Write w x
            else w.Write(f, x)

        let inline read bypass (r : Reader) (f : Formatter<'T>) =
            if bypass then f.Read r
            else r.Read f

        // length passed as argument to avoid unecessary evaluations of sequence
        let inline writeSeq (w : Writer) (ef : Formatter<'T>) (length : int) (xs : seq<'T>) =
            let isValue = ef.TypeInfo <= TypeInfo.Value
            w.BW.Write length
            for x in xs do write isValue w ef x

        // TODO : value types should probably be block deserialized
        let inline readSeq<'T> (r : Reader) (ef : Formatter<'T>) =
            let isValue = ef.TypeInfo <= TypeInfo.Value
            let length = r.BR.ReadInt32()
            let xs = Array.zeroCreate<'T> length
            for i = 0 to length - 1 do
                xs.[i] <- read isValue r ef
            xs

        // length passed as argument to avoid unecessary evaluations of sequence
        let inline writeKVPair (w : Writer) (kf : Formatter<'K>) (vf : Formatter<'V>) (length : int) (xs : ('K * 'V) seq) =
            let kIsValue = kf.TypeInfo <= TypeInfo.Value
            let vIsValue = vf.TypeInfo <= TypeInfo.Value
            w.BW.Write length
            for k,v in xs do
                write kIsValue w kf k
                write vIsValue w vf v

        let inline readKVPair<'K,'V> (r : Reader) (kf : Formatter<'K>) (vf : Formatter<'V>) =
            let kIsValue = kf.TypeInfo <= TypeInfo.Value
            let vIsValue = vf.TypeInfo <= TypeInfo.Value
            let length = r.BR.ReadInt32()
            let xs = Array.zeroCreate<'K * 'V> length
            for i = 0 to length - 1 do
                let k = read kIsValue r kf
                let v = read vIsValue r vf
                xs.[i] <- k,v

            xs