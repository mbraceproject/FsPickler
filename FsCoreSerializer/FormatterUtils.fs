namespace FsCoreSerializer

    open System
    open System.Reflection
    open System.Linq.Expressions
    open System.Runtime.CompilerServices
    open System.Runtime.Serialization
    
    open FsCoreSerializer
    open FsCoreSerializer.Utils

    module internal FormatterUtils =

        let containsAttr<'T when 'T :> Attribute> (m : MemberInfo) =
            m.GetCustomAttributes(typeof<'T>, true) |> Seq.isEmpty |> not

        let fieldBindings = 
            BindingFlags.NonPublic ||| BindingFlags.Public ||| 
                BindingFlags.Instance ||| BindingFlags.FlattenHierarchy 

        let memberBindings =
            BindingFlags.NonPublic ||| BindingFlags.Public |||
                BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.FlattenHierarchy

        let ctorBindings = BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public

        let tryGetCtor (t : Type) (args : Type []) = denull <| t.GetConstructor(ctorBindings,null,args, [||]) 

        // builds type info enumeration out of reflection info
        let getTypeInfo (t : Type) =
            if t.IsPrimitive then TypeInfo.Primitive
            elif t.IsEnum then TypeInfo.Enum
            elif t.IsValueType then TypeInfo.Value
            elif t.IsArray then TypeInfo.Array
            elif t.IsSealed then TypeInfo.Sealed
            elif t.IsAbstract then TypeInfo.Abstract
            else TypeInfo.NonSealed

        // initialize a formatter from a typed set of lambdas
        let inline mkFormatter<'T> info useWithSubtypes cache (reader : Reader -> 'T) (writer : Writer -> 'T -> unit) =
            {
                Type = typeof<'T>
                TypeInfo = getTypeInfo typeof<'T>
                TypeHash = ObjHeader.computeHash typeof<'T>

                Write = fun bw o -> writer bw (o :?> 'T)
                Read = fun br -> reader br :> obj

                FormatterInfo = info
                CacheObj = cache
                UseWithSubtypes = useWithSubtypes
            }

        // compilation provision for methods that carry the OnSerializing, OnSerialized, etc attributes
        let preComputeSerializationMethods (declaringType : Type) (ms : MethodInfo []) =
            match ms with
            | [||] -> None
            | methods ->
                let dele = Expression.compile2<obj, StreamingContext, unit>(fun boxed sc ->
                    let unboxed = Expression.unbox declaringType boxed
                    let actions = methods |> Array.map (fun m -> Expression.Call(unboxed, m, sc) :> Expression)
                    Expression.Block actions |> Expression.returnUnit)

                Some dele

        let isSerializationMethod (m : MethodInfo) =
            not m.IsStatic &&
                match m.GetParameters() with
                | [| p |] when p.ParameterType = typeof<StreamingContext> -> true
                | _ -> false

        //
        //  internal read/write combinators
        //

        let inline write (w : Writer) (f : Formatter) (x : obj) =
            if f.TypeInfo <= TypeInfo.Value then f.Write w x
            elif not f.CacheObj then
                if obj.ReferenceEquals(x, null) then w.BW.Write true
                else
                    w.BW.Write false ; f.Write w x
            elif f.FormatterInfo = FormatterInfo.FSharpValue then
                if obj.ReferenceEquals(x, null) then w.BW.Write true
                else
                    do RuntimeHelpers.EnsureSufficientExecutionStack()

                    w.BW.Write false
                    f.Write w x
            else
                w.WriteObj(f, x)

        let inline read (r : Reader) (f : Formatter) =
            if f.TypeInfo <= TypeInfo.Value then f.Read r
            elif not f.CacheObj || f.FormatterInfo = FormatterInfo.FSharpValue then
                if r.BR.ReadBoolean() then null
                else f.Read r
            else
                r.ReadObj f

        // length passed as argument to avoid unecessary evaluations of sequence
        let inline writeSeq (w : Writer) (ef : Formatter) (length : int) (xs : seq<'T>) =
            w.BW.Write length
            for x in xs do write w ef x

        let inline readSeq<'T> (r : Reader) (ef : Formatter) =
            let length = r.BR.ReadInt32()
            let xs = Array.zeroCreate<'T> length
            for i = 0 to length - 1 do
                xs.[i] <- read r ef :?> 'T 
            xs

        // length passed as argument to avoid unecessary evaluations of sequence
        let inline writeKVPair (w : Writer) (kf : Formatter) (vf : Formatter) (length : int) (xs : ('K * 'V) seq) =
            w.BW.Write length
            for k,v in xs do
                write w kf k
                write w vf v

        let inline readKVPair<'K,'V> (r : Reader) (kf : Formatter) (vf : Formatter) =
            let length = r.BR.ReadInt32()
            let xs = Array.zeroCreate<'K * 'V> length
            for i = 0 to length - 1 do
                let k = read r kf :?> 'K
                let v = read r vf :?> 'V
                xs.[i] <- k,v

            xs

        let inline zipWrite (w : Writer) (formatters : Lazy<Formatter> []) (objs : obj []) : unit =
            for i = 0 to formatters.Length - 1 do
                write w formatters.[i].Value objs.[i]

        let inline zipRead (r : Reader) (formatters : Lazy<Formatter> []) : obj [] =
            let objs = Array.zeroCreate formatters.Length
            for i = 0 to formatters.Length - 1 do
                objs.[i] <- read r formatters.[i].Value

            objs