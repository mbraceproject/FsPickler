module internal FsCoreSerializer.AdditionalFormatters

    open System
    open System.Reflection

    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.ExprShape

    open FsCoreSerializer.Utils
    open FsCoreSerializer.FsCoreFormatterImpl
    open FsCoreSerializer.Reflection

    let typeFormatter =
        let writer (w : Writer) (t : Type) =
            match t.AssemblyQualifiedName with
            | null ->
                if t.IsGenericParameter then
                    w.BW.Write true
                    w.BW.Write t.ReflectedType.AssemblyQualifiedName
                    w.BW.Write t.Name
                else 
                    failwithf "Could not encode type %A" t
            | aqn ->
                w.BW.Write false
                w.BW.Write aqn

        let reader (r : Reader) : Type =
            if r.BR.ReadBoolean() then
                // is generic type
                let reflectedType = r.BR.ReadString()
                let name = r.BR.ReadString()
                try
                    Type.GetType(reflectedType).GetGenericArguments() |> Array.find(fun a -> a.Name = name)
                with _ -> invalidOp (sprintf "Could not load type \"%s\"" reflectedType)
            else
                let aqn = r.BR.ReadString()
                let t = Type.GetType aqn
                if t = null then invalidOp (sprintf "Could not load type \"%s\"" aqn)
                else t

        mkFormatter CacheByRef false true false reader writer


    let memberInfoFormatter =
        let writer (w : Writer) (m : MemberInfo) =
            let m, genericArgs =
                match m.MemberType with
                | MemberTypes.Method ->
                    let meth = m :?> MethodInfo
                    if meth.IsGenericMethod && not meth.IsGenericMethodDefinition then
                        meth.GetGenericMethodDefinition() :> MemberInfo, meth.GetGenericArguments()
                    else m, [||]
                | _ -> m, [||]

            w.Write(typeFormatter, m.DeclaringType)
            w.BW.Write m.Name
            w.BW.Write (int m.MemberType)

            // if ambiguities emerge, also serialize the member signature
            if m.DeclaringType.GetMember(m.Name, m.MemberType, allFlags).Length > 1 then 
                w.BW.Write(m.ToString())

            Writer.writeSeq w typeFormatter genericArgs

        let reader (r : Reader) = 
            let declaringType = r.Read typeFormatter :?> Type
            let name = r.BR.ReadString()
            let memberType = enum<MemberTypes> (r.BR.ReadInt32())
            let overloads = declaringType.GetMember(name, memberType, allFlags)

            let m =
                match overloads with
                | [||] -> invalidOp (sprintf "Method \"%s\" not found" name)
                | [|m|] -> m
                | _ ->
                    let signature = r.BR.ReadString()
                    try
                        overloads |> Array.find (fun m -> m.ToString() = signature)
                    with _ -> invalidOp (sprintf "Method \"%s\" not found" signature)

            let genericArgs = Reader.readSeq<Type> r typeFormatter |> Array.ofSeq

            if genericArgs.Length > 0 then
                (m :?> MethodInfo).MakeGenericMethod(genericArgs) :> MemberInfo
            else m

        mkFormatter CacheByRef false true false reader writer

    let unionCaseInfoFormatter =
        let mkUci = lazy(
            let ctor = typeof<UnionCaseInfo>.GetConstructors(allFlags).[0]
            FSharpValue.PreComputeConstructor ctor)

        let writer (w : Writer) (uci : UnionCaseInfo) =
            w.Write(typeFormatter, uci.DeclaringType)
            w.BW.Write uci.Tag

        let reader (r : Reader) =
            let t = r.Read typeFormatter :?> Type
            let tag = r.BR.ReadInt32 ()
            mkUci.Value [| t ; tag |] :?> UnionCaseInfo

        mkFormatter CacheByRef false false false reader writer

    let varFormatter =
        let writer (w : Writer) (v : Var) =
            w.BW.Write v.Name
            w.Write(typeFormatter, v.Type)
            w.BW.Write v.IsMutable

        let reader (r : Reader) =
            new Var(r.BR.ReadString(), r.Read typeFormatter :?> Type, r.BR.ReadBoolean())

        // caching of variables is extremely imporant for quotation serialization
        Formatter.Create(writer, reader, cacheMode = CacheByEquality)

    let reflectionFormatters = [ typeFormatter ; memberInfoFormatter ; varFormatter ; unionCaseInfoFormatter ]

    let exprFormatter =
        let combReader = lazy(
            match Expr.Value 2 with
            | ShapeCombination(o,_) -> FsTuple(o.GetType())
            | _ -> failwith "impossible")

        let exprInfoFormatter = lazy(
            let exprInfoType = combReader.Value.Elements.[0]
            let init = mkFormatterCache(primitives @ reflectionFormatters)
            tryResolveFormatter memberInfoFormatter Map.empty init NoCaching exprInfoType |> Option.get)

        let rec exprWriter (w : Writer) (e : Expr) =
            match e with
            | ShapeVar v -> w.BW.Write 0uy ; w.Write(varFormatter, v)
            | ShapeLambda(v, b) ->
                w.BW.Write 1uy
                w.Write(varFormatter, v)
                exprWriter w b
            | ShapeCombination(o, exprs) ->
                let constInfo, attrs = 
                    let es = combReader.Value.Decompose o 
                    es.[0], es.[1] :?> Expr list

                w.BW.Write 2uy
                w.Write(exprInfoFormatter.Value, constInfo)
                exprsWriter w attrs
                exprsWriter w exprs

        and exprsWriter (w : Writer) (es : Expr list) =
            w.BW.Write es.Length
            for e in es do exprWriter w e

        let rec exprReader (r : Reader) =
            match r.BR.ReadByte() with
            | 0uy -> Expr.Var(r.Read varFormatter :?> Var)
            | 1uy -> Expr.Lambda(r.Read varFormatter :?> Var, exprReader r)
            | 2uy ->
                let exprInfo = r.Read exprInfoFormatter.Value
                let attrs = exprsReader r
                let exprs = exprsReader r
                RebuildShapeCombination(combReader.Value.Compose [|exprInfo ; attrs|], exprs)
            | _ -> invalidOp "Invalid or corrupt serialization."

        and exprsReader (r : Reader) =
            [ for _ in 1 .. r.BR.ReadInt32() -> exprReader r ]

        mkFormatter CacheByRef true false false exprReader exprWriter

    type ExprFormatter<'T> (resolver : Type -> Formatter option cell, t : Type) =
        do assert (t = typeof<Expr<'T>>)

        interface IFormatterFactory with
            member __.Create () =
                let writer (w : Writer) (e : Expr<'T>) = exprFormatter.Writer w (e :> _)
                let reader (r : Reader) =
                    let e = exprFormatter.Reader r :?> Expr
                    Expr.Cast<'T> e

                Formatter.Create(writer, reader, requiresExternalSerializer = true)

    let gExprFormatter = mkGenericFormatterDescr typedefof<Expr<_>> typedefof<ExprFormatter<_>>