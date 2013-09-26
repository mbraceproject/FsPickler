module internal FsPickler.TypeShape

    // The following provides management logic for IGenericPicklerFactory implementations.
    // Since generic types come with multiple combinations of type variables and constraints,
    // specifying a predetermined set of generic formatter interfaces is simply not practical.
    //
    // All that is required of input formatters is to implement the IGenericPicklerFactory interface
    // (which contains no methods) and to contain an implementation of a non-static method
    //
    //         Create<'T1,..,'Tn> : (Type -> Lazy<Pickler>) -> Pickler
    //
    // The method is determined and executed through reflection, hence the possibility of runtime errors is real.
    // This has the advantage of not having to concern ourselves with type constraints at this stage, where they do
    // not really matter. 
    //
    // Furthermore, a type shape scheme is used in order for the formatter resolver to assign the best matching
    // generic formatter given an input type. 
    // For instance, given input (int * int) [] , the resolver will determine that a generic formatter of 
    // shape ('X * 'X) [] is more suitable than one of shape 'X []

    open System
    open System.Reflection
    open System.Runtime.Serialization

    open FsPickler
    open FsPickler.Utils

    // embed peano arithmetic in System.Type
    type Peano =
        abstract Value : int
    and Zero () = 
        interface Peano with
            member __.Value = 0
    and Succ<'T when 'T :> Peano> () =
        static let value = 1 + Activator.CreateInstance<'T>().Value
        interface Peano with
            member __.Value = value

    let zero = typeof<Zero>
    let succ (t : Type) = typedefof<Succ<_>>.MakeGenericType [| t |]

    let getPeanoVars (n : int) =
        [|
            let current = ref zero
            for i = 0 to n-1 do
                yield !current
                current := succ !current
        |]

    let (|PeanoType|_|) t =
        if typeof<Peano>.IsAssignableFrom t then 
            Some (Activator.CreateInstance t :?> Peano).Value
        else
            None

    let (|GenericType|_|) (t : Type) =
        if t.IsGenericType then Some(t.GetGenericTypeDefinition(), t.GetGenericArguments())
        else None

    type TypeShape =
        | Var of int
        | Named of Type
        | Array of int * TypeShape * Type
        | Ref of (* byRef *) bool * TypeShape * Type
        | Generic of Type * TypeShape list * Type
    with
        override s.ToString () =
            let rec print s =
                match s with
                | Var i -> sprintf "'T%d" i
                | Named t -> t.Name
                | Array (rank, nested, _) -> 
                    let name = print nested
                    match rank with
                    | 1 -> name + " []"
                    | 2 -> name + " [,]"
                    | 3 -> name + " [,,]"
                    | 4 -> name + " [,,,]"
                    | _ -> name + " array"
                | Ref (true, nested, _) -> print nested + "&"
                | Ref (false, nested, _) -> print nested + "*"
                | Generic (t, tyArgs, _) ->
                    let name = t.Name.Split('`').[0]
                    let args = Seq.map print tyArgs |> String.concat ", "
                    name + "<" + args + ">"

            print s

        member s.Type =
            match s with
            | Var _ -> invalidOp "cannot reify variables."
            | Named t -> t
            | Array(_,_,t) -> t
            | Ref(_,_,t) -> t
            | Generic(_,_,t) -> t

        member s.Size =
            let rec getSize s =
                match s with
                | Var _ -> 0
                | Named _ -> 1
                | Array(_,s,_) -> 1 + getSize s
                | Ref(_,s,_) -> 1 + getSize s
                | Generic(_,ss,_) -> 1 + List.sumBy getSize ss

            getSize s

        member s.FreeVars =
            let rec gather gathered ss =
                match ss with
                | Var i :: rest -> gather (i :: gathered) rest
                | Named _ :: rest -> gather gathered rest
                | Array(_,s,_) :: rest -> gather gathered (s :: rest)
                | Ref(_,s,_) :: rest -> gather gathered (s :: rest)
                | Generic(_,ss,_) :: rest -> gather gathered (ss @ rest)
                | [] -> gathered

            // pass to set to sort and distinct
            gather [] [s] |> set |> Set.toList

        static member OfType (t : Type) =
            let rec unfold (t : Type) =
                match t with
                | PeanoType i -> Var i
                | GenericType (gt, gas) -> Generic(gt, Seq.map unfold gas |> Seq.toList, t)
                | _ when t.IsArray -> Array(t.GetArrayRank(), unfold <| t.GetElementType(), t)
                | _ when t.IsByRef -> Ref(true, unfold <| t.GetElementType(), t)
                | _ when t.IsPointer -> Ref(false, unfold <| t.GetElementType(), t)
                | _ -> Named t

            unfold t

        // matches a type pattern against a given shape
        // if matching, will return collection of subshapes that correspond to substituted variables in pattern
        // in other words,
        //    Some [t1; .. ; tn] = tryMatch p t  iff  p[ti/xi] = t

        static member TryMatch (pattern : TypeShape) (target : TypeShape) =
            let rec traverse (varIdx : Map<int, TypeShape> option) (pat : TypeShape) (tgt : TypeShape) =
                match varIdx with 
                | None -> None 
                | Some map ->
                    match pat, tgt with
                    | _, Var _ -> failwith "target shape cannot contain variables."
                    | Var i, tgt ->
                        match map.TryFind i with
                        | Some tgt' when tgt = tgt' -> varIdx
                        | Some _ -> None // variable matches to different shapes in target, incompatible
                        | None -> Some <| map.Add(i, tgt)
                    | Named t, Named t' when t = t' -> varIdx
                    | Array(i,x,_), Array(j,y,_) when i = j -> traverse varIdx x y
                    | Ref(b,x,_), Ref(b',y,_) when b = b' -> traverse varIdx x y
                    | Generic(x, xargs, _), Generic(y, yargs, _) when x = y ->
                        (xargs, yargs)
                        ||> Seq.zip
                        |> Seq.fold (fun varIdx (x,y) -> traverse varIdx x y) varIdx
                    | _ -> 
                        None

            match traverse (Some Map.empty) pattern target with
            | None -> None
            | Some map ->
                map |> Map.toSeq |> Seq.sortBy fst |> Seq.map snd |> Seq.toArray |> Some



    type OverwriteBehavior = Overwrite | Discard | Fail

    // an immutable dictionary implementation that returns the best matching shape entry given a type
    // a best match is determined by minimizing the distance w.r.t. a naturally occuring 1-norm
    // over shape trees.
    type ShapeMap<'T> private (map : Map<string, (TypeShape * 'T) list>) =

        static let topLevelId =
            function
            | Var _ -> "0"
            | Array _ -> "1"
            | Ref(true,_,_) -> "2"
            | Ref(false,_,_) -> "3"
            | Named t -> t.AssemblyQualifiedName
            | Generic(t,_,_) -> t.AssemblyQualifiedName

        member __.TryFind (t : Type) =
            let shape = TypeShape.OfType t

            // lookup based on top level shape
            match map.TryFind <| topLevelId shape with
            | None ->
                // handle top level patterns
                match map.TryFind <| topLevelId (Var 0) with
                | None | Some [] -> None
                | Some ((_,t) :: _) -> Some([|shape|], t)
            | Some candidates ->
                // make linear search on found pairs
                let matches =
                    candidates
                    |> List.choose (fun (s,t) -> TypeShape.TryMatch s shape |> Option.map (fun m -> m,t))

                if Seq.isEmpty matches then None
                else
                    // select by minimizing wrt to 1-distance
                    matches |> List.minBy (fun (m,_) -> m |> Array.sumBy (fun s -> s.Size)) |> Some

        member m.Add (shape : TypeShape, value : 'T, overwrite) =
            let shapeId = topLevelId shape
            let content = defaultArg (map.TryFind shapeId) []
            let updated = (shape, value) :: (content |> List.filter (fun (s,_) -> s <> shape))
            let isRegisteredShape = updated.Length = content.Length
            
            match overwrite with
            | Discard when isRegisteredShape -> m
            | Fail when isRegisteredShape ->
                invalidOp "A generic formatter of equivalent shape already exists."
            | _ ->
                ShapeMap<_>(map.Add(shapeId, updated))

        member __.Add (t : Type, value : 'T, overwrite) = __.Add(TypeShape.OfType t, value, overwrite)
        member __.Remove (shape : TypeShape) =
            let shapeId = topLevelId shape
            let content = defaultArg (map.TryFind shapeId) []
            let updated = (content |> List.filter (fun (s,_) -> s <> shape))

            ShapeMap<_>(map.Add(shapeId, updated))

        member __.ToList () = map |> Map.toSeq |> Seq.map snd |> Seq.concat |> Seq.toList

        static member Empty = ShapeMap<'T>(Map.empty)

    /// an immutable index for generic formatters
    type GenericPicklerIndex internal (shapeMap : ShapeMap<IGenericPicklerFactory * MethodInfo>) =

        static member Empty = new GenericPicklerIndex(ShapeMap.Empty)

        member i.AddGenericPickler(gf : IGenericPicklerFactory, overwrite) =
            let t = gf.GetType()
            let tryGetCreateMethod (t : Type) =
                t.GetMethods(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
                |> Seq.tryFind(fun m -> 
                        m.Name = "Create" &&  m.IsGenericMethod 
                        &&
                            (let ts = m.GetParameters() |> Array.map (fun p -> p.ParameterType)
                                in ts = [| typeof<IPicklerResolver> |])
                        &&
                            typeof<Pickler>.IsAssignableFrom m.ReturnType)

            let createMethod =
                match tryGetCreateMethod t with
                | Some m -> Some m
                | None ->
                    // Create method may be hidden in intermediate interfaces
                    t.GetInterfaces() 
                    |> Seq.filter (fun i -> typeof<IGenericPicklerFactory>.IsAssignableFrom i)
                    |> Seq.tryPick tryGetCreateMethod

            match createMethod with
            | None -> 
                SerializationException(sprintf "IGenericPickler: instance '%s' does not implement a factory method 'Create<..> : (Type -> Lazy<Pickler>) -> Pickler'" t.Name)
                |> raise
            | Some m ->
                // apply Peano type variables to formatter in order to extrapolate the type shape
                let tyVars = getPeanoVars (m.GetGenericArguments().Length)
                let resolver =
                    {
                        new IPicklerResolver with
                            member __.Resolve (t : Type) = ReflectionPicklers.AbstractPickler.CreateUntyped t
                            member __.Resolve<'T> () = ReflectionPicklers.AbstractPickler.Create<'T> ()
                    }

                let m0 =
                    try m.MakeGenericMethod tyVars
                    with :? System.ArgumentException & InnerExn (:? System.Security.VerificationException) ->
                        SerializationException(sprintf "IGenericPickler: instance '%s' contains unsupported type constraint." t.Name)
                        |> raise

                let fmt = m0.Invoke(gf, [| resolver :> obj|]) :?> Pickler
                let shape = TypeShape.OfType fmt.Type
                let fvs = shape.FreeVars
                if fvs.Length < tyVars.Length then
                    let missingVar =
                        match fvs |> Seq.mapi (fun i n -> (i,n)) |> Seq.tryPick (fun (i,j) -> if i <> j then Some i else None) with
                        | None -> fvs.Length
                        | Some i -> i

                    SerializationException(sprintf "IGenericPickler: type variable #%d in instance '%s' is not used in pattern." missingVar t.Name)
                    |> raise

                match shape with
                | Var _ -> 
                    SerializationException(sprintf "IGenericPickler: pattern in instance '%s' has type variable at top level position." t.Name)
                    |> raise
                | _ -> ()

                new GenericPicklerIndex(shapeMap.Add(shape, (gf,m), overwrite))

        member i.AddGenericPicklers(gfs : seq<IGenericPicklerFactory>, overwrite) =
            (i,gfs) ||> Seq.fold (fun i gf -> i.AddGenericPickler(gf,overwrite))

        member i.TryResolveGenericPickler(t : Type, resolver : IPicklerResolver) : Pickler option =
            match shapeMap.TryFind t with
            | None -> None
            | Some (shapes, (gf,m)) ->
                // get hole instances that match given pattern
                let types = shapes |> Array.map (fun s -> s.Type)
                Some(m.MakeGenericMethod(types).Invoke(gf, [| resolver :> obj |]) :?> Pickler)

        member __.GetEntries() =
            shapeMap.ToList()
            |> List.map (fun (shape,(gf,_)) -> gf.GetType(), shape.ToString())