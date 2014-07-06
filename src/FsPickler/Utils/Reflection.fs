module internal Nessos.FsPickler.Reflection

    open System
    open System.Reflection
    open System.Runtime.Serialization

    open Microsoft.FSharp.Reflection


    [<Literal>]
    let allFields = 
        BindingFlags.NonPublic ||| BindingFlags.Public ||| 
            BindingFlags.Instance ||| BindingFlags.FlattenHierarchy 

    [<Literal>]
    let allMembers =
        BindingFlags.NonPublic ||| BindingFlags.Public |||
            BindingFlags.Instance ||| BindingFlags.Static |||
                BindingFlags.FlattenHierarchy

    [<Literal>]
    let allStatic =
        BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static

    [<Literal>]
    let allConstructors = BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public


    type Delegate with
        static member CreateDelegate<'T when 'T :> Delegate> (m : MethodInfo) =
            System.Delegate.CreateDelegate(typeof<'T>, m) :?> 'T


    type Type with
        member t.GetGenericMethod(isStatic, name : string, genericArgCount : int, paramCount : int) =
            t.GetMethods(allMembers)
            |> Array.find(fun m ->
                m.Name = name 
                    && m.IsStatic = isStatic
                    && genericArgCount = m.GetGenericArguments().Length
                    && paramCount = m.GetParameters().Length)

        member t.TryGetConstructor(args : Type []) = 
            denull <| t.GetConstructor(allConstructors,null,args, [||])


    type MethodInfo with
        member m.GuardedInvoke(instance : obj, parameters : obj []) =
            try m.Invoke(instance, parameters)
            with :? TargetInvocationException as e when e.InnerException <> null ->
                reraise' e.InnerException

        member m.GetParameterTypes() = m.GetParameters() |> Array.map (fun p -> p.ParameterType)


    type ConstructorInfo with
        member c.GetParameterTypes() = c.GetParameters() |> Array.map (fun p -> p.ParameterType)

    let private memberNameRegex = new System.Text.RegularExpressions.Regex(@"[^a-zA-Z0-9]")
    type MemberInfo with
        /// normalizes member name into a serialializable string.
        member m.NormalizedName = memberNameRegex.Replace(m.Name, "")

    let containsAttr<'T when 'T :> Attribute> (m : MemberInfo) =
        m.GetCustomAttributes(typeof<'T>, true) |> Seq.isEmpty |> not

    let wrapDelegate<'Dele when 'Dele :> Delegate> (ms : MethodInfo []) =
        let wrap m = Delegate.CreateDelegate(typeof<'Dele>, m) :?> 'Dele
        Array.map wrap ms

    let inline getStreamingContext (x : ^T when ^T : (member StreamingContext : StreamingContext)) =
        ( ^T : (member StreamingContext : StreamingContext) x)

    let rec isISerializable (t : Type) =
        if typeof<ISerializable>.IsAssignableFrom t then true
        else
            match t.BaseType with
            | null -> false
            | bt -> isISerializable bt

    /// returns all methods of type `StreamingContext -> unit` and given Attribute
    let getSerializationMethods<'Attr when 'Attr :> Attribute> (ms : MethodInfo []) =
        let isSerializationMethod(m : MethodInfo) =
            not m.IsStatic && 
            containsAttr<'Attr> m &&
            m.ReturnType = typeof<System.Void> &&

                match m.GetParameters() with
                | [| p |] when p.ParameterType = typeof<StreamingContext> -> true
                | _ -> false

        ms |> Array.filter isSerializationMethod

    let isNullableType(t : Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Nullable<_>>

    /// walks up the type hierarchy, gathering all instance fields
    let gatherFields (t : Type) =
        // resolve conflicts, index by declaring type and field name
        let gathered = ref Map.empty<string * string, (* index *) int * FieldInfo>
        let i = ref 0

        let isSerializableField (f : FieldInfo) =
            not (f.IsLiteral || f.IsNotSerialized)

        let rec gather (t : Type) =
            let fields = t.GetFields(allFields)
            for f in fields do
                let k = f.DeclaringType.AssemblyQualifiedName, f.Name
                if isSerializableField f && not <| gathered.Value.ContainsKey k then
                    gathered := gathered.Value.Add(k, (!i, f))
                    incr i

            match t.BaseType with
            | null -> ()
            | t when t = typeof<obj> -> ()
            | bt -> gather bt

        do gather t

        gathered.Value 
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.sortBy fst // sort by index; this is to preserve field serialization ordering
        |> Seq.map snd
        |> Seq.toArray


    /// checks if instances of given type can be arrays
    let isAssignableFromArray =
        let getCanonicalType (t:Type) = 
            if t.IsGenericType then t.GetGenericTypeDefinition() 
            else t

        let arrayIfs =  typeof<int []>.GetInterfaces() |> Array.map getCanonicalType

        fun (t : Type) ->
            if t.IsAssignableFrom typeof<Array> then true
            elif t.IsArray then true
            elif not t.IsInterface then false
            else
                // check interface compatibility
                Array.exists ((=) (getCanonicalType t)) arrayIfs

    // Recursive type detection
    // ========================
    // Let 't1 -> t2' be the binary relation between types that denotes the statement 't1 contans a field of type t2'.
    // A type t is defined as being *recursive* iff either of the following properties hold:
    //     a) t is not sealed or ISerializable,
    //     b) there exists t -> t' such that t' is recursive
    //     c) there exists a chain (t -> t1 -> ... -> tn) so that t <: tn
    //
    // A type is recursive iff its instances admit recursive object graphs.
    //
    // F# union types are treated specially since recursive bindings cannot be created under normal circumstances
    // for instance, the
    //
    //      type Rec = Rec of Rec
    //
    // is flagged as non-recursive since defining instances of this type is impossible in F#.
    // However,
    //
    //     type Rec = { Rec : Rec }
    //
    // is flagged as recursive since a recursive definition is actually possible in F#.
    // Finally, the
    //
    //     type Func = Func of (int -> int)
    //
    // is flagged as recursive since recursive bindings *can* be made, for example
    // `let rec f = Func (fun x -> let (Func f0) = f in f0 x + 1)`

    let isRecursiveType excludeUnionRecTypes (t : Type) =
        let rec aux d (traversed : (int * bool * bool * Type) list) (t : Type) =

            if t.IsValueType then false
            elif typeof<MemberInfo>.IsAssignableFrom t then false
            elif isISerializable t then true
            else

            let recAncestors = traversed |> List.filter (fun (_,_,_,t') -> t.IsAssignableFrom t') 

            if recAncestors.Length > 0 then
                if excludeUnionRecTypes then
                    // recursive F# union types marked as 'recursive' only if intertwined with non-union(mutable) types
                    // e.g. 'type Peano = Zero | Succ of Peano ref' is recursive
                    let isRecType (d : int, isUnion : bool, _, _ : Type) =
                        if isUnion then
                            traversed |> List.exists (fun (i,_,isMutable,_) -> i > d && isMutable)
                        else
                            true
                                

                    List.exists isRecType recAncestors
                else
                    true

            elif t.IsArray || t.IsByRef || t.IsPointer then
                aux (d+1) ((d,false,true,t) :: traversed) <| t.GetElementType()
            elif FSharpType.IsUnion(t, allMembers) then
                FSharpType.GetUnionCases(t, allMembers)
                |> Seq.map (fun u -> u.GetFields() |> Seq.map (fun p -> p.PropertyType))
                |> Seq.concat
                |> Seq.distinct
                |> Seq.exists (aux (d+1) ((d,true,false,t) :: traversed))
#if OPTIMIZE_FSHARP
            // System.Tuple is not sealed, but inheriting is not an idiomatic pattern in F#
            elif FSharpType.IsTuple t then
                t.GetFields(allFields)
                |> Seq.map (fun f -> f.FieldType)
                |> Seq.distinct
                |> Seq.exists (aux (d+1) ((d,false,false,t) :: traversed))
#endif
            elif FSharpType.IsRecord(t, allMembers) then
                FSharpType.GetRecordFields(t, allMembers)
                |> Seq.map (fun p -> p.CanWrite, p.PropertyType)
                |> Seq.distinct
                |> Seq.exists (fun (isMutable, t') -> aux (d+1) ((d,false,isMutable,t) :: traversed) t')

            // leaves with open hiearchies are treated as recursive by definition
            elif not t.IsSealed then true
            else
                gatherFields t
                |> Seq.map (fun f -> f.FieldType)
                |> Seq.distinct
                |> Seq.exists (aux (d+1) ((d,false,true,t) :: traversed))

        aux 0 [] t


    //
    //  types like int * bool, int option, string, etc have object graphs of fixed scale
    //  types like arrays, rectypes, or non-sealed types can have instances of arbitrary graph size
    //

    let rec isOfFixedSize (t : Type) =
        if t.IsPrimitive then true
        elif t = typeof<string> then true
        elif typeof<MemberInfo>.IsAssignableFrom t then true

        elif t.IsArray then false
        elif isRecursiveType false t then false
        elif FSharpType.IsUnion(t, allMembers) then
            FSharpType.GetUnionCases(t, allMembers)
            |> Seq.collect(fun u -> u.GetFields())
            |> Seq.distinct
            |> Seq.forall(fun f -> isOfFixedSize f.PropertyType)
        else
            gatherFields t
            |> Seq.distinct
            |> Seq.forall (fun f -> isOfFixedSize f.FieldType)