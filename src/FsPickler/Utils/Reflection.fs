module internal MBrace.FsPickler.Reflection

open System
open System.Text.RegularExpressions
open System.Reflection
open System.Runtime.Serialization

open Microsoft.FSharp.Reflection

let allFields = 
    BindingFlags.NonPublic ||| BindingFlags.Public ||| 
        BindingFlags.Instance ||| BindingFlags.FlattenHierarchy 

let allMembers =
    BindingFlags.NonPublic ||| BindingFlags.Public |||
        BindingFlags.Instance ||| BindingFlags.Static |||
            BindingFlags.FlattenHierarchy

let allStatic = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static

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
        with :? TargetInvocationException as e when isNotNull e.InnerException ->
            reraise' e.InnerException

    member m.GetParameterTypes() = m.GetParameters() |> Array.map (fun p -> p.ParameterType)

type ConstructorInfo with
    member c.GetParameterTypes() = c.GetParameters() |> Array.map (fun p -> p.ParameterType)

let private memberNameRegex = new Regex(@"[^a-zA-Z0-9]", RegexOptions.Compiled)
let getNormalizedFieldName i (text : string) = 
    match memberNameRegex.Replace(text, "") with
    | name when String.IsNullOrEmpty name -> sprintf "anonfield%d" i
    | name -> name

let containsAttr<'T when 'T :> Attribute> (m : MemberInfo) =
    m.GetCustomAttributes(typeof<'T>, true) |> Seq.isEmpty |> not

let tryGetAttr<'T when 'T :> Attribute> (m : MemberInfo) =
    m.GetCustomAttributes(typeof<'T>, true) 
    |> Seq.tryPick (function :? 'T as t -> Some t | _ -> None)

let wrapDelegate<'Dele when 'Dele :> Delegate> (ms : MethodInfo []) =
    let wrap m = Delegate.CreateDelegate(typeof<'Dele>, m) :?> 'Dele
    Array.map wrap ms

let inline getStreamingContext (x : ^T when ^T : (member StreamingContext : StreamingContext)) =
    ( ^T : (member StreamingContext : StreamingContext) x)

/// correctly resolves if type is assignable to interface
let rec isAssignableFrom (interfaceTy : Type) (ty : Type) =
    if interfaceTy.IsAssignableFrom ty then true
    else
        match ty.BaseType with
        | null -> false
        | bt -> isAssignableFrom interfaceTy bt

let isRoslynReplSubmissionType (t : Type) =
    if t.Assembly.GetName().Name.StartsWith "ℛ*" then
        let rec aux (t : Type) =
            if t.Name.StartsWith "Submission#" then true
            else
                match t.DeclaringType with
                | null -> false
                | dt -> aux dt

        aux t
    else
        false

let isLinqEnumerable(t : Type) =
    isAssignableFrom typeof<System.Collections.IEnumerable> t && t.FullName.StartsWith "System.Linq"

let isReflectionSerializable (t : Type) = t.IsSerializable || containsAttr<SerializableAttribute> t

let isISerializable (t : Type) = isAssignableFrom typeof<ISerializable> t
let tryGetISerializableCtor (t : Type) = t.TryGetConstructor [| typeof<SerializationInfo> ; typeof<StreamingContext> |]

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

let isExceptionDispatchInfo (t : Type) =
    t.Assembly = typeof<int>.Assembly && t.FullName = "System.Runtime.ExceptionServices.ExceptionDispatchInfo"

/// walks up the type hierarchy, gathering all instance members
let gatherMembers (t : Type) =
    // resolve conflicts, index by declaring type and field name
    let gathered = ref Map.empty<string * string, MemberInfo>

    let rec gather (t : Type) =
        let members = t.GetMembers(allFields)
        for m in members do
            let k = m.DeclaringType.AssemblyQualifiedName, m.ToString()
            if not <| gathered.Value.ContainsKey k then
                gathered := gathered.Value.Add(k, m)

        match t.BaseType with
        | null -> ()
        | t when t = typeof<obj> -> ()
        | bt -> gather bt

    do gather t

    gathered.Value 
    |> Map.toSeq
    |> Seq.toArray
    // sort by name since Type.GetMembers() is non deterministic an unordered. https://github.com/mbraceproject/FsPickler/issues/92
    // Since names are not unique in class hierarchies, we use the full key here being (assemblyQualifiedName * FieldName).
    |> Array.sortBy (fun (k,_) -> k) 
    |> Array.map snd

let gatherSerializedFields (t : Type) =
    let isSerializedField (m : MemberInfo) =
        match m with
        | :? FieldInfo as f when not (f.IsLiteral || f.IsNotSerialized) -> Some f
        | _ -> None

    t |> gatherMembers |> Array.choose isSerializedField
            



// Recursive type detection
// ========================
// Let 't1 -> t2' be the binary relation between types that denotes the statement 't1 contans a field of type t2'.
// A type t is defined as being *recursive* iff either of the following properties hold:
//     a) t is not sealed or ISerializable,
//     b) there exists t -> t' such that t' is recursive
//     c) there exists t ->* t' so that t <: t'
//
// A reference type is recursive iff its instances admit cyclic object graphs.

exception PolymorphicRecursiveException of Type

/// Detect polymorphic recursion patterns
let isPolymorphicRecursive (t : Type) =
    let rec aux traversed (t : Type) =
        if t.IsGenericType then
            let gt = t.GetGenericTypeDefinition()

            // eliminate simple recursion patterns
            if traversed |> List.exists((=) t) then None else

            // determine if polymorphic recursive
            let polyRecAncestors =
                traversed
                |> List.filter (fun t' -> t'.GetGenericTypeDefinition() = gt)
                |> List.length

            if polyRecAncestors > 1 then Some gt else

            // traverse fields
            if FSharpType.IsUnion(t, allMembers) then
                FSharpType.GetUnionCases(t, allMembers)
                |> Seq.collect (fun u -> u.GetFields())
                |> Seq.map (fun p -> p.PropertyType)
                |> Seq.distinct
                |> Seq.tryPick (aux (t :: traversed))
            else
                gatherSerializedFields t
                |> Seq.map (fun f -> f.FieldType)
                |> Seq.distinct
                |> Seq.tryPick (aux (t :: traversed))

        elif t.IsArray || t.IsByRef || t.IsPointer then
            aux traversed <| t.GetElementType()
        else
            None

    if t.IsGenericType then
        let gt = t.GetGenericTypeDefinition()
        match aux [] gt with
        | Some gt' when gt = gt' -> true
        | _ -> false
    else
        false

/// Checks if type is 'recursive' according to above definition
/// Note that type must additionally be a reference type for this to be meaningful.
let isRecursiveType openHierarchiesOnly (t : Type) =
    let rec aux (traversed : Type list) (t : Type) =
        if t.IsPrimitive then false
        elif typeof<MemberInfo>.IsAssignableFrom t then false
        // check for cyclic type dependencies
        elif not openHierarchiesOnly && traversed |> List.exists (fun t' -> isAssignableFrom t t') then true
        elif openHierarchiesOnly && traversed |> List.exists (fun t' -> t = t') then false else

        // detect polymorphic recursion patterns
        if isPolymorphicRecursive t then raise <| PolymorphicRecursiveException t

        // continue with traversal
        if t.IsArray || t.IsByRef || t.IsPointer then
            aux (t :: traversed) <| t.GetElementType()
        elif FSharpType.IsUnion(t, allMembers) then
            FSharpType.GetUnionCases(t, allMembers)
            |> Seq.collect (fun u -> u.GetFields())
            |> Seq.map (fun p -> p.PropertyType)
            |> Seq.distinct
            |> Seq.exists (aux (t :: traversed))
#if OPTIMIZE_FSHARP
        // System.Tuple is not sealed, but inheriting is not an idiomatic pattern in F#
        elif FSharpType.IsTuple t then
            FSharpType.GetTupleElements t
            |> Seq.distinct
            |> Seq.exists (aux (t :: traversed))
#endif
        // leaves with open hiearchies are treated as recursive by definition
        elif not t.IsSealed then true
        else
            gatherSerializedFields t
            |> Seq.map (fun f -> f.FieldType)
            |> Seq.distinct
            |> Seq.exists (aux (t :: traversed))

    aux [] t


//
//  types like int * bool, int option, etc have object graphs of fixed size
//  types like strings, arrays, rectypes, or non-sealed types can have instances of arbitrary graph size
//

let isOfFixedSize isRecursive (t : Type) =
    let rec aux (t : Type) =
        if t.IsPrimitive then true
        elif typeof<MemberInfo>.IsAssignableFrom t then true
        elif t = typeof<string> then false
        elif t.IsArray then false
        elif FSharpType.IsUnion(t, allMembers) then
            FSharpType.GetUnionCases(t, allMembers)
            |> Seq.collect(fun u -> u.GetFields())
            |> Seq.map (fun f -> f.PropertyType)
            |> Seq.distinct
            |> Seq.forall aux
        else
            gatherSerializedFields t
            |> Seq.map (fun f -> f.FieldType)
            |> Seq.distinct
            |> Seq.forall aux

    if isRecursive then false
    else
        // not recursive, check for arrays and strings in the type graph
        aux t