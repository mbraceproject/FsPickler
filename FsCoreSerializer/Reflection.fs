namespace FsCoreSerializer

    open System
    open System.Reflection
    open System.Reflection.Emit
    open Microsoft.FSharp.Reflection

    open FsCoreSerializer.Utils

    // Code largely taken from Kurt Schelfthout's FsReflect project: 
    // http://fortysix-and-two.blogspot.gr/2012/05/making-f-reflection-faster.html
    // added support for Union tag readers, tuples and exceptions

    //This uses some simple dynamic IL generation,
    //and a clever technique desribed by Jon Skeet here:
    //https://msmvps.com/blogs/jon_skeet/archive/2008/08/09/making-reflection-fly-and-exploring-delegates.aspx
    //The result of all this hackery is that the four exposed reflection operations
    //are 10x to 20x faster.
    module private ReflectionImpl =

        let allFlags = (enum<BindingFlags> Int32.MaxValue) &&& (~~~ BindingFlags.IgnoreCase)

        let isOptionTy (t : Type) =
            t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option>

        let pushParams offset (generator:ILGenerator) (paramTypes:seq<Type>) =
            let castFromObject typ =
                if typ <> typeof<obj> then
                    if typ.IsValueType then
                        generator.Emit(OpCodes.Unbox_Any, typ)
                    else
                        generator.Emit(OpCodes.Castclass, typ)

            paramTypes
            |> Seq.iteri (fun i paramType -> 
                            generator.Emit(OpCodes.Ldarg, 0)
                            generator.Emit(OpCodes.Ldc_I4, offset + i)
                            generator.Emit(OpCodes.Ldelem_Ref)
                            castFromObject paramType) 
                            
        let inline preComputeConstructor(ctorInfo : ConstructorInfo) =
            let meth = new DynamicMethod( "ctor", MethodAttributes.Static ||| MethodAttributes.Public,
                                CallingConventions.Standard, typeof<obj>, [| typeof<obj[]> |],
                                ctorInfo.DeclaringType,
                                true )
            let generator = meth.GetILGenerator()
            let paramTypes = ctorInfo.GetParameters() |> Seq.map (fun pi -> pi.ParameterType)

            pushParams 0 generator paramTypes
            generator.Emit(OpCodes.Newobj, ctorInfo)
            generator.Emit(OpCodes.Ret)

            let dele = meth.CreateDelegate(typeof<Func<obj[],obj>>) :?> Func<obj[],obj>

            dele.Invoke

    
        let preComputeRecordContructor(recordType:Type,bindingFlags:BindingFlags option) =
            assert FSharpType.IsRecord(recordType, ?bindingFlags=bindingFlags)
            let ctorInfo = FSharpValue.PreComputeRecordConstructorInfo(recordType,?bindingFlags=bindingFlags)
            preComputeConstructor ctorInfo

        let preComputeExceptionConstructor(exnType : Type, bindingFlags:BindingFlags option) =
            assert FSharpType.IsExceptionRepresentation(exnType, ?bindingFlags = bindingFlags)
            let signature = FSharpType.GetExceptionFields(exnType, ?bindingFlags = bindingFlags) |> Array.map(fun f -> f.PropertyType)
            let ctors = match bindingFlags with Some f -> exnType.GetConstructors f | None -> exnType.GetConstructors()
            match ctors |> Array.tryFind(fun ctor -> signature = (ctor.GetParameters() |> Array.map(fun p -> p.ParameterType))) with
            | None -> invalidArg "exnType" "The exception type is private. You must specify BindingFlags.NonPublic to access private type representations."
            | Some ctorInfo -> preComputeConstructor ctorInfo

        let preComputeTupleConstructor(tuple : Type) : obj [] -> obj =
            let meth = new DynamicMethod( "ctor", MethodAttributes.Static ||| MethodAttributes.Public,
                                             CallingConventions.Standard, typeof<obj>, [| typeof<obj[]> |],
                                             tuple,
                                             true )
            let generator = meth.GetILGenerator()

            let rec traverse offset (tuple : Type) =
                let ctorInfo, nested = FSharpValue.PreComputeTupleConstructorInfo tuple
                let paramTypes = ctorInfo.GetParameters() |> Array.map (fun pi -> pi.ParameterType)

                match nested with
                | None -> pushParams offset generator paramTypes
                | Some nested ->
                    let n = paramTypes.Length
                    pushParams offset generator (Seq.take (n-1) paramTypes)
                    traverse (offset + n - 1) nested

                generator.Emit(OpCodes.Newobj, ctorInfo)

            let invoker =
                traverse 0 tuple
                generator.Emit(OpCodes.Ret)
                meth.CreateDelegate(typeof<Func<obj[],obj>>) :?> Func<obj[],obj>
            invoker.Invoke
    
        let preComputeUnionConstructor(unionCaseInfo:UnionCaseInfo, bindingFlags:BindingFlags option) =
            let methodInfo = FSharpValue.PreComputeUnionConstructorInfo(unionCaseInfo, ?bindingFlags=bindingFlags)
            let targetType = methodInfo.DeclaringType
            assert FSharpType.IsUnion(targetType, ?bindingFlags=bindingFlags)
            let meth = new DynamicMethod( "invoke", MethodAttributes.Static ||| MethodAttributes.Public,
                                            CallingConventions.Standard, typeof<obj>,
                                            [| typeof<obj[]> |], targetType, true )
            let paramTypes = methodInfo.GetParameters() |> Seq.map (fun pi -> pi.ParameterType)
            let generator = meth.GetILGenerator()

            let invoker =
                pushParams 0 generator paramTypes
                generator.Emit(OpCodes.Call, methodInfo)
                generator.Emit(OpCodes.Ret)
                meth.CreateDelegate(typeof<Func<obj[],obj>>) :?> Func<obj[],obj>

            invoker.Invoke

        // bundle multiple property getter methods in one dynamic method
        let preComputeGetters (declaringType : Type) (methods : MethodInfo []) : obj -> obj [] =
            assert(methods |> Array.forall (fun m -> declaringType = m.DeclaringType && m.GetParameters().Length = 0))
            if methods.Length = 0 then (fun o -> [||]) else

            let meth = new DynamicMethod("fieldEvaluator", MethodAttributes.Static ||| MethodAttributes.Public,
                                             CallingConventions.Standard, typeof<obj []>, 
                                             [|typeof<obj>|], declaringType, true)
            let generator = meth.GetILGenerator()

            let unboxed = generator.DeclareLocal(declaringType)
            let arr = generator.DeclareLocal(typeof<obj []>)

            // init obj array
            generator.Emit(OpCodes.Ldarg_0)
            generator.Emit(OpCodes.Unbox_Any, declaringType)
            generator.Emit(OpCodes.Stloc, unboxed)

            // unbox input
            generator.Emit(OpCodes.Ldc_I4, methods.Length)
            generator.Emit(OpCodes.Newarr, typeof<obj>)
            generator.Emit(OpCodes.Stloc, arr)

            let computeProperty (idx : int) (m : MethodInfo) =
                // arr.[idx] <- p.GetValue(o) :> obj
                generator.Emit(OpCodes.Ldloc, arr)
                generator.Emit(OpCodes.Ldc_I4, idx)

                // call property getter
                generator.Emit(OpCodes.Ldloc, unboxed)
                generator.EmitCall(OpCodes.Call, m, null)
                if m.ReturnType.IsValueType then generator.Emit(OpCodes.Box, m.ReturnType)

                // store
                generator.Emit(OpCodes.Stelem_Ref)

            methods |> Seq.iteri computeProperty

            generator.Emit(OpCodes.Ldloc, arr)
            generator.Emit(OpCodes.Ret)

            let dele = meth.CreateDelegate(typeof<Func<obj,obj[]>>) :?> Func<obj,obj[]>

            dele.Invoke

        let wantNonPublic bindingFlags =
            let bindingFlags = defaultArg bindingFlags BindingFlags.Public
            bindingFlags &&& BindingFlags.NonPublic = BindingFlags.NonPublic

        let preComputeFieldsReader bindingFlags (declaringType : Type) (fields : PropertyInfo []) : obj -> _ =
            let getters = fields |> Array.map (fun p -> p.GetGetMethod(wantNonPublic bindingFlags))
            preComputeGetters declaringType getters

        let preComputeGetter (m : MethodInfo) =
            let reader = preComputeGetters m.DeclaringType [|m|]
            fun (o : obj) -> (reader o).[0]

        let preComputeRecordReader (recordType:Type, bindingFlags:BindingFlags option) =
            let fields = FSharpType.GetRecordFields(recordType, ?bindingFlags=bindingFlags)
            preComputeFieldsReader bindingFlags recordType fields

        let preComputeTupleReader (tuple : Type) =
            let rec gather (tuple : Type) =
                let fields = tuple.GetProperties()  |> Seq.filter (fun p -> p.Name.StartsWith("Item") || p.Name = "Rest") 
                                                    |> Seq.sortBy (fun p -> p.Name) //need: Items < 10 & "Item" < "Rest"
                                                    |> Seq.toArray
                let partial = preComputeFieldsReader None tuple fields
                match tuple.GetProperty("Rest") with
                | null -> partial
                | rest ->
                    let nested = gather rest.PropertyType
                    fun (o:obj) ->
                        let values = partial o
                        Array.append values.[..values.Length-2] (nested values.[values.Length-1])

            gather tuple : obj -> _

        let preComputeUnionReader(unionCase:UnionCaseInfo, bindingFlags:BindingFlags option) =
            let fields = unionCase.GetFields()
            let declaringType = if fields.Length = 0 then unionCase.DeclaringType else fields.[0].DeclaringType
            preComputeFieldsReader bindingFlags declaringType fields

        let preComputeExceptionReader(exnT : Type, bindingFlags:BindingFlags option) =
            let fields = FSharpType.GetExceptionFields(exnT, ?bindingFlags = bindingFlags)
            preComputeFieldsReader bindingFlags exnT fields

        let preComputeUnionTagReader(union : Type, bindingFlags) : obj -> int =
            if isOptionTy union then 
                (fun (obj:obj) -> match obj with null -> 0 | _ -> 1)
            else
                match union.GetProperty("Tag", allFlags) with
                | null ->
                    match union.GetMethod("GetTag", allFlags, null, [| union |], null) with
                    | null -> fun _ -> 0 // unary DU
                    | meth -> 
                        let d = preComputeGetter meth
                        fun (o : obj) -> d o :?> int
                | prop -> 
                    let d = preComputeGetter (prop.GetGetMethod(true))
                    fun (o : obj) -> d o :?> int

        // check if type is recursive
        let isRecursiveTy0 baseT immediateSubtypes =
            let rec traverseSubtype baseT (traversed : Set<_>) (t : Type) =
                if traversed.Contains t.AssemblyQualifiedName then traversed else
            
                let traversed = traversed.Add t.AssemblyQualifiedName 
                
                if t = baseT then traversed
                elif t.IsGenericType then
                    let ta = t.GetGenericArguments()
                    traverseSubtypes baseT traversed ta
                elif t.IsArray then
                    let et = t.GetElementType()
                    traverseSubtype baseT traversed et
                elif FSharpType.IsUnion t then
                    let ucis = FSharpType.GetUnionCases t
                    let ts = ucis |> Seq.collect (fun u -> u.GetFields() |> Seq.map (fun f -> f.PropertyType))
                    traverseSubtypes baseT traversed ts
                elif FSharpType.IsRecord t then
                    let ts = FSharpType.GetRecordFields t |> Seq.map (fun f -> f.PropertyType)
                    traverseSubtypes baseT traversed ts
//                elif FSharpType.IsExceptionRepresentation t then
//                    let types = FSharpType.GetExceptionFields t |> Seq.map (fun f -> f.PropertyType)
//                    traverseSubtypes traversed types
                else traversed

            and traverseSubtypes baseT traversed (ts : seq<Type>) = 
                Seq.fold (traverseSubtype baseT) traversed ts

            let traversed = traverseSubtypes baseT Set.empty immediateSubtypes
            traversed.Contains baseT.AssemblyQualifiedName

        let isRecursiveTy (t : Type) =
            let branches =
                if FSharpType.IsUnion t then
                    let ucis = FSharpType.GetUnionCases t
                    ucis |> Seq.collect (fun u -> u.GetFields() |> Seq.map (fun f -> f.PropertyType))
                elif FSharpType.IsRecord t then
                    FSharpType.GetRecordFields t |> Seq.map (fun f -> f.PropertyType)
                else Seq.empty

            isRecursiveTy0 t branches
        

    module Reflection =

        type FSharpValue =
            static member PreComputeRecordConstructor(recordType:Type,?bindingFlags:BindingFlags) =
                ReflectionImpl.preComputeRecordContructor(recordType,bindingFlags)
            static member PreComputeUnionConstructor(unionCase:UnionCaseInfo, ?bindingFlags:BindingFlags) =
                ReflectionImpl.preComputeUnionConstructor(unionCase,bindingFlags)
            static member PreComputeExceptionConstructor(exnT,?bindingFlags) =
                ReflectionImpl.preComputeExceptionConstructor(exnT, bindingFlags)
            static member PreComputeRecordReader(recordType:Type, ?bindingFlags:BindingFlags) : obj -> obj[] =
                ReflectionImpl.preComputeRecordReader(recordType,bindingFlags)
            static member PreComputeUnionReader(unionCase:UnionCaseInfo, ?bindingFlags:BindingFlags) : obj -> obj[] =
                ReflectionImpl.preComputeUnionReader(unionCase, bindingFlags)
            static member PreComputeExceptionReader(exnT:Type, ?bindingFlags:BindingFlags) : obj -> obj[] =
                ReflectionImpl.preComputeExceptionReader(exnT, bindingFlags)
            static member PreComputeConstructor(ctorInfo : ConstructorInfo) = 
                ReflectionImpl.preComputeConstructor ctorInfo
            static member PreComputePropertyGetters(t : Type, properties : PropertyInfo [], ?bindingFlags:BindingFlags) : obj -> obj [] =
                let isValid (p : PropertyInfo) = p.DeclaringType = t
                if not <| Array.forall isValid properties then invalidArg "invalid property getters" "getters"
                ReflectionImpl.preComputeFieldsReader bindingFlags t properties

        type FSharpType =
            static member IsRecursive(t : Type) = ReflectionImpl.isRecursiveTy t


        // a few pretty wrappers for our F# types

        type FsUnion(union : Type, ?bindingFlags) =
            let ucis = FSharpType.GetUnionCases(union, ?bindingFlags = bindingFlags)
            let declaringType = if ucis.Length > 0 then ucis.[0].DeclaringType else union
        
            let tagMap =
                ucis    |> Seq.map(fun uci -> uci.Tag, (uci, ReflectionImpl.preComputeUnionConstructor(uci, bindingFlags),
                                                                    ReflectionImpl.preComputeUnionReader(uci, bindingFlags)))
                        |> Map.ofSeq

            let isRecursive = lazy(
                let ts = ucis |> Seq.collect (fun uci -> uci.GetFields() |> Seq.map (fun f -> f.PropertyType))
                ReflectionImpl.isRecursiveTy0 declaringType ts)

            let tagReader = ReflectionImpl.preComputeUnionTagReader (declaringType, bindingFlags)

            // index for globally memoizing unions
            static let memoIdx = Atom.atom Map.empty<string * BindingFlags option, FsUnion>
            static member Create(t : Type, ?bindingFlags : BindingFlags) =
                let k = t.AssemblyQualifiedName, bindingFlags
                match memoIdx.Value.TryFind k with
                | Some u -> u
                | None ->
                    let u = new FsUnion(t, ?bindingFlags = bindingFlags)
                    memoIdx.Swap(fun m -> m.Add(k, u))
                    u

            member __.GetTag (o : obj) = tagReader o
            member __.UCIs = ucis
            member __.GetUCI (o : obj) = let uci,_,_ = tagMap.[tagReader o] in uci
            member __.Decompose (o : obj) = let tag = tagReader o in let _,_,reader = tagMap.[tag] in tag, reader o
            member __.Compose (tag : int, parameters : obj []) = let _,cons,_ = tagMap.[tag] in cons parameters
            member __.DeclaringType = declaringType
            member __.IsRecursive = isRecursive.Value


        type FsRecord(record : Type, ?bindingFlags) =
            let fields = FSharpType.GetRecordFields(record, ?bindingFlags = bindingFlags)
            let constr = ReflectionImpl.preComputeRecordContructor(record, bindingFlags)
            let reader = ReflectionImpl.preComputeRecordReader(record, bindingFlags)

            let isRecursive = lazy (
                    let ts = fields |> Seq.map (fun f -> f.PropertyType)
                    ReflectionImpl.isRecursiveTy0 record ts)

            // index for globally memoizing records
            static let memoIdx = Atom.atom Map.empty<string * BindingFlags option, FsRecord>
            static member Create(t : Type, ?bindingFlags : BindingFlags) =
                let k = t.AssemblyQualifiedName, bindingFlags
                match memoIdx.Value.TryFind k with
                | Some r -> r
                | None ->
                    let r = new FsRecord(t, ?bindingFlags = bindingFlags)
                    memoIdx.Swap(fun m -> m.Add(k, r))
                    r

            member __.DeclaringType = record
            member __.Fields = fields
            member __.Decompose (o : obj) = reader o
            member __.Compose (fields : obj []) = constr fields
            member __.IsRecursive = isRecursive.Value

        type FsTuple(tuple : Type) =
            let elems = FSharpType.GetTupleElements tuple
            let constr = ReflectionImpl.preComputeTupleConstructor tuple
            let reader = ReflectionImpl.preComputeTupleReader tuple

            // index for globally memoizing tuples
            static let memoIdx = Atom.atom Map.empty<string, FsTuple>
            static member Create(t : Type) =
                let k = t.AssemblyQualifiedName
                match memoIdx.Value.TryFind k with
                | Some t -> t
                | None ->
                    let t = new FsTuple(t)
                    memoIdx.Swap(fun m -> m.Add(k, t))
                    t

            member __.Elements = elems
            member __.Decompose (o : obj) = reader o
            member __.Compose (fields : obj []) = constr fields
            member __.DeclaringType = tuple

        type FsException(exnT : Type, ?bindingFlags) =
            let fields = FSharpType.GetExceptionFields(exnT, ?bindingFlags = bindingFlags)
            let constr = ReflectionImpl.preComputeExceptionConstructor(exnT, bindingFlags)
            let reader = ReflectionImpl.preComputeExceptionReader(exnT, bindingFlags)

            // index for globally memoizing exceptions
            static let memoIdx = Atom.atom Map.empty<string * BindingFlags option, FsException>
            static member Create(t : Type, ?bindingFlags : BindingFlags) =
                let k = t.AssemblyQualifiedName, bindingFlags
                match memoIdx.Value.TryFind k with
                | Some u -> u
                | None ->
                    let e = new FsException(t, ?bindingFlags = bindingFlags)
                    memoIdx.Swap(fun m -> m.Add(k, e))
                    e

            member __.DeclaringType = exnT
            member __.Fields = fields
            member __.Decompose (o : obj) = reader o
            member __.Compose (fields : obj []) = constr fields
