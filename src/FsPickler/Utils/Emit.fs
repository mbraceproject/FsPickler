namespace MBrace.FsPickler

#if EMIT_IL

module internal Emit =

    open System
    open System.Reflection
    open System.Reflection.Emit
    open System.Runtime.Serialization
    open System.Collections.Concurrent

    /// a descriptor for local variables or parameters in emitted IL

    type EnvItem<'T>(ilGen : ILGenerator, ?argument : int16) = 
        inherit EnvItem(typeof<'T>, ilGen, ?argument = argument)

    and EnvItem(ty : Type, ilGen : ILGenerator, ?argument : int16) =

        let env = 
            match argument with
            | Some argId -> Arg argId
            | None -> LocalVar <| ilGen.DeclareLocal ty

        member __.Type = ty

        member e.Load () =
            match env with
            | Arg 0s -> ilGen.Emit OpCodes.Ldarg_0
            | Arg 1s -> ilGen.Emit OpCodes.Ldarg_1
            | Arg 2s -> ilGen.Emit OpCodes.Ldarg_2
            | Arg 3s -> ilGen.Emit OpCodes.Ldarg_3
            | Arg i -> ilGen.Emit (OpCodes.Ldarg_S, i)
            | LocalVar v -> ilGen.Emit(OpCodes.Ldloc, v)

        member e.LoadAddress () =
            match env with
            | Arg i -> ilGen.Emit (OpCodes.Ldarga_S, i)
            | LocalVar v -> ilGen.Emit(OpCodes.Ldloca, v)

        member e.Store () =
            match env with
            | LocalVar v -> ilGen.Emit(OpCodes.Stloc, v)
            | _ -> invalidOp "cannot store to arg param."

        member e.LocalBuilder =
            match env with
            | LocalVar v -> v
            | _ -> invalidArg "EnvItem" "is not a local variable."

    and EnvDescriptor =
        | Arg of int16
        | LocalVar of LocalBuilder


    // wrappers for defining dynamic methods

    module DynamicMethod =

        type private Marker = class end

        let private voidType = Type.GetType("System.Void")

        let private compileCache = new ConcurrentDictionary<Type * string, Lazy<Delegate>>()

        /// Emits a dynamic method that is cached based on the type of the delegate and method name
        let emitDynamicMethod<'Delegate when 'Delegate :> Delegate> 
                    (name : string) (argTypes : Type []) (returnType : Type)
                    (ilBuilder : ILGenerator -> unit) : 'Delegate =

            let compile () =
                let dynamicMethod =
                    new DynamicMethod(name, MethodAttributes.Static ||| MethodAttributes.Public, 
                                        CallingConventions.Standard, returnType, argTypes, typeof<Marker>, 
                                        skipVisibility = true)

                let ilGen = dynamicMethod.GetILGenerator()
                do ilBuilder ilGen
                dynamicMethod.CreateDelegate typeof<'Delegate>

            let wrapper = compileCache.GetOrAdd((typeof<'Delegate>, name), fun _ -> lazy(compile ()))
            wrapper.Value :?> 'Delegate

        let compileFunc<'T> (name : string) (builderF : ILGenerator -> unit) =
            emitDynamicMethod<Func<'T>> name [||] typeof<'T> builderF

        let compileFunc1<'U1,'V> (name : string) (builderF : EnvItem<'U1> -> ILGenerator -> unit) =
            emitDynamicMethod<Func<'U1,'V>> name [| typeof<'U1> |] typeof<'V>
                (fun ilGen ->
                    let arg0 = EnvItem<'U1>(ilGen, 0s)
                    builderF arg0 ilGen)

        let compileFunc2<'U1,'U2,'V> (name : string) (builderF : EnvItem<'U1> -> EnvItem<'U2> -> ILGenerator -> unit) =
            emitDynamicMethod<Func<'U1,'U2,'V>> name [| typeof<'U1> ; typeof<'U2> |] typeof<'V>
                (fun ilGen ->
                    let arg0 = EnvItem<'U1>(ilGen, 0s)
                    let arg1 = EnvItem<'U2>(ilGen, 1s)
                    do builderF arg0 arg1 ilGen)

        let compileFunc3<'U1,'U2,'U3,'V> (name : string) (builderF : EnvItem<'U1> -> EnvItem<'U2> -> EnvItem<'U3> -> ILGenerator -> unit) =
            emitDynamicMethod<Func<'U1,'U2,'U3,'V>> name [| typeof<'U1> ; typeof<'U2> ; typeof<'U3> |] typeof<'V>
                (fun ilGen ->
                    let arg0 = EnvItem<'U1>(ilGen, 0s)
                    let arg1 = EnvItem<'U2>(ilGen, 1s)
                    let arg2 = EnvItem<'U3>(ilGen, 2s)
                    do builderF arg0 arg1 arg2 ilGen)

        let compileFunc4<'U1,'U2,'U3,'U4,'V> (name : string) (builderF : EnvItem<'U1> -> EnvItem<'U2> -> EnvItem<'U3> -> EnvItem<'U4> -> ILGenerator -> unit) =
            emitDynamicMethod<Func<'U1,'U2,'U3,'U4,'V>> name [| typeof<'U1> ; typeof<'U2> ; typeof<'U3> ; typeof<'U4> |] typeof<'V>
                (fun ilGen ->
                    let arg0 = EnvItem<'U1>(ilGen, 0s)
                    let arg1 = EnvItem<'U2>(ilGen, 1s)
                    let arg2 = EnvItem<'U3>(ilGen, 2s)
                    let arg3 = EnvItem<'U4>(ilGen, 3s)
                    do builderF arg0 arg1 arg2 arg3 ilGen)

        let compileAction1<'U1> (name : string) (builderF : EnvItem<'U1> -> ILGenerator -> unit) =
            emitDynamicMethod<Action<'U1>> name [| typeof<'U1> |] voidType
                (fun ilGen ->
                    let arg0 = EnvItem<'U1>(ilGen, 0s)
                    do builderF arg0 ilGen)

        let compileAction2<'U1,'U2> (name : string) (builderF : EnvItem<'U1> -> EnvItem<'U2> -> ILGenerator -> unit) =
            emitDynamicMethod<Action<'U1,'U2>> name [| typeof<'U1> ; typeof<'U2> |] voidType
                (fun ilGen ->
                    let arg0 = EnvItem<'U1>(ilGen, 0s)
                    let arg1 = EnvItem<'U2>(ilGen, 1s)
                    do builderF arg0 arg1 ilGen)

        let compileAction3<'U1,'U2,'U3> (name : string) (builderF : EnvItem<'U1> -> EnvItem<'U2> -> EnvItem<'U3> -> ILGenerator -> unit) =
            emitDynamicMethod<Action<'U1,'U2,'U3>> name [| typeof<'U1> ; typeof<'U2> ; typeof<'U3> |] voidType
                (fun ilGen ->
                    let arg0 = EnvItem<'U1>(ilGen, 0s)
                    let arg1 = EnvItem<'U2>(ilGen, 1s)
                    let arg2 = EnvItem<'U3>(ilGen, 2s)
                    do builderF arg0 arg1 arg2 ilGen)

        let compileAction4<'U1,'U2,'U3,'U4> (name : string) (builderF : EnvItem<'U1> -> EnvItem<'U2> -> EnvItem<'U3> -> EnvItem<'U4> -> ILGenerator -> unit) =
            emitDynamicMethod<Action<'U1,'U2,'U3,'U4>> name [| typeof<'U1> ; typeof<'U2> ; typeof<'U3> ; typeof<'U4> |] voidType
                (fun ilGen ->
                    let arg0 = EnvItem<'U1>(ilGen, 0s)
                    let arg1 = EnvItem<'U2>(ilGen, 1s)
                    let arg2 = EnvItem<'U3>(ilGen, 2s)
                    let arg3 = EnvItem<'U4>(ilGen, 3s)
                    do builderF arg0 arg1 arg2 arg3 ilGen)
        
#endif