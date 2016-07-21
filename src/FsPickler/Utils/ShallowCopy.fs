namespace MBrace.FsPickler

open System
open System.Reflection

open MBrace.FsPickler.Reflection

#if EMIT_IL
open System.Reflection.Emit
open MBrace.FsPickler.Emit
#endif

type internal ShallowObjectCopier<'T> private () =
    static let copier : Action<'T, 'T> =
        let t = typeof<'T>
        if t.IsValueType then invalidArg t.FullName "not a class."
        let fields = gatherSerializedFields t

#if EMIT_IL
        DynamicMethod.compileAction2<'T, 'T> "shallowCopier" (fun source target ilGen ->
            for f in fields do
                target.Load()
                source.Load()
                ilGen.Emit(OpCodes.Ldfld, f)
                ilGen.Emit(OpCodes.Stfld, f)

            ilGen.Emit OpCodes.Ret)
#else
        new Action<'T,'T>(fun src dst ->
            for f in fields do
                let v = f.GetValue(src)
                f.SetValue(dst, v))
#endif

    static member Copy (source : 'T) (target : 'T) = copier.Invoke(source, target)