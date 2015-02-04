namespace Nessos.FsPickler

open System
open System.Reflection

open Nessos.FsPickler.Reflection

#if EMIT_IL
open System.Reflection.Emit
open Nessos.FsPickler.Emit
#endif

type internal ShallowObjectCopier private () =  
    static let mkCopier (t : Type) =
        if t.IsValueType then invalidOp t.FullName "not a class."

        let fields = gatherSerializedFields t
#if EMIT_IL
        let dele =
            DynamicMethod.compileAction2<obj, obj> "shallowCopier" (fun source target ilGen ->
                for f in fields do
                    target.Load()
                    source.Load()
                    ilGen.Emit(OpCodes.Ldfld, f)
                    ilGen.Emit(OpCodes.Stfld, f)

                ilGen.Emit OpCodes.Ret
            )

        fun (src : obj) (tgt : obj) -> dele.Invoke(src,tgt)
#else
        fun src dst ->
            for f in fields do
                let v = f.GetValue(src)
                f.SetValue(dst, v)
#endif
    static let mkCopierMemoized = memoize mkCopier

    static member Copy (t : Type) (source : obj) (target : obj) = 
        mkCopierMemoized t source target