namespace Nessos.FsPickler

    open System
    open System.IO
    open System.Reflection
    open System.Threading
    open System.Runtime.Serialization

    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection

#if EMIT_IL
    open System.Reflection.Emit
    open Nessos.FsPickler.Emit
    open Nessos.FsPickler.PicklerEmit
#endif

    type internal DataContractPickler =

        static member Create<'T>(resolver : IPicklerResolver) =

            let t = typeof<'T>
            let cacheByRef = not <| t.IsValueType
            
            let tryGetDataMemberInfo (m : MemberInfo) =
                match m with
                | :? PropertyInfo as p ->
                    match tryGetAttr<DataMemberAttribute> p with
                    | None -> None
                    | Some _ when not p.CanRead ->
                        let msg = sprintf "property '%s' marked as data member but missing getter." p.Name
                        raise <| new PicklerGenerationException(t, msg)

                    | Some _ when not p.CanWrite ->
                        let msg = sprintf "property '%s' marked as data member but missing setter." p.Name
                        raise <| new PicklerGenerationException(t, msg)

                    | Some attr -> Some(attr, p)

                | _ -> None

            let dataContractInfo = 
                gatherMembers t 
                |> Seq.choose tryGetDataMemberInfo
                |> Seq.mapi (fun i v -> (i,v))
                // sort data members: primarily specified by user-specified order
                // and secondarily by definition order
                |> Seq.sortBy (fun (i,(attr,_)) -> (attr.Order, i))
                |> Seq.map snd
                |> Seq.toArray

            let properties = dataContractInfo |> Array.map snd
            let picklers = properties |> Array.map (fun p -> resolver.Resolve p.PropertyType)
            let names = dataContractInfo |> Array.map (fun (attr,p) -> match attr.Name with null -> p.NormalizedName | name -> getNormalizedName name)

#if EMIT_IL
            let writer =
                if properties.Length = 0 then (fun _ _ _ -> ()) 
                else
                    let dele =
                        DynamicMethod.compileAction3<Pickler [], WriteState, 'T> "dataContractSerializer" (fun picklers writer parent ilGen ->

                            emitSerializeProperties properties names writer picklers parent ilGen

                            ilGen.Emit OpCodes.Ret)

                    fun w t v -> dele.Invoke(picklers, w, v)

            let readerDele =
                DynamicMethod.compileFunc2<Pickler [], ReadState, 'T> "dataContractDeserializer" (fun picklers reader ilGen ->
                    // initialize empty value type
                    let value = EnvItem<'T>(ilGen)
                    emitObjectInitializer typeof<'T> ilGen
                    value.Store ()

                    emitDeserializeProperties properties names reader picklers value ilGen

                    value.Load ()
                    ilGen.Emit OpCodes.Ret
                )

            let reader r t = readerDele.Invoke(picklers, r)
                
#else
            let writer (w : WriteState) (tag : string) (value : 'T) =
                for i = 0 to properties.Length - 1 do
                    let v = properties.[i].GetValue value
                    picklers.[i].UntypedWrite w names.[i] v

            let reader (r : ReadState) (tag : string) =
                let value = FormatterServices.GetUninitializedObject(t) |> fastUnbox<'T>
                for i = 0 to properties.Length - 1 do
                    let v = picklers.[i].UntypedRead r names.[i]
                    properties.[i].SetValue(value, v)

                value
#endif

            CompositePickler.Create(reader, writer, PicklerInfo.DataContract, cacheByRef = cacheByRef, useWithSubtypes = false)