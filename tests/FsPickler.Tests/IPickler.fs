namespace Nessos.FsPickler.Tests

open System
open System.IO
open System.Reflection
open System.Runtime.Serialization
open System.Runtime.Serialization.Formatters.Binary

open Newtonsoft.Json

open Nessos.FsPickler

type IPickler =
    abstract Name : string
    abstract Pickle : 'T -> byte []
    abstract UnPickle<'T> : byte [] -> 'T

[<AutoOpen>]
module PickleUtils =
    let inline pickle (serializer : Stream -> unit) =
        use m = new MemoryStream()
        serializer m
        m.ToArray()

    let inline unpickle (deserializer : Stream -> 'T) (data : byte []) =
        use m = new MemoryStream(data)
        deserializer m

type BinaryFormatterPickler () =
    let bfs = new BinaryFormatter()

    interface IPickler with
        member __.Name = "BinaryFormatter"
        member __.Pickle (x : 'T) = pickle (fun s -> bfs.Serialize(s, x))
        member __.UnPickle<'T> data = unpickle (fun s -> bfs.Deserialize s :?> 'T) data

type NetDataContractPickler () =
    let ndc = new System.Runtime.Serialization.NetDataContractSerializer()

    interface IPickler with
        member __.Name = "NetDataContractSerializer"
        member __.Pickle (x : 'T) = pickle (fun s -> ndc.Serialize(s, x))
        member __.UnPickle data = unpickle (fun s -> ndc.Deserialize s :?> 'T) data

type JsonDotNetPickler () =
    let jdn = Newtonsoft.Json.JsonSerializer.Create()
        
    interface IPickler with
        member __.Name = "Json.Net"
        member __.Pickle (t : 'T) =
            pickle (fun s ->
                use writer = new System.IO.StreamWriter(s)
                jdn.Serialize(writer, t)
                writer.Flush())

        member __.UnPickle data : 'T =
            unpickle (fun s ->
                use reader = new System.IO.StreamReader(s)
                jdn.Deserialize(reader, typeof<'T>) :?> 'T) data


type FailoverPicklerException(message : string) =
    inherit System.Exception(message)

/// Provides a reliable serialization format
/// Pass object throught a chain of candidate serializers, choosing the first that succeeds.
type FailoverPickler (picklers : IPickler list) =
        
    interface IPickler with
        member __.Name = "Failover Pickler"
        member __.Pickle (x : 'T) =
            let rec tryNext id (rest : IPickler list) =
                match rest with
                | [] -> raise <| FailoverPicklerException(sprintf "failed to pickle '%O." x)
                | hd :: tl ->
                    let result =
                        try 
                            let p = hd.Pickle x

                            let isCorrectDeserialization =
                                let x1 = hd.UnPickle<'T> p

                                match box x with
                                | null -> obj.ReferenceEquals(x1, null)
                                | :? MemberInfo as m -> m = (box x1 :?> MemberInfo)
                                | _ -> x.ToString() = x1.ToString()
                                
                            if isCorrectDeserialization then Some p
                            else None

                        with _ -> None

                    match result with
                    | Some p -> Array.append [|byte id|] p
                    | None -> tryNext (id + 1) tl

            tryNext 0 picklers

        member __.UnPickle<'T> (data : byte []) =
            let id = int <| data.[0]
            picklers.[id].UnPickle<'T> (data.[1..])


    static member Create() =
        let bfp = new BinaryFormatterPickler() :> IPickler
        let ndc = new NetDataContractPickler() :> IPickler
        let jdn = new JsonDotNetPickler() :> IPickler

        if runsOnMono then
            new FailoverPickler([bfp ; jdn ]) :> IPickler
        else
            new FailoverPickler([bfp ; jdn ; ndc ]) :> IPickler