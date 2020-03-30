[<AutoOpen>]
module MBrace.FsPickler.Tests.Utils

open System
open System.IO
open System.Threading.Tasks
open System.Reflection
open System.Runtime.Serialization

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open NUnit.Framework

let runsOnMono = System.Type.GetType("Mono.Runtime") <> null

let allFlags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static

let failAssert fmt = Printf.ksprintf(fun msg -> raise <| new AssertionException(msg)) fmt

let shouldFailwith<'Exception when 'Exception :> exn>(f : unit -> unit) =
    let result =
        try f () ; Choice1Of3 ()
        with
        | :? 'Exception -> Choice2Of3 ()
        | e -> Choice3Of3 e

    match result with
    | Choice1Of3 () ->
        let msg = sprintf "Expected exception '%s', but was successful." typeof<'Exception>.Name
        raise <| new AssertionException(msg)
    | Choice2Of3 () -> ()
    | Choice3Of3 e ->
        let msg = sprintf "An unexpected exception type was thrown\nExpected: '%s'\n but was: '%O'." typeof<'Exception>.Name e
        raise <| new AssertionException(msg)

let (|InnerExn|_|) (e : exn) =
    match e.InnerException with
    | null -> None
    | ie -> Some ie

let rec getMemberCall (expr : Expr) =
    match expr with
    | Call(_,m,_) -> m :> MemberInfo
    | PropertyGet(_,p,_) -> p :> MemberInfo
    | PropertySet(_,p,_,_) -> p :> MemberInfo
    | FieldGet(_,f) -> f :> MemberInfo
    | FieldSet(_,f,_) -> f :> MemberInfo
    | Lambda(_,body) -> getMemberCall body
    | Let(_,_,c) -> getMemberCall c
    | Application(expr, _) -> getMemberCall expr
    | _ -> invalidArg "expr" "not a member call"

type Async with
    static member AwaitTask(t : Task) = Async.AwaitTask(t.ContinueWith ignore)

type AsyncBuilder with
    member ab.Bind(t : Task<'T>, cont : 'T -> Async<'S>) = ab.Bind(Async.AwaitTask t, cont)
    member ab.Bind(t : Task, cont : unit -> Async<'S>) = ab.Bind(t.ContinueWith ignore, cont)

type Stream with
    static member AsyncCopy(source : Stream, target : Stream) =
        Async.AwaitTask(source.CopyToAsync(target))

type MemoryStream with
    member m.Clone() = new MemoryStream(m.ToArray())

#if !NETCOREAPP
type AppDomain with

    static member Create(?name : string) =
        let name = match name with Some n -> n | None -> Guid.NewGuid().ToString("N")
        let currentDomain = AppDomain.CurrentDomain
        let appDomainSetup = currentDomain.SetupInformation
        let evidence = new Security.Policy.Evidence(currentDomain.Evidence)
        AppDomain.CreateDomain(name, evidence, appDomainSetup)

    member domain.Activate<'T when 'T :> MarshalByRefObject and 'T : (new : unit -> 'T)> () : 'T =
        let assemblyName = typeof<'T>.Assembly.FullName
        let typeName = typeof<'T>.FullName
        let culture = System.Globalization.CultureInfo.CurrentCulture
        let handle = domain.CreateInstance(assemblyName, typeName, false, allFlags, null, [||], culture, [||])
        handle.Unwrap() :?> 'T
#endif

[<AutoOpen>]
module private FailoverPicklerImpl =

    type IPickler =
        abstract Id : string
        abstract Pickle<'T> : 'T -> byte[]
        abstract UnPickle<'T> : byte[] -> 'T

    let inline pickle (serializer : Stream -> unit) =
        use m = new MemoryStream()
        serializer m
        m.ToArray()

    let inline unpickle (deserializer : Stream -> 'T) (data : byte[]) =
        use m = new MemoryStream(data)
        deserializer m

    let mkBinaryFormatterPickler() =
        let serializer = new Formatters.Binary.BinaryFormatter()
        { new IPickler with
            member __.Id = "BinaryFormatter"
            member __.Pickle t = pickle (fun s -> serializer.Serialize(s,t))
            member __.UnPickle p = unpickle (fun s -> serializer.Deserialize(s) :?> 'T) p }

#if !NETCOREAPP
    let mkNetDataContractPickler() =
        let serializer = new NetDataContractSerializer()
        { new IPickler with
            member __.Id = "NetDataContract"
            member __.Pickle t = pickle (fun s -> serializer.Serialize(s,t))
            member __.UnPickle p = unpickle (fun s -> serializer.Deserialize(s) :?> 'T) p }
#endif

    let mkNewtonsoftPickler() =
        let serializer = new Newtonsoft.Json.JsonSerializer()
        { new IPickler with
            member __.Id = "Newtonsoft.Json"
            member __.Pickle t =
                pickle (fun s ->
                    use writer = new System.IO.StreamWriter(s)
                    serializer.Serialize(writer, obj)
                    writer.Flush())

            member __.UnPickle p =
                unpickle (fun s ->
                    use reader = new System.IO.StreamReader(s)
                    serializer.Deserialize(reader, typeof<'T>) :?> 'T) p }


    let tryPickleWith (pickler:IPickler) (value : 'T) =
        try 
            let pickle = pickler.Pickle value
            let roundTripValue = pickler.UnPickle pickle

            let isCorrectRoundtrip =
                match box roundTripValue with
                | null -> obj.ReferenceEquals(value, null)
                | :? MemberInfo as m -> m = (box value :?> MemberInfo)
                | v -> v.ToString() = value.ToString()
                                
            if isCorrectRoundtrip then Some (pickler.Id, pickle)
            else None
        with _ -> None
     
     
type FailoverPickle<'T> = FailoverPickle of picklerId:string * data:byte[]

/// A serializer that wraps around a collection of readily available .NET serializers
/// Used as a baseline for serialization tests
type FailoverPickler private () =
    static let picklers = 
#if !NETCOREAPP
        if runsOnMono then
            [mkBinaryFormatterPickler() ; mkNewtonsoftPickler()]
        else
            [mkBinaryFormatterPickler() ; mkNewtonsoftPickler() ; mkNetDataContractPickler()]
#else
            [mkBinaryFormatterPickler() ; mkNewtonsoftPickler()]
#endif

    static member TryPickle<'T> (value : 'T) : FailoverPickle<'T> option =
        picklers 
        |> List.tryPick (fun p -> tryPickleWith p value)
        |> Option.map FailoverPickle

    static member UnPickle<'T> (FailoverPickle(picklerId, pickle) : FailoverPickle<'T>) =
        let pickler = picklers |> List.find (fun p -> p.Id = picklerId)
        pickler.UnPickle<'T> pickle

    static member IsPickleable (value : 'T) = FailoverPickler.TryPickle value |> Option.isSome