namespace MBrace.FsPickler.Tests

open System
open System.IO
open System.Threading.Tasks
open System.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open NUnit.Framework

[<AutoOpen>]
module Utils =

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

#if !NETCOREAPP2_0
    type AppDomainManager private () =
        static let container = new System.Collections.Concurrent.ConcurrentDictionary<string, AppDomain>()

        static let initDomain (name : string) =
            let currentDomain = AppDomain.CurrentDomain
            let appDomainSetup = currentDomain.SetupInformation
            let evidence = new Security.Policy.Evidence(currentDomain.Evidence)
            AppDomain.CreateDomain(name, evidence, appDomainSetup)

        static member GetAppDomain(name : string) =
            let ok, value = container.TryGetValue name
            if ok then value
            else
                container.GetOrAdd(name, initDomain)

        static member Activate<'T when 'T :> MarshalByRefObject>(domain : string, ctorParams : obj []) =
            let domain = AppDomainManager.GetAppDomain(domain)
            let assemblyName = typeof<'T>.Assembly.FullName
            let typeName = typeof<'T>.FullName
            let culture = System.Globalization.CultureInfo.CurrentCulture
            let handle = domain.CreateInstance(assemblyName, typeName, false, allFlags, null, ctorParams, culture, [||])
            handle.Unwrap() :?> 'T
#endif