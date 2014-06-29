namespace Nessos.FsPickler.Tests

    open System
    open System.Reflection

    open NUnit.Framework

    [<AutoOpen>]
    module Utils =

        let runsOnMono = System.Type.GetType("Mono.Runtime") <> null

        let allFlags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static

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
                let msg = sprintf "An unexpected exception type was thrown\nExpected: '%s'\n but was: '%s'." (e.GetType().Name) typeof<'Exception>.Name
                raise <| new AssertionException(msg)


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