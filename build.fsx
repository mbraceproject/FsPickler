// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "packages/build/FAKE/tools"
#r "packages/build/FAKE/tools/FakeLib.dll"

open System
open System.IO

open Fake 
open Fake.Git
open Fake.ReleaseNotesHelper
open Fake.AssemblyInfoFile
open Fake.Testing.NUnit3

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "FsPickler"

let summary = "A fast serialization framework and pickler combinator library for .NET"

let gitOwner = "mbraceproject"
let gitHome = "https://github.com/" + gitOwner
let gitName = "FsPickler"
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/" + gitOwner

let testProjects = "tests/**/*.??proj"

// Folder to deposit deploy artifacts
let artifactsDir = __SOURCE_DIRECTORY__ @@ "artifacts"

// --------------------------------------------------------------------------------------
// The rest of the code is standard F# build script 
// --------------------------------------------------------------------------------------

//// Read release notes & version info from RELEASE_NOTES.md
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")

Target "BuildVersion" (fun _ ->
    Fake.AppVeyor.UpdateBuildVersion release.NugetVersion
)

Target "AssemblyInfo" (fun _ ->
    let getAssemblyInfoAttributes projectName =
        [ Attribute.Title projectName
          Attribute.Product project
          Attribute.Description summary
          Attribute.Copyright "\169 Eirik Tsarpalis."
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion ]

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension projectPath
        ( projectPath,
          projectName,
          System.IO.Path.GetDirectoryName projectPath,
          getAssemblyInfoAttributes projectName
        )

    !! "src/**/*.??proj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, projectName, folderName, attributes) ->
        match projFileName with
        | Fsproj -> CreateFSharpAssemblyInfo (folderName </> "AssemblyInfo.fs") attributes
        | Csproj -> CreateCSharpAssemblyInfo ((folderName </> "Properties") </> "AssemblyInfo.cs") attributes
        | Vbproj -> CreateVisualBasicAssemblyInfo ((folderName </> "My Project") </> "AssemblyInfo.vb") attributes
        | Shproj -> ()
        )
)

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "Clean" (fun _ ->
    CleanDirs <| !! "./**/bin/Release*"
    CleanDir "./tools/output"
    CleanDir "./temp"
    CleanDir "./artifacts"
)


// --------------------------------------------------------------------------------------
// Build library & test project

Target "DotNet.Restore" (fun _ -> DotNetCli.Restore id)

let build configuration () =
    // Build the rest of the project
    { BaseDirectory = __SOURCE_DIRECTORY__
      Includes = [ project + ".sln" ]
      Excludes = [] } 
    |> MSBuild "" "Build" ["Configuration", configuration; "SourceLinkCreate", "true"]
    |> Log ""

Target "Build.Release" (build "Release")
Target "Build.Release-NoEmit" (build "Release-NoEmit")

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner & kill test runner when complete

let runTests config (proj : string) =
    if EnvironmentHelper.isWindows then
        DotNetCli.Test (fun c ->
            { c with
                Project = proj
                Configuration = config })
    else
        // work around xunit/mono issue
        let projDir = Path.GetDirectoryName proj
        let projName = Path.GetFileNameWithoutExtension proj
        let netcoreFrameworks, legacyFrameworks = 
            !! (projDir @@ "bin" @@ config @@ "*/")
            |> Seq.map Path.GetFileName
            |> Seq.toArray
            |> Array.partition 
                (fun f -> 
                    f.StartsWith "netcore" || 
                    f.StartsWith "netstandard")
            

        for framework in netcoreFrameworks do
            DotNetCli.Test (fun c ->
                { c with
                    Project = proj
                    Framework = framework
                    Configuration = config })

        for framework in legacyFrameworks do
            let assembly = projDir @@ "bin" @@ config @@ framework @@ projName + ".dll"
            !! assembly
            |> NUnit3 (fun c ->
                { c with
                    OutputDir = sprintf "TestResult.%s.xml" config
                    TimeOut = TimeSpan.FromMinutes 20. })

Target "RunTests" DoNothing

Target "RunTests.Release" (fun _ ->
    for proj in !! testProjects do
        runTests "Release" proj
)

Target "RunTests.Release-NoEmit" (fun _ ->
    for proj in !! testProjects do
        runTests "Release-NoEmit" proj
)


// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet.Pack" (fun _ ->    
    Paket.Pack (fun p -> 
        { p with
            OutputPath = artifactsDir
            Version = release.NugetVersion
            ReleaseNotes = toLines release.Notes })
)

Target "SourceLink.Test" (fun _ ->
    !! (sprintf "%s/*.nupkg" artifactsDir)
    |> Seq.iter (fun nupkg ->
        DotNetCli.RunCommand
            (fun p -> { p with WorkingDir = __SOURCE_DIRECTORY__ @@ "tests" @@ "FsPickler.Core.Tests" } )
            (sprintf "sourcelink test %s" nupkg)
    )
)

Target "Nuget.Push" (fun _ -> Paket.Push (fun p -> { p with WorkingDir = artifactsDir }))


// Doc generation

Target "GenerateDocs" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"] [] |> ignore
)

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    let outputDir = "docs/output"

    CleanDir tempDocsDir
    Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir
    
    fullclean tempDocsDir
    ensureDirectory outputDir
    CopyRecursive outputDir tempDocsDir true |> tracefn "%A"
    StageAll tempDocsDir
    Commit tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir
)

// Github Releases

#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
open Octokit

Target "ReleaseGitHub" (fun _ ->
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun s -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun s -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some s -> s.Split().[0]

    //StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.pushBranch "" remote (Information.getBranchName "")

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion

    let client =
        match Environment.GetEnvironmentVariable "OctokitToken" with
        | null -> 
            let user =
                match getBuildParam "github-user" with
                | s when not (String.IsNullOrWhiteSpace s) -> s
                | _ -> getUserInput "Username: "
            let pw =
                match getBuildParam "github-pw" with
                | s when not (String.IsNullOrWhiteSpace s) -> s
                | _ -> getUserPassword "Password: "

            createClient user pw
        | token -> createClientWithToken token

    // release on github
    client
    |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    |> releaseDraft
    |> Async.RunSynchronously
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "Root" DoNothing
Target "Prepare" DoNothing
Target "PrepareRelease" DoNothing
Target "Build" DoNothing
Target "Default" DoNothing
Target "Bundle" DoNothing
Target "Release" DoNothing


"Root"
  =?> ("BuildVersion", buildServer = BuildServer.AppVeyor)
  ==> "Clean"
  ==> "AssemblyInfo"
  ==> "Prepare"
  ==> "DotNet.Restore"
  ==> "Build.Release"
  ==> "Build.Release-NoEmit"
  ==> "Build"
  ==> "RunTests.Release"
  ==> "RunTests.Release-NoEmit"
  ==> "RunTests"
  ==> "Default"

"Default"
  ==> "PrepareRelease"
  ==> "GenerateDocs"
  ==> "NuGet.Pack"
  ==> "SourceLink.Test"
  ==> "Bundle"

"Bundle"
  ==> "ReleaseDocs"
  ==> "Nuget.Push"
  ==> "ReleaseGithub"
  ==> "Release"

RunTargetOrDefault "Default"