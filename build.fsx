// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "packages/build/FAKE/tools"
#r "packages/build/FAKE/tools/FakeLib.dll"

open System

open Fake 
open Fake.Git
open Fake.ReleaseNotesHelper

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "FsPickler.sln"

let gitOwner = "mbraceproject"
let gitHome = "https://github.com/" + gitOwner
let gitName = "FsPickler"
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/" + gitOwner

let testProjects = "tests/*.Tests/*.??proj"

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

let build configuration () =
    DotNetCli.Build(fun c ->
        { c with
            Project = project
            Configuration = configuration
            AdditionalArgs = 
                [ 
                    "-p:Version=" + release.NugetVersion
                    "-p:GenerateAssemblyInfo=true"
                    "-p:SourceLinkCreate=true" 
                ]
        })

Target "Build.Release" (build "Release")
Target "Build.Release-NoEmit" (build "Release-NoEmit")

// --------------------------------------------------------------------------------------
// Run the tests 

let runTests config (proj : string) =
    DotNetCli.Test (fun c ->
        { c with
            Project = proj
            Configuration = config
            AdditionalArgs = 
                [
                    yield "--no-build"
                    yield "--"
                    if EnvironmentHelper.isMono then yield "RunConfiguration.DisableAppDomain=true"
                ]
        })

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
        DotNetCli.RunCommand id (sprintf "sourcelink test %s" nupkg)
    )
)

Target "NuGet.Push" (fun _ -> Paket.Push (fun p -> { p with WorkingDir = artifactsDir }))


// Doc generation

Target "GenerateDocs" (fun _ ->
    let path = __SOURCE_DIRECTORY__ @@ "packages/build/FSharp.Compiler.Tools/tools/fsi.exe"
    let workingDir = "docs/tools"
    let args = "--define:RELEASE generate.fsx"
    let command, args = 
        if EnvironmentHelper.isMono then "mono", sprintf "'%s' %s" path args 
        else path, args

    if Shell.Exec(command, args, workingDir) <> 0 then
        failwith "failed to generate docs"
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
#nowarn "85"
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
  ==> "Prepare"
  ==> "Build.Release"
  ==> "Build.Release-NoEmit"
  ==> "Build"
  ==> "RunTests.Release"
  ==> "RunTests.Release-NoEmit"
  ==> "RunTests"
  ==> "Default"

"Default"
  ==> "PrepareRelease"
  ==> "NuGet.Pack"
  //==> "SourceLink.Test"
  ==> "GenerateDocs"
  ==> "Bundle"

"Bundle"
  ==> "ReleaseDocs"
  ==> "NuGet.Push"
  ==> "ReleaseGithub"
  ==> "Release"

RunTargetOrDefault "Default"