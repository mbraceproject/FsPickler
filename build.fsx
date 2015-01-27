// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------

#I "packages/FAKE/tools"
#r "packages/FAKE/tools/FakeLib.dll"
//#load "packages/SourceLink.Fake/tools/SourceLink.fsx"
open System
open System.IO

open Fake 
open Fake.Git
open Fake.ReleaseNotesHelper
open Fake.AssemblyInfoFile
//open SourceLink

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "FsPickler"
let authors = ["Eirik Tsarpalis"]
let summary = "A fast serialization framework and pickler combinator library for .NET"

let description = """
    FsPickler is a serialization library that facilitates the distribution of .NET objects. 
    The implementation focuses on performance and completeness in supported types, including F# types. 
    It supports multiple, pluggable serialization formats including Binary, Xml, JSON and BSON. 
    The library is based on the functional programming concept of pickler combinators which 
    has been adapted to accommodate the object oriented nature of the .NET framework.
"""

let tags = "F# fsharp serializer binary pickler"

let gitHome = "https://github.com/nessos"
let gitName = "FsPickler"
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/nessos"


let testAssemblies = ["bin/FsPickler.Tests.dll" ; "bin/NoEmit/FsPickler.Tests.dll"]

//
//// --------------------------------------------------------------------------------------
//// The rest of the code is standard F# build script 
//// --------------------------------------------------------------------------------------

//// Read release notes & version info from RELEASE_NOTES.md
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let release = parseReleaseNotes (IO.File.ReadAllLines "RELEASE_NOTES.md")
let nugetVersion = release.NugetVersion

Target "BuildVersion" (fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" nugetVersion) |> ignore
)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    let attrs =
        [ 
            Attribute.Version release.AssemblyVersion
            Attribute.FileVersion release.AssemblyVersion
        ] 

    CreateFSharpAssemblyInfo "src/FsPickler/AssemblyInfo.fs" attrs
    CreateFSharpAssemblyInfo "src/FsPickler.Json/AssemblyInfo.fs" attrs
    CreateCSharpAssemblyInfo "src/FsPickler.CSharp/Properties/AssemblyInfo.cs" attrs

)


// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target "RestorePackages" (fun _ ->
    !! "./**/packages.config"
    |> Seq.iter (RestorePackage (fun p -> { p with ToolPath = "./.nuget/NuGet.exe" }))
)

Target "Clean" (fun _ ->
    CleanDirs <| !! "./**/bin/"
    CleanDir "./tools/output"
    CleanDir "./temp"
)

//
//// --------------------------------------------------------------------------------------
//// Build library & test project

let configuration = environVarOrDefault "Configuration" "Release"

let build configuration () =
    // Build the rest of the project
    { BaseDirectory = __SOURCE_DIRECTORY__
      Includes = [ project + ".sln" ]
      Excludes = [] } 
    |> MSBuild "" "Build" ["Configuration", configuration]
    |> Log "AppBuild-Output: "

Target "Build.Default" (build configuration)
Target "Build.NoEmit" (build "NoEmit")
Target "Build.Net40" (build "Release-NET40")

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner & kill test runner when complete

Target "RunTests" (fun _ ->
    let nunitVersion = GetPackageVersion "packages" "NUnit.Runners"
    let nunitPath = sprintf "packages/NUnit.Runners.%s/tools" nunitVersion
    ActivateFinalTarget "CloseTestRunner"

    testAssemblies
    |> NUnit (fun p ->
        { p with
            Framework = "v4.0.30319"
            ToolPath = nunitPath
            DisableShadowCopy = true
            TimeOut = TimeSpan.FromMinutes 20.
            OutputFile = "TestResults.xml" })
)

FinalTarget "CloseTestRunner" (fun _ ->  
    ProcessHelper.killProcess "nunit-agent.exe"
)
//
//// --------------------------------------------------------------------------------------
//// Build a NuGet package

let addFile (target : string) (file : string) =
    if File.Exists (Path.Combine("nuget", file)) then (file, Some target, None)
    else raise <| new FileNotFoundException(file)

let addAssembly (target : string) assembly =
    let includeFile force file =
        let file = file
        if File.Exists (Path.Combine("nuget", file)) then [(file, Some target, None)]
        elif force then raise <| new FileNotFoundException(file)
        else []

    seq {
        yield! includeFile true assembly
        yield! includeFile false <| Path.ChangeExtension(assembly, "pdb")
        yield! includeFile false <| Path.ChangeExtension(assembly, "xml")
        yield! includeFile false <| assembly + ".config"
    }

Target "NuGet.FsPickler" (fun _ ->
    let nugetPath = ".nuget/NuGet.exe"
    NuGet (fun p -> 
        { p with   
            Authors = authors
            Project = "FsPickler"
            Summary = summary
            Description = description
            Version = nugetVersion
            ReleaseNotes = String.concat " " release.Notes
            Tags = tags
            OutputPath = "bin"
            ToolPath = nugetPath
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Dependencies = []
            Publish = hasBuildParam "nugetkey" 
            Files =
                [
                    yield! addAssembly @"lib\net45" @"..\bin\FsPickler.dll"
                    yield! addAssembly @"lib\net40" @"..\bin\net40\FsPickler.dll"
                ]
        })
        ("nuget/FsPickler.nuspec")
)

Target "NuGet.FsPickler.Json" (fun _ ->
    let nugetPath = ".nuget/NuGet.exe"
    NuGet (fun p -> 
        { p with   
            Authors = authors
            Project = "FsPickler.Json"
            Summary = summary
            Description = description
            Version = nugetVersion
            ReleaseNotes = String.concat " " release.Notes
            Tags = tags
            OutputPath = "bin"
            ToolPath = nugetPath
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Dependencies = [("FsPickler", RequireExactly release.NugetVersion) ; ("Newtonsoft.Json", "6.0.5")] 
            Publish = hasBuildParam "nugetkey" 
            Files =
                [
                    yield! addAssembly @"lib\net45" @"..\bin\FsPickler.Json.dll"
                    yield! addAssembly @"lib\net40" @"..\bin\net40\FsPickler.Json.dll"
                ]
            
        })
        ("nuget/FsPickler.nuspec")
)

Target "NuGet.FsPickler.CSharp" (fun _ ->
    let nugetPath = ".nuget/NuGet.exe"
    NuGet (fun p -> 
        { p with   
            Authors = authors
            Project = "FsPickler.CSharp"
            Summary = summary
            Description = description
            Version = nugetVersion
            ReleaseNotes = String.concat " " release.Notes
            Tags = tags
            OutputPath = "bin"
            ToolPath = nugetPath
            AccessKey = getBuildParamOrDefault "nugetkey" ""
            Dependencies = 
                [
                    ("FSharp.Core", "3.1.2.1")
                    ("FsPickler", RequireExactly release.NugetVersion)
                    ("FsPickler.Json", RequireExactly release.NugetVersion)
                ] 
            Publish = hasBuildParam "nugetkey" 
            Files =
                [
                    yield! addAssembly @"lib\net45" @"..\bin\FsPickler.CSharp.dll"
                    yield! addAssembly @"lib\net40" @"..\bin\net40\FsPickler.CSharp.dll"
                ]
        })
        ("nuget/FsPickler.nuspec")
)

// Doc generation

Target "GenerateDocs" (fun _ ->
    executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"] [] |> ignore
)

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    CleanDir tempDocsDir
    Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir

    fullclean tempDocsDir
    CopyRecursive "docs/output" tempDocsDir true |> tracefn "%A"
    StageAll tempDocsDir
    Commit tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "Prepare" DoNothing
Target "PrepareRelease" DoNothing
Target "Build" DoNothing
Target "NuGet" DoNothing
Target "Default" DoNothing
Target "Release" DoNothing

"Clean"
  ==> "RestorePackages"
  ==> "AssemblyInfo"
  ==> "Prepare"
  ==> "Build.Default"
  ==> "Build.NoEmit"
  ==> "Build.Net40"
  ==> "Build"
  ==> "RunTests"
  ==> "Default"

"Build"
  ==> "PrepareRelease"
  ==> "NuGet.FsPickler"
  ==> "NuGet.FsPickler.Json"
  ==> "NuGet.FsPickler.CSharp"
  ==> "NuGet"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"
  ==> "Release"

RunTargetOrDefault "Default"