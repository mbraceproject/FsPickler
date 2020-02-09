// --------------------------------------------------------------------------------------
// Builds the documentation from `.fsx` and `.md` files in the 'docs/content' directory
// (the generated documentation is stored in the 'docs/output' directory)
// --------------------------------------------------------------------------------------

// Binaries that have XML documentation (in a corresponding generated XML file)
let referenceProjects = [ "../../src/FsPickler" ; "../../src/FsPickler.Json" ]
// Web site location for the generated documentation
let website = "/FsPickler"

let githubLink = "http://github.com/mbraceproject/FsPickler"

// Specify more information about your project
let info =
  [ "project-name", "FsPickler"
    "project-author", "Eirik Tsarpalis"
    "project-summary", "A general-purpose serialization library for .NET"
    "project-github", githubLink
    "project-nuget", "http://www.nuget.org/packages/FsPickler" ]

// --------------------------------------------------------------------------------------
// For typical project, no changes are needed below
// --------------------------------------------------------------------------------------

#I "../../packages/build/FSharp.Compiler.Service/lib/net45"
#I "../../packages/build/FSharp.Formatting/lib/net461"
#r "../../packages/build/FAKE/tools/FakeLib.dll"
#r "RazorEngine.dll"
#r "FSharp.Markdown.dll"
#r "FSharp.Literate.dll"
#r "FSharp.CodeFormat.dll"
#r "FSharp.MetadataFormat.dll"
#r "FSharp.Formatting.Common.dll"
#r "FSharp.Formatting.Razor.dll"

open Fake
open System.IO
open FSharp.Formatting.Razor

// When called from 'build.fsx', use the public project URL as <root>
// otherwise, use the current 'output' directory.
#if RELEASE
let root = website
#else
let root = "file://" + (__SOURCE_DIRECTORY__ @@ "../output")
#endif

// Paths with template/source/output locations
let bin        = __SOURCE_DIRECTORY__ @@ "../../bin"
let content    = __SOURCE_DIRECTORY__ @@ "../content"
let output     = __SOURCE_DIRECTORY__ @@ "../output"
let files      = __SOURCE_DIRECTORY__ @@ "../files"
let templates  = __SOURCE_DIRECTORY__ @@ "templates"
let formatting = __SOURCE_DIRECTORY__ @@ "../../packages/build/FSharp.Formatting/"
let docTemplate = formatting @@ "templates/docpage.cshtml"

// Where to look for *.csproj templates (in this order)
let layoutRoots =
  [ templates; formatting @@ "templates"
    formatting @@ "templates/reference" ]

// Copy static files and CSS + JS from F# Formatting
let copyFiles () =
  ensureDirectory (output @@ "content")
  CopyRecursive (formatting @@ "styles") (output @@ "content") true 
    |> Log "Copying styles and scripts: "
  CopyRecursive files output true |> Log "Copying file: "


let getReferenceAssembliesForProject (proj : string) =
    let projName = Path.GetFileName proj
    !! (proj @@ "bin/Release/netstandard*/" + projName + ".dll") |> Seq.head

// Build API reference from XML comments
let buildReference () =
  CleanDir (output @@ "reference")
  let binaries = referenceProjects |> List.map getReferenceAssembliesForProject
  printfn "%A" binaries
  RazorMetadataFormat.Generate
    ( binaries, output @@ "reference", layoutRoots, 
      parameters = ("root", root)::info,
      sourceRepo = githubLink @@ "tree/master",
      sourceFolder = __SOURCE_DIRECTORY__ @@ ".." @@ "..",
      publicOnly = true )

// Build documentation from `fsx` and `md` files in `docs/content`
let buildDocumentation () =
  let subdirs = Directory.EnumerateDirectories(content, "*", SearchOption.AllDirectories)
  for dir in Seq.append [content] subdirs do
    let sub = if dir.Length > content.Length then dir.Substring(content.Length + 1) else "."
    RazorLiterate.ProcessDirectory
      ( dir, docTemplate, output @@ sub, replacements = ("root", root)::info,
        layoutRoots = layoutRoots, generateAnchors = true )

// Generate
ensureDirectory output
CleanDir output
copyFiles()
buildDocumentation()
buildReference()