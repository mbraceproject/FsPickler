namespace System
open System.Reflection

[<assembly: AssemblyProductAttribute("FsPickler")>]
[<assembly: AssemblyCopyrightAttribute("© Eirik Tsarpalis.")>]
[<assembly: AssemblyVersionAttribute("2.3.0")>]
[<assembly: AssemblyFileVersionAttribute("2.3.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.3.0"
