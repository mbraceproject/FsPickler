namespace System
open System.Reflection

[<assembly: AssemblyProductAttribute("FsPickler")>]
[<assembly: AssemblyCopyrightAttribute("© Eirik Tsarpalis.")>]
[<assembly: AssemblyVersionAttribute("1.8.0")>]
[<assembly: AssemblyFileVersionAttribute("1.8.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.8.0"
