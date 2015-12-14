namespace System
open System.Reflection

[<assembly: AssemblyProductAttribute("FsPickler")>]
[<assembly: AssemblyCopyrightAttribute("© Eirik Tsarpalis.")>]
[<assembly: AssemblyVersionAttribute("1.7.0")>]
[<assembly: AssemblyFileVersionAttribute("1.7.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.7.0"
