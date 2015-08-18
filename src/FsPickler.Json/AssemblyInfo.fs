namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsPickler")>]
[<assembly: AssemblyProductAttribute("FsPickler")>]
[<assembly: AssemblyCopyrightAttribute("© Eirik Tsarpalis.")>]
[<assembly: AssemblyVersionAttribute("1.3.3")>]
[<assembly: AssemblyFileVersionAttribute("1.3.3")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.3.3"
