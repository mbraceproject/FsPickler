namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("1.2.6")>]
[<assembly: AssemblyFileVersionAttribute("1.2.6")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.2.6"
