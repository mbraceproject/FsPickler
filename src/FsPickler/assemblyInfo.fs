namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("1.0.13")>]
[<assembly: AssemblyFileVersionAttribute("1.0.13")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.13"
