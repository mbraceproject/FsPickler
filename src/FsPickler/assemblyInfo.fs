namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.9.7")>]
[<assembly: AssemblyFileVersionAttribute("0.9.7")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.9.7"
