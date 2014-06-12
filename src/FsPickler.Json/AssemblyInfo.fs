namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.9.4")>]
[<assembly: AssemblyFileVersionAttribute("0.9.4")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.9.4"
