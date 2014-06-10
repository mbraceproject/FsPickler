namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.8.9")>]
[<assembly: AssemblyFileVersionAttribute("0.8.9")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.8.9"
