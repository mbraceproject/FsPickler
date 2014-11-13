namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("1.0.4")>]
[<assembly: AssemblyFileVersionAttribute("1.0.4")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.4"
