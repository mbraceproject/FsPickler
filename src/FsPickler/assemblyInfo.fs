namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("1.2.0")>]
[<assembly: AssemblyFileVersionAttribute("1.2.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.2.0"
