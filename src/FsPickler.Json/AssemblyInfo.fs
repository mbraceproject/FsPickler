namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("1.2.5")>]
[<assembly: AssemblyFileVersionAttribute("1.2.5")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.2.5"
