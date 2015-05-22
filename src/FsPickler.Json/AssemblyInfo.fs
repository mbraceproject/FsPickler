namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("1.2.2")>]
[<assembly: AssemblyFileVersionAttribute("1.2.2")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.2.2"
