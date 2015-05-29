namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("1.2.3")>]
[<assembly: AssemblyFileVersionAttribute("1.2.3")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.2.3"
