#I "../../bin"

#r "Newtonsoft.Json.dll"
#r "PerfUtil.dll"
#r "ProtoBuf-Net.dll"
#r "Wire.dll"
#r "nunit.framework.dll"
#r "FsPickler.dll"
#r "FsPickler.PerfTests.dll"

open Nessos.FsPickler
open Nessos.FsPickler.Tests
open Nessos.FsPickler.Tests.SerializationSize

plot "integer" 1
plot "pair" (42, "lorem ipsum")
plot "int list (1000 elements)" [1..1000]
plot "int array (1000000 elements)" [|1 .. 1000000|]
plot "pair array (10000 elements)" ([|1 .. 10000|] |> Array.map (fun i -> string i,i))
plot "list of pairs (1000 elements)" <| List.init 1000 (fun i -> (i, string i))
plot "3D float array (100x100x100)" <| Array3D.init 100 100 100 (fun i j k -> i + 100 * j + 100000 * k)
plot "exception with stacktrace" <| mkExceptionWithStackTrace()
plot "F# quotation" <@ let rec f n = if n <= 1 then n else f (n-1) + f(n-2) in f 10 @>
plot "Single Record" <| Entry()
plot "Dictionary of Records (1000 entries)" (Seq.init 1000 (fun i -> (i, Entry())) |> dict)
plot "Binary tree (balanced, depth = 10)" <| mkClassTree 10


report "integer" 1
report "pair" (42, "lorem ipsum")
report "int list (1000 elements)" [1..1000]
report "int array (1000000 elements)" [|1 .. 1000000|]
report "pair array (10000 elements)" ([|1 .. 10000|] |> Array.map (fun i -> string i,i))
report "list of pairs (1000 elements)" <| List.init 1000 (fun i -> (i, string i))
report "3D float array (100x100x100)" <| Array3D.init 100 100 100 (fun i j k -> i + 100 * j + 100000 * k)
report "exception with stacktrace" <| mkExceptionWithStackTrace()
report "F# quotation" <@ let rec f n = if n <= 1 then n else f (n-1) + f(n-2) in f 10 @>
report "Single Record" <| Entry()
report "Dictionary of Records (1000 entries)" (Seq.init 1000 (fun i -> (i, Entry())) |> dict)
report "Binary tree (balanced, depth = 10)" <| mkClassTree 10