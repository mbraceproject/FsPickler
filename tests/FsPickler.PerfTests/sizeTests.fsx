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

plot "System.Int32" 1
plot "Tuple<int, string>" (42, "lorem ipsum")
plot "F# list of integers (1000 elements)" [1..1000]
plot "Array of integers (1000000 elements)" [|1 .. 1000000|]
plot "Array of pairs (10000 elements)" ([|1 .. 10000|] |> Array.map (fun i -> string i,i))
plot "F# list of pairs (1000 elements)" <| List.init 1000 (fun i -> (i, string i))
plot "3D array of floats (100x100x100)" <| Array3D.init 100 100 100 (fun i j k -> i + 100 * j + 100000 * k)
plot "Exception with stacktrace" <| mkExceptionWithStackTrace()
plot "F# Quotation" <@ let rec f n = if n <= 1 then n else f (n-1) + f(n-2) in f 10 @>
plot "Simple POCO" <| Entry()
plot "Dictionary of POCOs (1000 entries)" (Seq.init 1000 (fun i -> (i, Entry())) |> dict)
plot "Binary tree (balanced, depth = 10)" <| mkClassTree 10


report "System.Int32" 1
report "Tuple<int, string>" (42, "lorem ipsum")
report "F# list of integers (1000 elements)" [1..1000]
report "Array of integers (1000000 elements)" [|1 .. 1000000|]
report "Array of pairs (10000 elements)" ([|1 .. 10000|] |> Array.map (fun i -> string i,i))
report "F# list of pairs (1000 elements)" <| List.init 1000 (fun i -> (i, string i))
report "3D array of floats (100x100x100)" <| Array3D.init 100 100 100 (fun i j k -> i + 100 * j + 100000 * k)
report "Exception with stacktrace" <| mkExceptionWithStackTrace()
report "F# Quotation" <@ let rec f n = if n <= 1 then n else f (n-1) + f(n-2) in f 10 @>
report "Simple POCO" <| Entry()
report "Dictionary of POCOs (1000 entries)" (Seq.init 1000 (fun i -> (i, Entry())) |> dict)
report "Binary tree (balanced, depth = 10)" <| mkClassTree 10