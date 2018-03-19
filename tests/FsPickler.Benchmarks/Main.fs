module MBrace.FsPickler.Benchmarks.Main

open System.Reflection
open BenchmarkDotNet.Running

let getAllBenchmarks () =
    let assembly = Assembly.GetExecutingAssembly()
    assembly.GetTypes()
    |> Array.filter typeof<RoundtripBenchmark>.IsAssignableFrom
    |> Array.filter (fun t -> not t.IsAbstract)

[<EntryPoint>]
let main args =

    let allBenchmarks = getAllBenchmarks ()
    let switcher = new BenchmarkSwitcher(allBenchmarks)
    let summaries = switcher.Run args

    0