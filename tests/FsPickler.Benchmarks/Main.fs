module MBrace.FsPickler.Benchmarks.Main

open System.Reflection
open BenchmarkDotNet.Running

[<EntryPoint>]
let main args =
    let assembly = Assembly.GetExecutingAssembly()
    let switcher = new BenchmarkSwitcher(assembly)
    let summaries = switcher.Run args
    0