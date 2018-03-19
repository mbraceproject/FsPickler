module MBrace.FsPickler.Benchmarks.Main

open BenchmarkDotNet.Running

[<EntryPoint>]
let main _ =
    //let _summary = BenchmarkRunner.Run<LargeObject.Roundtrip>()
    //let _summary = BenchmarkRunner.Run<Poco.Roundtrip>()
    let _summary = BenchmarkRunner.Run<ISerializable.Roundtrip>()
    0