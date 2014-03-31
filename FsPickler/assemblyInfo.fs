namespace Nessos.FsPickler

    open System.Reflection

    [<assembly:AssemblyVersion("0.8.5.*")>]
    do ()

    module internal Config =
        [<Literal>]
        let optimizeForLittleEndian = true
