namespace FsPickler

    open System.Reflection

#if USE_STRONG_NAMES
    [<assembly:AssemblyKeyFile("../../Lib/key.snk")>]
#endif
    [<assembly:AssemblyVersion("0.6.2.*")>]
    do()