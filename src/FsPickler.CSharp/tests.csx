#r "../../bin/Newtonsoft.Json.dll"
#r "../../bin/FsPickler.dll"
#r "../../bin/FsPickler.Json.dll"
#r "../../bin/FsPickler.CSharp.dll"

using MBrace.CsPickler;

var serializer = CsPickler.CreateJsonSerializer(indent: true, omitHeader: true);

var json = serializer.PickleToString<Func<int, int>>(x => x + 1);

Console.WriteLine(json)