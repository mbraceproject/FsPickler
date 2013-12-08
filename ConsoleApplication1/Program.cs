using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using FsPickler;

namespace ConsoleApplication1
{
    class Program
    {
        static void Main(string[] args)
        {
            var x = new Tuple<int, string>(42, "poutses mple");
            var fsp = new FsPickler.FsPickler();

            var p = fsp.GeneratePickler<Tuple<string, int>> ();

            var bytes = fsp.Pickle(x);
            var y = fsp.UnPickle<Tuple<int, string>>(bytes);

            Console.WriteLine(x.Equals(y));

            Console.ReadLine();
        }
    }
}
