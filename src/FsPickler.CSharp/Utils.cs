using System;
using System.Text;
using System.Runtime.Serialization;

using Microsoft.FSharp.Core;

namespace Nessos.CsPickler
{
    internal static class Utils
    {
        public static FSharpOption<Encoding> GetEncoding(Encoding e)
        {
            if (e == null) e = Encoding.UTF8;

            return new FSharpOption<Encoding>(e);
        }

        public static FSharpOption<StreamingContext> GetStreamingContext(Object context)
        {
            StreamingContext ctx;

            if (context == null)
                ctx = new StreamingContext(StreamingContextStates.All);
            else
                ctx = new StreamingContext(StreamingContextStates.All, context);

            return new FSharpOption<StreamingContext>(ctx);
        }

        public static FSharpOption<bool> GetLeaveOpen(bool leaveOpen)
        {
            return new FSharpOption<bool>(leaveOpen);
        }
    }
}
