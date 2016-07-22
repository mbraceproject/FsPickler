using System;
using System.Text;
using System.Runtime.Serialization;

using Microsoft.FSharp.Core;
using MBrace.FsPickler.CSharpProxy;

namespace MBrace.CsPickler
{
    internal static class Utils
    {
        public static FSharpOption<T> ToOption<T>(this T value)
        {
            if (value == null) return Option.None<T>();
            return Option.Some(value);
        }

        public static StreamingContext GetStreamingContext(Object context)
        {
            if (context == null) return new StreamingContext(StreamingContextStates.All);
            return new StreamingContext(StreamingContextStates.All, context);
        }
    }
}
