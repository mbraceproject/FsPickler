namespace MBrace.FsPickler.CSharpProxy

open System.Runtime.CompilerServices

type Option = 
    static member Some<'a> (v : 'a) : 'a option = Some v
    static member None<'a>() : 'a option = None

[<Extension>]
type FSharpFunc =
    [<Extension>]static member ToFSharpFunc<'a,'b> (func:System.Converter<'a,'b>) : 'a -> 'b = fun a -> func.Invoke(a)
    [<Extension>]static member ToFSharpFunc<'a>(func:System.Predicate<'a>) : 'a -> bool = fun a -> func.Invoke(a)

    [<Extension>]static member ToFSharpFunc<'a> (func:System.Func<'a>) : unit -> 'a = fun () -> func.Invoke()
    [<Extension>]static member ToFSharpFunc<'a,'b> (func:System.Func<'a,'b>) : 'a -> 'b = fun a -> func.Invoke(a)
    [<Extension>]static member ToFSharpFunc<'a,'b,'c> (func:System.Func<'a,'b,'c>) : 'a -> 'b -> 'c = fun a b -> func.Invoke(a,b)
    [<Extension>]static member ToFSharpFunc<'a,'b,'c,'d> (func:System.Func<'a,'b,'c,'d>) : 'a -> 'b -> 'c -> 'd = fun a b c -> func.Invoke(a,b,c)
    [<Extension>]static member ToFSharpFunc<'a,'b,'c,'d,'e> (func:System.Func<'a,'b,'c,'d,'e>) : 'a -> 'b -> 'c -> 'd -> 'e = fun a b c d -> func.Invoke(a,b,c,d)
    [<Extension>]static member ToFSharpFunc<'a,'b,'c,'d,'e,'f> (func:System.Func<'a,'b,'c,'d,'e,'f>) : 'a -> 'b -> 'c -> 'd -> 'e -> 'f = fun a b c d e -> func.Invoke(a,b,c,d,e)
    [<Extension>]static member ToFSharpFunc<'a,'b,'c,'d,'e,'f,'g> (func:System.Func<'a,'b,'c,'d,'e,'f,'g>) : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g = fun a b c d e f -> func.Invoke(a,b,c,d,e,f)
    [<Extension>]static member ToFSharpFunc<'a,'b,'c,'d,'e,'f,'g,'h> (func:System.Func<'a,'b,'c,'d,'e,'f,'g,'h>) : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h = fun a b c d e f g -> func.Invoke(a,b,c,d,e,f,g)
    [<Extension>]static member ToFSharpFunc<'a,'b,'c,'d,'e,'f,'g,'h,'i> (func:System.Func<'a,'b,'c,'d,'e,'f,'g,'h,'i>) : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i = fun a b c d e f g h -> func.Invoke(a,b,c,d,e,f,g,h)
    [<Extension>]static member ToFSharpFunc<'a,'b,'c,'d,'e,'f,'g,'h,'i,'j> (func:System.Func<'a,'b,'c,'d,'e,'f,'g,'h,'i,'j>) : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j = fun a b c d e f g h i -> func.Invoke(a,b,c,d,e,f,g,h,i)
    [<Extension>]static member ToFSharpFunc<'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k> (func:System.Func<'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k>) : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k = fun a b c d e f g h i j -> func.Invoke(a,b,c,d,e,f,g,h,i,j)

    [<Extension>]static member ToFSharpFunc(func:System.Action) : unit -> unit = fun () -> func.Invoke()
    [<Extension>]static member ToFSharpFunc<'a>(func:System.Action<'a>) : 'a -> unit = fun a -> func.Invoke(a)
    [<Extension>]static member ToFSharpFunc<'a,'b>(func:System.Action<'a,'b>) : 'a -> 'b -> unit = fun a b -> func.Invoke(a,b)
    [<Extension>]static member ToFSharpFunc<'a,'b,'c>(func:System.Action<'a,'b,'c>) : 'a -> 'b -> 'c -> unit = fun a b c -> func.Invoke(a,b,c)
    [<Extension>]static member ToFSharpFunc<'a,'b,'c,'d>(func:System.Action<'a,'b,'c,'d>) : 'a -> 'b -> 'c -> 'd -> unit = fun a b c d -> func.Invoke(a,b,c,d)
    [<Extension>]static member ToFSharpFunc<'a,'b,'c,'d,'e>(func:System.Action<'a,'b,'c,'d,'e>) : 'a -> 'b -> 'c -> 'd -> 'e -> unit = fun a b c d e -> func.Invoke(a,b,c,d,e)
    [<Extension>]static member ToFSharpFunc<'a,'b,'c,'d,'e,'f>(func:System.Action<'a,'b,'c,'d,'e,'f>) : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> unit = fun a b c d e f -> func.Invoke(a,b,c,d,e,f)
    [<Extension>]static member ToFSharpFunc<'a,'b,'c,'d,'e,'f,'g>(func:System.Action<'a,'b,'c,'d,'e,'f,'g>) : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> unit = fun a b c d e f g -> func.Invoke(a,b,c,d,e,f,g)
    [<Extension>]static member ToFSharpFunc<'a,'b,'c,'d,'e,'f,'g,'h>(func:System.Action<'a,'b,'c,'d,'e,'f,'g,'h>) : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> unit = fun a b c d e f g h -> func.Invoke(a,b,c,d,e,f,g,h)