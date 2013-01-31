namespace FsCoreSerializer


    module Utils =
        
        open System
        open System.Collections.Generic
        open System.IO
        open System.Threading

        type Cell<'T>(x : 'T ref) = member __.Value = x.contents
        and 'T cell = Cell<'T>

        type Atom<'T when 'T : not struct>(value : 'T) =
            let refCell = ref value
    
            let rec swap f = 
                let currentValue = !refCell
                let result = Interlocked.CompareExchange<'T>(refCell, f currentValue, currentValue)
                if obj.ReferenceEquals(result, currentValue) then ()
                else Thread.SpinWait 20; swap f

            let transact f =
                let output = ref Unchecked.defaultof<'S>
                let f' x = let t,s = f x in output := s ; t
                swap f' ; !output
        
            member __.Value with get() : 'T = !refCell
            member __.Swap (f : 'T -> 'T) : unit = swap f
            member __.Set (v : 'T) : unit = swap (fun _ -> v)
            member __.Transact (f : 'T -> 'T * 'S) : 'S = transact f  
            member __.Cell = Cell refCell

        [<RequireQualifiedAccess>]
        module Atom =
            let atom x = Atom<_>(x)
            let get (a : Atom<_>) = a.Value
            let swap (f : 'T -> 'T) (a : Atom<_>) = a.Swap f
            let transact (f : 'T -> 'T * 'R) (a : Atom<_>) = a.Transact f


        type IDictionary<'K,'V> with
            member d.TryFind (k : 'K) =
                let v = ref Unchecked.defaultof<'V>
                if d.TryGetValue(k,v) then Some !v else None


        /// fixpoint combinator for parametric mutual recursion
        let YParametric (initial : 'a -> 'b) (F : ('a -> 'b cell) -> ('b -> 'b cell) -> 'a -> 'b cell) : 'a -> 'b =
            let dict = new System.Collections.Generic.Dictionary<'a, 'b cell> ()

            let rec recurse (x : 'a) =
                match dict.TryFind x with
                | None ->
                    let r = ref (initial x)
                    let c = Cell r
                    dict.Add(x, c)
                    F recurse (fun b -> r := b; c) x
                | Some c -> c

            fun x -> (recurse x).Value


        let inline writeInt (stream : Stream) (n : int) =
            let buf = Array.zeroCreate sizeof<int>
            let mutable x = n
            for i = 0 to buf.Length - 1 do
                buf.[i] <- byte (x % 256)
                x <- x / 256
            stream.Write(buf, 0, buf.Length)

        let inline readInt (stream : Stream) = 
            let buf = Array.zeroCreate sizeof<int>
            stream.Read(buf, 0, buf.Length) |> ignore
            let mutable x = 0
            for i = 0 to buf.Length - 1 do
                x <- x + (int buf.[i] <<< 8 * i)
            x