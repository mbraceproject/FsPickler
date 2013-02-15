namespace FsCoreSerializer


    module Utils =
        
        open System
        open System.Collections.Generic
        open System.IO
        open System.Threading

        type Cell<'T> = abstract Value : 'T
        and 'T cell = Cell<'T>
        and ReadOnlyCell<'T>(r : 'T ref) =
            interface Cell<'T> with member __.Value = r.contents

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
            member __.Cell = ReadOnlyCell refCell :> 'T cell

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
        let YParametric (F : ('a -> Lazy<'b>) -> 'a -> 'b) (x : 'a) =
            let dict = new System.Collections.Generic.Dictionary<'a, Lazy<'b>> ()

            let rec recurse (x : 'a) =
                match dict.TryFind x with
                | None ->
                    let r = ref None
                    let l = lazy (
                        match r.Value with
                        | None -> failwith "attemping to consume at construction time!"
                        | Some v -> v)
                    dict.Add(x, l)
                    r := Some (F recurse x)
                    let _ = l.Force() in l
                | Some l -> l

            (recurse x).Value

        let (|ValueCreated|_|) (x : Lazy<'T>) =
            if x.IsValueCreated then Some x.Value else None

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