module internal Nessos.FsPickler.RootSerialization

    open System
    open System.Collections
    open System.Collections.Generic
    open System.Text
    open System.IO
    open System.Runtime.Serialization
    
    open Nessos.FsPickler
    open Nessos.FsPickler.TypeCache

    let initStreamWriter (formatP : IPickleFormatProvider) stream encoding leaveOpen =
        let encoding = defaultArg encoding Encoding.UTF8
        let leaveOpen = defaultArg leaveOpen false
        formatP.CreateWriter(stream, encoding, leaveOpen)

    let initStreamReader (formatP : IPickleFormatProvider) stream encoding leaveOpen =
        let encoding = defaultArg encoding Encoding.UTF8
        let leaveOpen = defaultArg leaveOpen false
        formatP.CreateReader(stream, encoding, leaveOpen)

    let initTextWriter (formatP : ITextPickleFormatProvider) writer leaveOpen =
        let leaveOpen = defaultArg leaveOpen false
        formatP.CreateWriter(writer, leaveOpen)

    let initTextReader (formatP : ITextPickleFormatProvider) reader leaveOpen =
        let leaveOpen = defaultArg leaveOpen false
        formatP.CreateReader(reader, leaveOpen)

    let writeRootObject resolver reflectionCache formatter streamingContext (pickler : Pickler<'T>) (value : 'T) =
        let writeState = new WriteState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qualifiedName = reflectionCache.GetQualifiedName pickler.Type
        formatter.BeginWriteRoot qualifiedName
        pickler.Write writeState "value" value
        formatter.EndWriteRoot ()

    let readRootObject resolver reflectionCache formatter streamingContext (pickler : Pickler<'T>) =
        let readState = new ReadState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qualifiedName = reflectionCache.GetQualifiedName pickler.Type

        try formatter.BeginReadRoot qualifiedName
        with 
        | :? FsPicklerException -> reraise () 
        | e -> raise <| new InvalidPickleException("error reading from pickle.", e)

        let value = pickler.Read readState "value"
        formatter.EndReadRoot ()
        value

    let writeRootObjectUntyped resolver reflectionCache formatter streamingContext (pickler : Pickler) (value : obj) =
        let writeState = new WriteState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qualifiedName = reflectionCache.GetQualifiedName pickler.Type
        formatter.BeginWriteRoot qualifiedName
        pickler.UntypedWrite writeState "value" value
        formatter.EndWriteRoot ()

    let readRootObjectUntyped resolver reflectionCache formatter streamingContext (pickler : Pickler) =
        let readState = new ReadState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qualifiedName = reflectionCache.GetQualifiedName pickler.Type
        try formatter.BeginReadRoot qualifiedName
        with 
        | :? FsPicklerException -> reraise () 
        | e -> raise <| new InvalidPickleException("error reading from pickle.", e)

        let value = pickler.UntypedRead readState "value"
        formatter.EndReadRoot ()
        value

    //
    //  top-level sequence serialization
    //

    let private sequenceStateResetThreshold = 1000 // resets write/read state at the given interval

    /// serializes a sequence of objects to stream

    let writeTopLevelSequence resolver reflectionCache formatter streamingContext (pickler : Pickler<'T>) (values : seq<'T>) : int =
        // write state initialization
        let state = new WriteState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qn = reflectionCache.GetQualifiedName pickler.Type + " seq"

        state.Formatter.BeginWriteRoot qn
        state.Formatter.BeginWriteUnBoundedSequence "values"

        let isObject = pickler.TypeKind > TypeKind.Value

#if DEBUG
        let write idx t =
#else
        let inline write idx t =
#endif
            if isObject && idx % sequenceStateResetThreshold = 0 then
                state.ResetCounters()
            
            formatter.WriteHasNextElement true
            pickler.Write state "elem" t
            
        // specialize enumeration
        let length =
            match values with
            | :? ('T []) as array ->
                let n = array.Length
                for i = 0 to n - 1 do
                    write i array.[i]
                n

            | :? ('T list) as list ->
                let rec writeLst i lst =
                    match lst with
                    | [] -> i
                    | t :: ts -> write i t ; writeLst (i+1) ts

                writeLst 0 list
            | _ ->
                let mutable i = 0
                for t in values do
                    write i t
                    i <- i + 1
                i

        state.Formatter.WriteHasNextElement false
        state.Formatter.EndWriteRoot()
        length


    type SequenceDeserializationEnumerator<'T>(state : ReadState, pickler : Pickler<'T>) =
        let formatter = state.Formatter
        let isObject = pickler.TypeKind > TypeKind.Value
        let mutable count = 0
        let mutable value = Unchecked.defaultof<'T>

        interface IEnumerator<'T> with
            member __.Current = value
            member __.Current = box value
            member __.Dispose () = formatter.EndReadRoot() ; formatter.Dispose()
            member __.MoveNext () =
                if formatter.ReadHasNextElement () then
                    if isObject && count % sequenceStateResetThreshold = 0 then
                        state.ResetCounters()

                    value <- pickler.Read state "elem"

                    true
                else
                    false

            member __.Reset () = raise <| NotSupportedException()

    let readTopLevelSequence resolver reflectionCache formatter streamingContext (pickler : Pickler<'T>) : seq<'T> =
        
        // read state initialization
        let state = new ReadState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qn = reflectionCache.GetQualifiedName pickler.Type + " seq"

        // read stream header
        try formatter.BeginReadRoot qn
        with 
        | :? FsPicklerException -> reraise () 
        | e -> raise <| new InvalidPickleException("error reading from pickle.", e)

        formatter.BeginReadUnBoundedSequence "values"

        let isConsumed = ref 0
        let getEnumerator () =
            if System.Threading.Interlocked.CompareExchange(isConsumed, 1, 0) = 0 then
                new SequenceDeserializationEnumerator<'T>(state, pickler) :> IEnumerator<'T>
            else
                invalidOp "Deserialization enumerable can only be consumed once."
        {
            new IEnumerable<'T> with
                member __.GetEnumerator () = getEnumerator ()
                member __.GetEnumerator () = getEnumerator () :> IEnumerator
        }



    let writeTopLevelSequenceUntyped resolver reflectionCache formatter streamingContext (pickler : Pickler) (values : IEnumerable) : int =
        let unpacker =
            {
                new IPicklerUnpacker<int> with
                    member __.Apply (p : Pickler<'T>) =
                        writeTopLevelSequence resolver reflectionCache formatter
                            streamingContext p (values :?> IEnumerable<'T>)
            }

        pickler.Unpack unpacker

    let readTopLevelSequenceUntyped resolver reflectionCache formatter streamingContext (pickler : Pickler) : IEnumerable =
        let unpacker =
            {
                new IPicklerUnpacker<IEnumerable> with
                    member __.Apply (p : Pickler<'T>) =
                        readTopLevelSequence resolver reflectionCache formatter 
                            streamingContext p :> IEnumerable
            }

        pickler.Unpack unpacker