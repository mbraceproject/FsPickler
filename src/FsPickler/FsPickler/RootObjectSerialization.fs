module internal Nessos.FsPickler.RootObjectSerialization

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
        let leaveOpen = defaultArg leaveOpen true
        formatP.CreateWriter(stream, encoding, leaveOpen)

    let initStreamReader (formatP : IPickleFormatProvider) stream encoding leaveOpen =
        let encoding = defaultArg encoding Encoding.UTF8
        let leaveOpen = defaultArg leaveOpen true
        formatP.CreateReader(stream, encoding, leaveOpen)

    let writeRootObject resolver reflectionCache formatter streamingContext (pickler : Pickler<'T>) (value : 'T) =
        use writeState = new WriteState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qualifiedName = reflectionCache.GetQualifiedName pickler.Type
        formatter.BeginWriteRoot qualifiedName
        pickler.Write writeState "value" value
        formatter.EndWriteRoot ()

    let readRootObject resolver reflectionCache formatter streamingContext (pickler : Pickler<'T>) =
        use readState = new ReadState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qualifiedName = reflectionCache.GetQualifiedName pickler.Type
        let rootName = formatter.BeginReadRoot ()
        if rootName <> qualifiedName then
            raise <| new InvalidPickleTypeException(qualifiedName, rootName)

        let value = pickler.Read readState "value"
        formatter.EndReadRoot ()
        value

    let writeRootObjectUntyped resolver reflectionCache formatter streamingContext (pickler : Pickler) (value : obj) =
        use writeState = new WriteState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qualifiedName = reflectionCache.GetQualifiedName pickler.Type
        formatter.BeginWriteRoot qualifiedName
        pickler.UntypedWrite writeState "value" value
        formatter.EndWriteRoot ()

    let readRootObjectUntyped resolver reflectionCache formatter streamingContext (pickler : Pickler) =
        use readState = new ReadState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qualifiedName = reflectionCache.GetQualifiedName pickler.Type
        let rootName = formatter.BeginReadRoot ()
        if rootName <> qualifiedName then
            raise <| new InvalidPickleTypeException(qualifiedName, rootName)

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
        use state = new WriteState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qn = sprintf "%s[]" <| reflectionCache.GetQualifiedName pickler.Type

        state.Formatter.BeginWriteRoot qn

        let isObject = pickler.TypeKind > TypeKind.Value

#if DEBUG
        let write idx t =
#else
        let inline write idx t =
#endif
            if isObject && idx % sequenceStateResetThreshold = 0 then
                state.ResetCounters()

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

        state.Formatter.EndWriteRoot()
        length



    let readTopLevelSequence resolver reflectionCache formatter streamingContext (pickler : Pickler<'T>) (length : int) : IEnumerator<'T> =
        
        // read state initialization
        let state = new ReadState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qn = sprintf "%s[]" <| reflectionCache.GetQualifiedName pickler.Type

        // read stream header
        let rootName = formatter.BeginReadRoot ()
        if rootName <> qn then
            raise <| new InvalidPickleTypeException(qn, rootName)

        let isObject = pickler.TypeKind > TypeKind.Value

        let read idx =
            if isObject && idx % sequenceStateResetThreshold = 0 then
                state.ResetCounters()
                
            pickler.Read state "elem"

        let cnt = ref 0
        let curr = ref Unchecked.defaultof<'T>
        {
            new System.Collections.Generic.IEnumerator<'T> with
                member __.Current = !curr
                member __.Current = box !curr
                member __.Dispose () = state.Formatter.EndReadRoot() ; (state :> IDisposable).Dispose()
                member __.MoveNext () =
                    if !cnt < length then
                        curr := read !cnt
                        incr cnt
                        true
                    else
                        false

                member __.Reset () = raise <| NotSupportedException()
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

    let readTopLevelSequenceUntyped resolver reflectionCache formatter streamingContext (pickler : Pickler) (length : int) : IEnumerator =
        let unpacker =
            {
                new IPicklerUnpacker<IEnumerator> with
                    member __.Apply (p : Pickler<'T>) =
                        readTopLevelSequence resolver reflectionCache formatter 
                            streamingContext p length :> IEnumerator
            }

        pickler.Unpack unpacker