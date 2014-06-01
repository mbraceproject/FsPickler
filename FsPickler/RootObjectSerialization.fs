module internal Nessos.FsPickler.RootObjectSerialization

    open System
    open System.Collections
    open System.Collections.Generic
    open System.IO
    open System.Runtime.Serialization
    
    open Nessos.FsPickler
    open Nessos.FsPickler.TypeCache

    let writeRootObject (formatP : IPickleFormatProvider) resolver reflectionCache 
                        streamingContext outStream (pickler : Pickler<'T>) (value : 'T) =
        
        let formatter = formatP.CreateWriter outStream
        use writeState = new WriteState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qualifiedName = reflectionCache.GetQualifiedName pickler.Type
        formatter.BeginWriteRoot qualifiedName
        pickler.Write writeState "value" value
        formatter.EndWriteRoot ()

    let readRootObject (formatP : IPickleFormatProvider) resolver reflectionCache 
                        streamingContext inStream (pickler : Pickler<'T>) =

        let formatter = formatP.CreateReader inStream
        use readState = new ReadState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qualifiedName = reflectionCache.GetQualifiedName pickler.Type
        formatter.BeginReadRoot qualifiedName
        let value = pickler.Read readState "value"
        formatter.EndReadRoot ()
        value

    let writeRootObjectUntyped (formatP : IPickleFormatProvider) resolver reflectionCache 
                        streamingContext outStream (pickler : Pickler) (value : obj) =
        
        let formatter = formatP.CreateWriter outStream
        use writeState = new WriteState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qualifiedName = reflectionCache.GetQualifiedName pickler.Type
        formatter.BeginWriteRoot qualifiedName
        pickler.UntypedWrite writeState "value" value
        formatter.EndWriteRoot ()

    let readRootObjectUntyped (formatP : IPickleFormatProvider) resolver reflectionCache 
                        streamingContext inStream (pickler : Pickler) =

        let formatter = formatP.CreateReader inStream
        use readState = new ReadState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qualifiedName = reflectionCache.GetQualifiedName pickler.Type
        formatter.BeginReadRoot qualifiedName
        let value = pickler.UntypedRead readState "value"
        formatter.EndReadRoot ()
        value


    //
    //  top-level sequence serialization
    //

    let private sequenceStateResetThreshold = 1000 // resets write/read state at the given interval

    /// serializes a sequence of objects to stream

    let writeTopLevelSequence (formatP : IPickleFormatProvider) resolver reflectionCache streamingContext
                                outStream (pickler : Pickler<'T>) (values : seq<'T>) : int =

        // write state initialization
        let formatter = formatP.CreateWriter outStream
        use state = new WriteState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qn = reflectionCache.GetQualifiedName pickler.Type

        state.Formatter.BeginWriteRoot <| sprintf "%s sequence" qn
//        state.Formatter.BeginWriteUnBoundedSequence "values"
//        state.Formatter.BeginWriteObject pickler.TypeInfo pickler.PicklerInfo tag ObjectFlags.IsSequenceHeader

        let isObject = pickler.TypeInfo > TypeKind.Value

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

//        state.Formatter.EndWriteObject()
        state.Formatter.EndWriteRoot()
        length



    let readTopLevelSequence (formatP : IPickleFormatProvider) resolver reflectionCache streamingContext
                                inStream (pickler : Pickler<'T>) (length : int) : IEnumerator<'T> =
        
        // read state initialization
        let formatter = formatP.CreateReader inStream
        let state = new ReadState(formatter, resolver, reflectionCache, ?streamingContext = streamingContext)
        let qn = reflectionCache.GetQualifiedName pickler.Type

        // read stream header
        formatter.BeginReadRoot <| sprintf "%s sequence" qn

        let isObject = pickler.TypeInfo > TypeKind.Value

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



    let writeTopLevelSequenceUntyped formatP resolver reflectionCache streamingContext
                                outStream (pickler : Pickler) (values : IEnumerable) : int =
        let unpacker =
            {
                new IPicklerUnpacker<int> with
                    member __.Apply (p : Pickler<'T>) =
                        writeTopLevelSequence formatP resolver reflectionCache streamingContext 
                            outStream p (values :?> IEnumerable<'T>)
            }

        pickler.Unpack unpacker

    let readTopLevelSequenceUntyped formatP resolver reflectionCache streamingContext
                                inStream (pickler : Pickler) (length : int) : IEnumerator =
        let unpacker =
            {
                new IPicklerUnpacker<IEnumerator> with
                    member __.Apply (p : Pickler<'T>) =
                        readTopLevelSequence formatP resolver reflectionCache streamingContext 
                                inStream p length :> IEnumerator
            }

        pickler.Unpack unpacker