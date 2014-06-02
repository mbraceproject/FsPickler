namespace Nessos.FsPickler

    open System

    open Nessos.FsPickler.Hashing

    [<AutoSerializable(false)>]
    type BinaryPickler (formatP : IBinaryPickleFormatProvider, ?tyConv) =
        inherit FsPickler(formatP, ?tyConv = tyConv)

        new (?tyConv) = new BinaryPickler(new BinaryPickleFormatProvider(), ?tyConv = tyConv)

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="pickler">Pickler to use.</param>
        /// <param name="value">Value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.Pickle (pickler : Pickler<'T>, value : 'T, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : byte [] =
            pickleBinary (fun m v -> f.Serialize(pickler, m, v, ?streamingContext = streamingContext, ?encoding = encoding)) value

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="value">Value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.Pickle (value : 'T, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : byte [] =
            pickleBinary (fun m v -> f.Serialize(m, v, ?streamingContext = streamingContext, ?encoding = encoding)) value

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="pickler">pickler to use.</param>
        /// <param name="value">value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.Pickle (pickler : Pickler, value : obj, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : byte [] =
            pickleBinary (fun m v -> f.Serialize(pickler, m, v, ?streamingContext = streamingContext, ?encoding = encoding)) value

        /// <summary>
        ///     Pickles given value to byte array.
        /// </summary>
        /// <param name="valueType">type of pickled value.</param>
        /// <param name="value">value to pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.Pickle (valueType : Type, value : obj, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : byte [] =
            pickleBinary (fun m v -> f.Serialize(valueType, m, v, ?streamingContext = streamingContext, ?encoding = encoding)) value

        /// <summary>
        ///     Unpickles value using given pickler.
        /// </summary>
        /// <param name="pickler">Pickler to use.</param>
        /// <param name="data">Pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        member f.UnPickle (pickler : Pickler<'T>, data : byte [], [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : 'T =
            unpickleBinary (fun m -> f.Deserialize(pickler, m, ?streamingContext = streamingContext, ?encoding = encoding)) data

        /// <summary>
        ///     Unpickle value to given type.
        /// </summary>
        /// <param name="pickle">pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        member f.UnPickle<'T> (pickle : byte [], [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : 'T =
            unpickleBinary (fun m -> f.Deserialize<'T>(m, ?streamingContext = streamingContext, ?encoding = encoding)) pickle

        /// <summary>
        ///     Unpickle value to given type.
        /// </summary>
        /// <param name="valueType">type of pickled value.</param>
        /// <param name="pickle">pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        member f.UnPickle (valueType : Type, pickle : byte [], [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : obj =
            unpickleBinary (fun m -> f.Deserialize(valueType, m, ?streamingContext = streamingContext, ?encoding = encoding)) pickle

        /// <summary>
        ///     Unpickle value to given type.
        /// </summary>
        /// <param name="pickler">pickler to use.</param>
        /// <param name="pickle">pickle.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        member f.UnPickle (pickler : Pickler, pickle : byte [], [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding) : obj =
            unpickleBinary (fun m -> f.Deserialize(pickler, m, ?streamingContext = streamingContext, ?encoding = encoding)) pickle

        /// <summary>Compute size and hashcode for given input.</summary>
        /// <param name="value">input value.</param>
        /// <param name="pickler">use specific pickler for hashcode generation.</param>
        /// <param name="hashFactory">the hashing algorithm to be used. MurMur3 by default</param>
        member f.ComputeHash<'T>(value : 'T, [<O;D(null)>] ?pickler : Pickler<'T>, [<O;D(null)>] ?hashFactory : IHashStreamFactory) =
            let hashStream = 
                match hashFactory with 
                | Some h -> h.Create()
                | None -> new MurMur3Stream() :> HashStream

            match pickler with
            | None -> f.Serialize(hashStream, value)
            | Some p -> f.Serialize(p, hashStream, value)

            {
                Algorithm = hashStream.HashAlgorithm
                Length = hashStream.Length
                Hash = hashStream.ComputeHash()
            }

        /// <summary>Compute size in bytes for given input.</summary>
        /// <param name="pickler">use specific pickler for length computation.</param>
        /// <param name="value">input value.</param>
        member f.ComputeSize<'T>(value : 'T, [<O;D(null)>]?pickler : Pickler<'T>) =

            let lengthCounter = new LengthCounter()
            match pickler with
            | None -> f.Serialize(lengthCounter, value)
            | Some p -> f.Serialize(p, lengthCounter, value)
            lengthCounter.Length