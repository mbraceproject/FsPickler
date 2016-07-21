namespace MBrace.FsPickler.Tests

open System
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

open FSharp.Charting

module SerializationSize =

    let private serializers = 
        [
            FsPickler.initBinary()
            FsPickler.initJson()
            new BinaryFormatterSerializer() :> Serializer
            new NetDataContractSerializer() :> Serializer
            new JsonDotNetSerializer() :> Serializer
            new ProtoBufSerializer() :> Serializer
            new WireSerializer() :> Serializer
        ]

    let reflectEquals (x : 'T) (y : 'T) =
        if obj.ReferenceEquals(x, null) then obj.ReferenceEquals(y, null)
        else
            x.GetType() = y.GetType() && x.ToString() = y.ToString()

    let tryGetSize (s : Serializer) (t : 'T) =
        try 
            let p = Serializer.write s t
            let r = Serializer.read s p : 'T
            if reflectEquals t r then Some p.Length
            else
                None
        with _ -> None

    let report (id : string) (t : 'T) =
        for s in serializers do
            let sizeFmt =
                match tryGetSize s t with
                | None -> "failed"
                | Some s when s < 1024 -> sprintf "%d bytes" s
                | Some s when s < 1024 * 1024 -> sprintf "%.2f KiB" (float s / 1024.)
                | Some s -> sprintf "%.2f MiB" (float s / (1024. * 1024.))

            printfn "%s (%s): %s" id s.Name sizeFmt

    let private dashGrid = ChartTypes.Grid(LineColor = Color.Gainsboro, LineDashStyle = ChartDashStyle.Dash)

    let plot (id : string) (t : 'T) =
        let results = 
            serializers 
            |> List.choose (fun s -> tryGetSize s t |> Option.map (fun r -> s.Name, float r))
            |> List.sortBy fst

        let mean = 
            results
            |> Seq.sumBy snd
            |> fun s -> s / float results.Length

        let title, data =
            if mean < 1024. then "bytes", results
            elif mean < 1024. * 1024. then "KiB", results |> List.map (fun (n,d) -> n, d / 1024.)
            else "MiB", results |> List.map (fun (n,d) -> n, d / (1024. * 1024.))

        Chart.Bar(data, Name = id, YTitle = title)
        |> Chart.WithTitle(Text = id, InsideArea = false)
        |> Chart.WithYAxis(MajorGrid = dashGrid) 
        |> Chart.WithXAxis(MajorGrid = dashGrid)
        |> fun ch -> ch.ShowChart()