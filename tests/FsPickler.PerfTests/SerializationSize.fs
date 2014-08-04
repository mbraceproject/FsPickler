namespace Nessos.FsPickler.Tests

    open System
    open System.Drawing
    open System.Windows.Forms.DataVisualization.Charting

    open FSharp.Charting

    module SerializationSize =

        let private serializers = 
            [
                FsPickler.initBinary()
                FsPickler.initJson()
                new BinaryFormatterSerializer() :> ISerializer
                new NetDataContractSerializer() :> ISerializer
                new JsonDotNetSerializer() :> ISerializer
                new ProtoBufSerializer() :> ISerializer
                new ServiceStackTypeSerializer() :> ISerializer
            ]

        let tryGetSize (s : ISerializer) (t : 'T) =
            try 
                let p = Serializer.write s t
                let r = Serializer.read s p : 'T
                Some p.Length
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
                |> List.choose (fun s -> tryGetSize s t |> Option.map (fun r -> s.Name, r))

            Chart.Bar(results, Name = id, ?Title = None, YTitle = "bytes")
            |> Chart.WithYAxis(MajorGrid = dashGrid) 
            |> Chart.WithXAxis(MajorGrid = dashGrid)
            |> fun ch -> ch.ShowChart()