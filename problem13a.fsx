#!/usr/bin/fsharpi

type Layer = { Depth : int ; Range : int ; ScannerPosition : int ; ScannerDirection : int }

let sprintLayer layer =
    sprintf "(%i, %i, %i, %i)" layer.Depth layer.Range layer.ScannerPosition layer.ScannerDirection

let parseLayer (str : string) =
    //88: 20
    let parts = str.Replace(" ", "").Split([| ':' |])
    if (Array.length parts) <> 2 then
        failwithf "Invalid layer string: %s" str
    let layerId = int parts.[0]
    let range = int parts.[1]
    { Depth = layerId ; Range = range ; ScannerPosition = 0 ; ScannerDirection = 1 }

let parseLayers lines =
    lines
    |> Seq.map (parseLayer)
    |> List.ofSeq

let advanceLayers layers =
    layers
    |> List.map (fun layer ->
        let newPosition = layer.ScannerPosition + layer.ScannerDirection
        let newDirection =
            if (newPosition = layer.Range - 1) || (newPosition = 0) then
                layer.ScannerDirection * -1
            else
                layer.ScannerDirection
        { layer with ScannerPosition = newPosition ; ScannerDirection = newDirection }
    )

let getDetected position layers =
    layers
    |> List.filter (fun layer -> layer.Depth = position && layer.ScannerPosition = 0)

let getDetections layers =
    let maxPosition =
        (layers |> List.maxBy (fun layer -> layer.Depth)).Depth
        
    let detections, _ =
        [ 0..maxPosition+1 ]
        |> List.fold (fun (detections, ls) position ->
            //printfn "position: %i\nls: %A\ndetections: %A\n" position (ls |> List.map (sprintLayer)) (detections |> List.map (sprintLayer))
            let newDetections = (getDetected position ls) @ detections
            let newLayers = advanceLayers ls
            (newDetections, newLayers)) ([], layers)
    detections


let fileName = fsi.CommandLineArgs.[1]

let lines = System.IO.File.ReadAllLines(fileName)

let layers = parseLayers lines
let detections = getDetections layers

let severities : int list =
    detections
    |> List.map (fun (layer) -> layer.Depth * layer.Range)

printfn "Total severity: %i" (severities |> List.sum)