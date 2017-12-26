#!/usr/bin/fsharpi

type Layer = { Depth : int ; Range : int }

let parseLayer (str : string) =
    //88: 20
    let parts = str.Replace(" ", "").Split([| ':' |])
    if (Array.length parts) <> 2 then
        failwithf "Invalid layer string: %s" str
    let layerId = int parts.[0]
    let range = int parts.[1]
    { Depth = layerId ; Range = range }

let parseLayers lines =
    lines
    |> Seq.map (parseLayer)
    |> List.ofSeq

let getScannerPosition time range =
    if time < 0 then
        invalidArg "time" (sprintf "time of %i is not valid - must be greater than or equal to 0" time)
    elif range <= 0 then
        invalidArg "range" (sprintf "range of %i is not valid - must be greater than 0" range)
    elif range = 1 then
        0
    else
        let i = time % ((2*range)-2)
        if i < range then i else (2*(range-1)) - i

let getDetected time position layers =
        layers
        |> List.filter (fun l -> l.Depth = position && (getScannerPosition time l.Range) = 0)

let getDetections (layers : Layer list) : Layer list =
    let maxPosition = (layers |> List.maxBy (fun layer -> layer.Depth)).Depth
    let detections =
        [ 0..(maxPosition+1) ]
        |> List.fold (fun detections position ->
            let time = position // move one position per tick
            let newDetections = (getDetected time position layers) @ detections
            newDetections
            ) []
    detections


let fileName = fsi.CommandLineArgs.[1]

let lines = System.IO.File.ReadAllLines(fileName)

let layers = parseLayers lines

printfn "Calculating..."
let detections = getDetections layers
let severities : int list =
    detections
    |> List.map (fun (layer) -> layer.Depth * layer.Range)

printfn "Total severity: %i" (severities |> List.sum)