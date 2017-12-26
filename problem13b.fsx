#!/usr/bin/fsharpi

type Layer = { Depth : int ; Range : int }

let sprintLayer layer =
    sprintf "(%i, %i)" layer.Depth layer.Range

let parseLayer (str : string) =
    //88: 20
    let parts = str.Replace(" ", "").Split([| ':' |])
    if (Array.length parts) <> 2 then
        failwithf "Invalid layer string: %s" str
    let layerId = int parts.[0]
    let range = int parts.[1]
    { Depth = layerId ; Range = range ; }

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

let getDetections (delay : int) (layers : Layer list) : Layer list =
    layers
    |> List.filter (fun l -> (getScannerPosition (l.Depth + delay) l.Range) = 0)

let rec findDelayWithoutGettingCaught delay layers =
    let detections = getDetections delay layers
    match detections with
    | [] -> delay
    | _ -> findDelayWithoutGettingCaught (delay + 1) layers


let fileName = fsi.CommandLineArgs.[1]

let lines = System.IO.File.ReadAllLines(fileName)

let layers = parseLayers lines

printfn "Calculating..."
let delayNeeded = findDelayWithoutGettingCaught 0 layers

printfn "Delay needed: %i" delayNeeded
