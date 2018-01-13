#!/usr/bin/fsharpi

type CardinalDirection =
    | North
    | South
    | East
    | West  

type RelativeDirection =
    | Left
    | Right

type NodeState =
    | Clean
    | Weakened
    | Infected
    | Flagged

type Carrier = { Location : int*int ; Facing : CardinalDirection ; InfectionsCaused : int }

let parseGrid lines =
    let height = Array.length lines
    let width = String.length lines.[0]
    let cx = (width/2)
    let cy = (height/2)
    seq {
        for r in 0..height-1 do
            for c in 0..width-1 do
                yield ((r,c), lines.[r].[c])
    }
    |> Seq.map (fun ((r,c), symbol) ->
        let x = c-cx
        let y = r-cy
        let nodeState =
            match symbol with
            | '#' -> Infected
            | _ -> Clean
        ((x,y), nodeState)
        )
    |> Seq.filter (fun (_, nodeState) -> nodeState <> Clean)
    |> Map.ofSeq

let turn relativeDirection cardinalDirection =
    match relativeDirection with
    | Left ->
        match cardinalDirection with
        | North -> West
        | East -> North
        | South -> East
        | West -> South
    | Right ->
        match cardinalDirection with
        | North -> East
        | East -> South
        | South -> West
        | West -> North

let move cardinalDirection (x, y) =
    match cardinalDirection with
    | North -> (x, y-1)
    | East -> (x+1, y)
    | South -> (x, y+1)
    | West -> (x-1, y)

let displayGrid currentLocation grid =
    let minX = grid |> Map.toSeq |> Seq.map (fun ((x,_),_) -> x) |> Seq.min
    let maxX = grid |> Map.toSeq |> Seq.map (fun ((x,_),_) -> x) |> Seq.max
    let minY = grid |> Map.toSeq |> Seq.map (fun ((_,y),_) -> y) |> Seq.min
    let maxY = grid |> Map.toSeq |> Seq.map (fun ((_,y),_) -> y) |> Seq.max
    let size =
        [ minX ; minY ; maxX ; maxY ]
        |> List.map (abs)
        |> List.max
    [ -size..size ]
    |> List.fold (fun lines y ->
        ([ -size..size ]
        |> List.fold (fun str x ->
            let s =
                match (Map.tryFind (x,y) grid) with
                | Some Weakened -> "W"
                | Some Infected -> "#"
                | Some Flagged -> "F"
                | _ -> "."

            str +
                if (x,y) = currentLocation then
                    "[" + s + "]"
                else
                    " " + s + " "
            ) ""
        )::lines
        ) []
    |> List.rev
    |> List.iter (printfn "%s")

let makeNetwork initialCarrier initialGrid =
    Seq.unfold (fun (carrier, grid) ->
            let nodeState =
                match Map.tryFind carrier.Location grid with
                | Some (state) -> state
                | None -> Clean
            let (newFacing, newInfections, newGrid) =
                match nodeState with
                | Clean ->
                    ((turn Left carrier.Facing), carrier.InfectionsCaused, grid |> Map.add carrier.Location Weakened)
                | Weakened ->
                    (carrier.Facing, carrier.InfectionsCaused + 1, grid |> Map.remove carrier.Location |> Map.add carrier.Location Infected)
                | Infected ->
                    ((turn Right carrier.Facing), carrier.InfectionsCaused, grid |> Map.remove carrier.Location |> Map.add carrier.Location Flagged)
                | Flagged ->
                    (turn Left (turn Left carrier.Facing), carrier.InfectionsCaused, grid |> Map.remove carrier.Location)

            let newLocation = move newFacing carrier.Location
            let newState = ({ Location = newLocation ; Facing = newFacing ; InfectionsCaused = newInfections }, newGrid)
            Some (newState, newState)
        ) (initialCarrier, initialGrid)

if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s filename" fsi.CommandLineArgs.[0]
    exit 1

printfn "Running..."

let filename = fsi.CommandLineArgs.[1]
let lines = System.IO.File.ReadAllLines(filename)
let grid = parseGrid lines

let network = makeNetwork ({ Location = (0,0) ; Facing = North ; InfectionsCaused = 0 }) grid

let numberOfBursts = 10000000

let finalCarrier, _ =
    network
    |> Seq.skip (numberOfBursts - 1)
    |> Seq.head

printfn "Number of infections caused: %i" finalCarrier.InfectionsCaused