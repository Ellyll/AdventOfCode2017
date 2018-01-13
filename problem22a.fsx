#!/usr/bin/fsharpi

type CardinalDirection =
    | North
    | South
    | East
    | West  

type RelativeDirection =
    | Left
    | Right

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
        let infected =
            match symbol with
            | '#' -> true
            | _ -> false
        ((x,y), infected)
        )
    |> Seq.filter (fun (_, infected) -> infected)
    |> Seq.map (fun (location, _) -> location)
    |> Set.ofSeq

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

let makeNetwork initialCarrier initialGrid =
    Seq.unfold (fun (carrier, grid) ->
            let (newFacing, newInfections, newGrid) =
                if grid |> Set.contains carrier.Location then
                    // infected
                    ((turn Right carrier.Facing), carrier.InfectionsCaused, grid |> Set.remove carrier.Location)
                else
                    // Clean
                    ((turn Left carrier.Facing), carrier.InfectionsCaused + 1, grid |> Set.add carrier.Location)
            let newLocation = move newFacing carrier.Location
            let newState = ({ Location = newLocation ; Facing = newFacing ; InfectionsCaused = newInfections }, newGrid)
            Some (newState, newState)
        ) (initialCarrier, initialGrid)

let displayGrid currentLocation grid =
    let minX = grid |> Set.map (fun (x,_) -> x) |> Set.minElement
    let maxX = grid |> Set.map (fun (x,_) -> x) |> Set.maxElement
    let minY = grid |> Set.map (fun (_,y) -> y) |> Set.minElement
    let maxY = grid |> Set.map (fun (_,y) -> y) |> Set.maxElement
    let size =
        [ minX ; minY ; maxX ; maxY ]
        |> List.map (abs)
        |> List.max
    [ -size..size ]
    |> List.fold (fun lines y ->
        ([ -size..size ]
        |> List.fold (fun str x ->
            let s = if (Set.contains ((x,y)) grid) then "#" else "."
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


if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s filename" fsi.CommandLineArgs.[0]
    exit 1

printfn "Running..."

let filename = fsi.CommandLineArgs.[1]
let lines = System.IO.File.ReadAllLines(filename)
let grid = parseGrid lines

let network = makeNetwork ({ Location = (0,0) ; Facing = North ; InfectionsCaused = 0 }) grid

let numberOfBursts = 10000

let finalCarrier, _ =
    network
    |> Seq.skip (numberOfBursts - 1)
    |> Seq.head

printfn "Number of infections caused: %i" finalCarrier.InfectionsCaused