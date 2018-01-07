#!/usr/bin/fsharpi

type DiagramSymbol =
    | Space
    | Cross
    | VerticalLine
    | HorizontalLine
    | Letter of char

type Direction =
    | Up
    | Down
    | Left
    | Right

type PacketState = { Position : int*int ; Direction : Direction }


let buildDiagram lines =
    let noOfLines = Array.length lines
    let noOfColumns = lines |> Seq.map (String.length) |> Seq.max
    Array2D.init noOfLines noOfColumns (fun r c ->
        let line = lines.[r]
        if (String.length line) < noOfColumns then
            Space
        else
            match line.[c] with
            | ' ' -> Space
            | '+' -> Cross
            | '|' -> VerticalLine
            | '-' -> HorizontalLine
            | l when System.Char.IsLetter(l) -> Letter l
            | invalid ->
                failwithf "Unexpected character %s when parsing diagram" (invalid.ToString())
        )

let contains x xs =
    Seq.contains x xs

let findStart diagram =
    let r = 0
    seq { for c in 0..(Array2D.length2 diagram) -> ((r,c), diagram.[r, c]) }
    |> Seq.filter (fun (_, symbol) -> symbol = VerticalLine)
    |> Seq.head
    |> fst

let isInBounds diagram (r,c) =
    (r >= 0 && r < Array2D.length1 diagram) && (c >= 0 && c < Array2D.length2 diagram)

let move (r,c) direction =
    match direction with
    | Up -> (r-1, c)
    | Down -> (r+1, c)
    | Left -> (r, c-1)
    | Right -> (r, c+1)

let isOpposite originalDirection newDirection =
    match newDirection with
    | Left when originalDirection = Right -> true
    | Right when originalDirection = Left -> true
    | Up when originalDirection = Down -> true
    | Down when originalDirection = Up -> true
    | _ -> false

let tryMove packetState (diagram : DiagramSymbol[,]) =
    let (r,c) = packetState.Position
    let direction = packetState.Direction
    let currentSymbol = diagram.[r,c]

    match currentSymbol with
    | Letter _
    | VerticalLine
    | HorizontalLine ->
        let (newR, newC) = move (r,c) direction
        if isInBounds diagram (newR,newC) then
            let nextSymbol = diagram.[newR, newC]
            match nextSymbol with
            | VerticalLine when (match currentSymbol with | VerticalLine | Cross | Letter _ -> true | _ -> false) ->
                Some ({ packetState with Position = (newR, newC) })
            | HorizontalLine when (match currentSymbol with | HorizontalLine | Cross | Letter _ -> true | _ -> false) ->
                Some ({ packetState with Position = (newR, newC) })
            | Cross
            | Letter _ -> Some ({ packetState with Position = (newR, newC) })
            | HorizontalLine when currentSymbol = VerticalLine ->
                let (newR2, newC2) = move (newR,newC) direction
                if isInBounds diagram (newR2,newC2) then
                    Some ({ packetState with Position = (newR2, newC2) })
                else
                    None
            | VerticalLine when currentSymbol = HorizontalLine ->
                let (newR2, newC2) = move (newR,newC) direction
                if isInBounds diagram (newR2,newC2) then
                    Some ({ packetState with Position = (newR2, newC2) })
                else
                    None
            | _ -> None
        else
            None
    | Cross ->
        [ Up ; Down ; Left ; Right ]
        |> List.filter (isOpposite direction >> not)
        |> List.map (fun newDirection -> ((move (r,c) newDirection), newDirection))
        |> List.filter (fun ((newR, newC), _) -> isInBounds diagram (newR, newC))
        |> List.map (fun ((newR, newC), newDirection) -> ((newR, newC), newDirection, diagram.[newR, newC]))
        |> List.filter (fun (_, newDirection, newSymbol) ->
                match newSymbol with
                | VerticalLine when [ Up ; Down ] |> contains newDirection -> true
                | HorizontalLine when [ Left ; Right ] |> contains newDirection -> true
                | Letter _
                | Cross -> true
                | _ -> false
            )
        |> List.tryHead
        |> Option.map (fun ((newR, newC), newDirection, _) -> { packetState with Position = (newR,newC) ; Direction = newDirection })
    | _ ->
        failwithf "Invalid current symbol: %A at position: (%i,%i)" currentSymbol r c


let rec travel (diagram : DiagramSymbol[,]) (packetState, letters) =
    let r, c = packetState.Position
    let currentSymbol = diagram.[r, c]
    let newLetters =
        match currentSymbol with
        | Letter x -> x::letters
        | _ -> letters
    match tryMove packetState diagram with
    | Some (newPacketState) -> travel diagram (newPacketState, newLetters)
    | None -> (packetState, newLetters)



if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s filename" fsi.CommandLineArgs.[0]
    exit 1

let filename = fsi.CommandLineArgs.[1]
let lines = System.IO.File.ReadAllLines(filename)

printfn "Building diagram..."
let diagram = buildDiagram lines
let startPosition = findStart diagram

printfn "Start is at %A, travelling..." startPosition
let initialState = { Position = startPosition ; Direction = Down }

let finalState, letters = travel diagram (initialState, [])

printfn "Final state: %A" finalState
printfn "Answer: %s" (letters |> List.rev |> List.toArray |> System.String)

printfn "Finished"
