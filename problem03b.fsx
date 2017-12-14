#!/usr/bin/fsharpi

open System


let (|ParsedInt|UnparsableInt|) input =
    match input with
    | _ when fst (Int32.TryParse input) -> ParsedInt (int input)
    | _ -> UnparsableInt

type Position = { Row : int ; Column : int }
type Square = { Index : int ; Position : Position ; Value : int }


let getContainingSquare n =
    let sq = floor (sqrt n)
    let oddsq = if sq % 2.0 = 0.0 then sq - 1.0 else sq
    if oddsq*oddsq = n then (int oddsq, int (oddsq*oddsq)) else (int (oddsq+2.0), int (Math.Pow(oddsq+2.0, 2.0)))


let squareToString (square : Square) : string =
    sprintf "(%i, (%i, %i), %A)" square.Index square.Position.Row square.Position.Column square.Value

let sub p1 p2 =
    { Row = p1.Row - p2.Row ; Column = p1.Column - p2.Column }

let isAdjacent p1 p2 =
    if p1 = p2 then
        false
    else
        let p = sub p1 p2        
        (abs p.Row) <= 1 && (abs p.Column) <= 1


let getPositionOfIndex n sideLength =
    let bottomRight = sideLength * sideLength
    let bottomLeft = bottomRight - sideLength + 1
    let topLeft = bottomLeft - sideLength + 1
    let topRight = topLeft - sideLength + 1

    let row, column =
        if n <= topRight then (topRight - n, sideLength - 1) // right side
        elif n <= topLeft then (0, topLeft - n)              // top side
        elif n <= bottomLeft then (n - topLeft, 0)           // left side
        else (sideLength - 1, n - bottomLeft)                // bottom side
    let offset = (sideLength - 1) / 2 // so that origin is at 0,0
    { Row = row  - offset ; Column = column - offset }

let getSquares maxLimit =
    let rec loop currentIndex maxLimit prevSquares =
        if (List.length prevSquares > 1) && (List.head prevSquares).Value > maxLimit then
            prevSquares
        else
            let square =
                match currentIndex with
                | 1 -> { Index = 1 ; Position = { Row = 0 ; Column = 0 } ; Value = 1 }
                | _ ->
                    let sideLength, _ = getContainingSquare (float currentIndex)
                    let p = getPositionOfIndex currentIndex sideLength
                    let adjacents =
                        prevSquares
                        |> List.filter (fun s -> s.Index < currentIndex && (isAdjacent s.Position p))
                    //printfn "adjacents for %i (%i, %i): %s" index p.Row p.Column (squaresToString adjacents)
                    let value =
                        adjacents
                        |> List.sumBy(fun s -> s.Value)
                    { Index = currentIndex ; Position = p ; Value = value}                    
            loop (currentIndex+1) maxLimit (square::prevSquares)
    loop 1 maxLimit []



if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s integer" fsi.CommandLineArgs.[0]
    exit 1

let n =
    match fsi.CommandLineArgs.[1] with
    | ParsedInt i -> i
    | UnparsableInt ->
        eprintfn "Input %s was not an integer" fsi.CommandLineArgs.[1]
        exit 1

printfn "Calculating..."


let squares = getSquares n
let square = squares |> List.head
let value = square.Value

printfn "Square with index of N: %s" (squareToString square)
printfn "Value of N: %A" value
