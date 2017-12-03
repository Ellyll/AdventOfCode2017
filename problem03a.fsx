#!/usr/bin/fsharpi

open System


let (|ParsedInt|UnparsableInt|) input =
    match input with
    | _ when fst (Int32.TryParse input) -> ParsedInt (int input)
    | _ -> UnparsableInt


let getContainingSquare n =
    let sq = floor (sqrt n)
    let oddsq = if sq % 2.0 = 0.0 then sq - 1.0 else sq
    if oddsq*oddsq = n then (int oddsq, int (oddsq*oddsq)) else (int (oddsq+2.0), int (Math.Pow(oddsq+2.0, 2.0)))

let getPositionOfN n sideLength max =
    let bottomRight = max
    let bottomLeft = bottomRight - sideLength + 1
    let topLeft = bottomLeft - sideLength + 1
    let topRight = topLeft - sideLength + 1

    // row, column
    if n <= topRight then (topRight - n, sideLength - 1) // right side
    elif n <= topLeft then (0, topLeft - n)              // top side
    elif n <= bottomLeft then (n - topLeft, 0)           // left side
    else (sideLength - 1, n - bottomLeft)                // bottom side

let getPositionOf1 sideLength =
    let centre = (sideLength - 1) / 2
    (centre, centre)

let getManhattanDistance (p1 : int, p2 : int) (q1 : int, q2 : int) =
    Math.Abs(p1 - q1) + Math.Abs(p2 - q2)



if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s integer" fsi.CommandLineArgs.[0]
    exit 1

let n =
    match fsi.CommandLineArgs.[1] with
    | ParsedInt i -> i
    | UnparsableInt ->
        eprintfn "Input %s was not an integer" fsi.CommandLineArgs.[1]
        exit 1

let sideLength, max = getContainingSquare (float n)
let positionOfN = getPositionOfN n sideLength max
let positionOf1 = getPositionOf1 sideLength
let manhattanDistance = getManhattanDistance positionOfN positionOf1


printfn "Containing square of %d is %d, with side %d" n max sideLength
printfn "Positions: 1=%A %d=%A" positionOf1 n positionOfN
printfn "Manhattan distance: %d" manhattanDistance