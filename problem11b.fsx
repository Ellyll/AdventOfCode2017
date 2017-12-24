#!/usr/bin/fsharpi

open System.IO

type CubeCoord = { X : int ; Y : int ; Z : int }

type Direction =
    |North
    |NorthEast
    |SouthEast
    |South
    |SouthWest
    |NorthWest

let parseDirection str =
    match str with
    | "n" -> North
    | "ne" -> NorthEast
    | "se" -> SouthEast
    | "s" -> South
    | "sw" -> SouthWest
    | "nw" -> NorthWest
    | dir -> failwithf "Invalid direction: %s" dir

let travel direction { X = x ; Y = y ; Z = z } =
    match direction with
    | North     -> { X = x     ; Y = y + 1 ; Z = z - 1 }
    | NorthEast -> { X = x + 1 ; Y = y     ; Z = z - 1 }
    | SouthEast -> { X = x + 1 ; Y = y - 1 ; Z = z     }
    | South     -> { X = x     ; Y = y - 1 ; Z = z + 1 }
    | SouthWest -> { X = x - 1 ; Y = y     ; Z = z + 1 }
    | NorthWest -> { X = x - 1 ; Y = y + 1 ; Z = z     }

let getDistance a b =
    (abs(a.X - b.X) + abs(a.Y - b.Y) + abs(a.Z - b.Z)) / 2

if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s dataFile" fsi.CommandLineArgs.[0]
    exit 1


let fileName = fsi.CommandLineArgs.[1]

let input = File.ReadAllText(fileName).Trim()

let directions =
    input.Split ','
    |> Seq.map parseDirection

let start = { X=0 ; Y=0 ; Z=0 }

let visited =
    directions
    |> Seq.fold (fun prev direction -> (travel direction (List.head prev)::prev)) [start]

let maxDistance =
    visited
    |> List.maxBy (fun v -> getDistance start v)
    |> getDistance start

printfn "Max distance: %i" maxDistance