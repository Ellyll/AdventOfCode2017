#!/usr/bin/fsharpi

open System.Numerics

type Point = { Id : int ; P : Vector3 ; V : Vector3 ; A : Vector3 }

let parsePoint lineNumber (line : string) =
    let vectors =
        (line.Replace(", ", ";").Split ';')
        |> Array.map (fun str ->
            let s = (str.Trim())
            let len = String.length s
            let vectorValues =
                s.[3..len-2].Split ','
                |> Array.map (float32)
            Vector3(vectorValues.[0], vectorValues.[1], vectorValues.[2])
            )
    { Id = lineNumber ; P = vectors.[0] ; V = vectors.[1] ; A = vectors.[2] }

let applyAcceleration point =
    let newV = point.V + point.A
    let newP = point.P + newV
    { point with V = newV ; P = newP }


if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s filename" fsi.CommandLineArgs.[0]
    exit 1

printfn "Running..."

let filename = fsi.CommandLineArgs.[1]
let lines = System.IO.File.ReadAllLines(filename)

let points =
    lines
    |> Array.mapi (parsePoint)

Seq.unfold (fun state ->
    let movedPoints = state |> Array.map (applyAcceleration)
    let collisions =
        movedPoints
        |> Array.groupBy (fun p -> p.P)
        |> Array.filter (fun (_, points) -> Array.length points > 1)
        |> Array.collect (fun (_, points) -> points)
        |> Array.map (fun point -> point.Id)
        |> Set.ofArray
    let nextPoints =
        movedPoints
        |> Array.filter (fun point -> not (Set.contains point.Id collisions))
    Some (state, nextPoints)
    ) points
|> Seq.skip 10000
|> Seq.head
|> Array.length
|> printfn "Answer: %A"
