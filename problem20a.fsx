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

let getManhattanDistance (p : Vector3) (q : Vector3) =
    (abs p.X-q.X) + (abs p.Y-q.Y) + (abs p.Z-q.Z)


if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s filename" fsi.CommandLineArgs.[0]
    exit 1

printfn "Running..."

let filename = fsi.CommandLineArgs.[1]
let lines = System.IO.File.ReadAllLines(filename)

let points =
    lines
    |> Array.mapi (parsePoint)

let origin = Vector3(0.0f, 0.0f, 0.0f)

Seq.unfold (fun state ->
    let nextPoints = state |> Array.map (applyAcceleration)
    Some (state, nextPoints)
    ) points
|> Seq.skip 10000
|> Seq.head
|> Seq.map (fun point -> (point.Id, getManhattanDistance point.P origin))
|> Seq.minBy (fun (_, distance) -> distance)
|> printfn "Answer: %A"
