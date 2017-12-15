#!/usr/bin/fsharpi

open System
open System.IO

if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s dataFile" fsi.CommandLineArgs.[0]
    exit 1

let fileName = fsi.CommandLineArgs.[1]
let lines = File.ReadAllLines(fileName)

let offsets =
    lines
    |> Array.map (fun line -> Int32.Parse(line))

let rec getStepsToExit steps index =
    if index >= (Array.length offsets) then
        steps
    else
        let jmp = offsets.[index]
        offsets.[index] <- if jmp >=3 then jmp - 1 else jmp + 1
        getStepsToExit (steps+1) (index + jmp)

let numberOfStepsToExit = getStepsToExit 0 0

printfn "Number of steps to exit: %i" numberOfStepsToExit