#!/usr/bin/fsharpi

type ShortSpinLock = { ValueAfter0 : int ; Position : int ; Value : int }

let makeGenerator stepsPerInsert =
    Seq.unfold (fun state ->
        let newItem = state.Value + 1
        let newPosition = if (state.Value = 0) then 0 else (state.Position + stepsPerInsert) % (state.Value+1)
        let newValueAfter0 = if (newPosition = 0) then newItem else state.ValueAfter0
        let value = { ValueAfter0 = newValueAfter0 ; Position = newPosition + 1 ; Value = newItem }
        let nextState = value
        Some (value, nextState)
        ) { ValueAfter0 = -1 ; Position = 0 ; Value = 0 }


if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s stepsPerInsert" fsi.CommandLineArgs.[0]
    exit 1

let stepsPerInsert = int fsi.CommandLineArgs.[1]

let generator = makeGenerator stepsPerInsert

printfn "Runnning..."

let answer =
    (generator
    |> Seq.skip (50_000_000-1)
    |> Seq.take 1
    |> Seq.last)

printfn "Answer: %i" (answer.ValueAfter0)
