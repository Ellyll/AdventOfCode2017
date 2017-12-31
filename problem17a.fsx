#!/usr/bin/fsharpi

type SpinLock = { Buffer : int list ; Position : int }

let makeGenerator stepsPerInsert =
    Seq.unfold (fun state ->
        let newItem = state.Buffer.[state.Position] + 1
        let newPosition = (state.Position + stepsPerInsert) % (List.length state.Buffer)
        let a, b =
            state.Buffer
            |> List.splitAt (newPosition + 1)
        let newBuffer = a @ (newItem::b)        
        let value = { Buffer = newBuffer ; Position = newPosition + 1 }
        let nextState = value
        Some (value, nextState)
        ) { Buffer = [ 0 ] ; Position = 0 }

let formatSpinLock spinlock =
    spinlock.Buffer
    |> Seq.mapi (fun i v ->
        if i = spinlock.Position then
            sprintf " (%i) " v
        else
            sprintf "  %i  " v
        )
    |> Seq.fold (fun str s -> (str + s)) ""


if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s stepsPerInsert" fsi.CommandLineArgs.[0]
    exit 1

let stepsPerInsert = int fsi.CommandLineArgs.[1]

let generator = makeGenerator stepsPerInsert

let answer =
    (generator
    |> Seq.skip 2016
    |> Seq.take 1
    |> Seq.last)

printfn "Answer: %i" (answer.Buffer |> List.item (answer.Position+1))

