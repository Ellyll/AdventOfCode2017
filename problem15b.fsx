#!/usr/bin/fsharpi

let is16bitMatch a b =
    (a <<< 16) = (b <<< 16)

let makeGenerator (factor : int) (start : int) =
    let divisor = 2147483647L
    Seq.unfold (fun state ->
        let value = int ((int64 state)*(int64 factor) % divisor)
        let nextState = value
        Some (value, nextState)
     ) start

// let startA = 65
// let startB = 8921
let startA = 512
let startB = 191

let generatorA = (makeGenerator 16807 startA) |> Seq.filter (fun n -> (n % 4) = 0)
let generatorB = (makeGenerator 48271 startB) |> Seq.filter (fun n -> (n % 8) = 0)

let values =
    Seq.zip generatorA generatorB

let numberOfMatches =
    values
    |> Seq.take 5_000_000
    |> Seq.fold (fun m (a,b) -> if (is16bitMatch a b) then m+1 else m) 0

printfn "Number of 16-bit matches: %i" numberOfMatches