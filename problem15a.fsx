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

let generatorA = makeGenerator 16807 512
let generatorB = makeGenerator 48271 191

let values =
    Seq.zip generatorA generatorB

let numberOfMatches =
    values
    |> Seq.take 40_000_000
    |> Seq.fold (fun m (a,b) -> if (is16bitMatch a b) then m+1 else m) 0

printfn "Number of 16-bit matches: %i" numberOfMatches