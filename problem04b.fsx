#!/usr/bin/fsharpi

open System

let phrases = IO.File.ReadAllLines("problem04a.data");

let sortLetters (word : string) : string =
    let chars = word.ToCharArray() |> Array.sort
    String.Join("", chars)


let isValid (phrase : string) =
    let words =
        phrase.Split([| ' ' |])
        |> Array.map sortLetters
    not (words
        |> Array.groupBy (id)
        |> Array.map (fun (w, ws) -> (w, Array.length ws))
        |> Array.exists (fun (_, count) -> count > 1))

let numberOfValidPhrases =
    phrases
    |> Array.filter (isValid)
    |> Array.length

printfn "Number of valid phrases = %i" numberOfValidPhrases
