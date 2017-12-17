#!/usr/bin/fsharpi

open System
open System.IO

type Token =
    | StartGroup
    | EndGroup
    | StartGarbage
    | EndGarbage
    | CancelNextChar
    | CancelledChar
    | Other

let parseToken isCancelled isGarbage token =
    if isCancelled then
        CancelledChar
    elif isGarbage then
        match token with
        | '>' -> EndGarbage
        | '!' -> CancelNextChar
        | _ -> Other
    else
        match token with
        | '{' -> StartGroup
        | '}' -> EndGroup
        | '<' -> StartGarbage
        | _ -> Other

let isCompleted (buffer : Token list) : bool =
    match buffer with
    | EndGroup::_ ->
        let tokenMap =
            buffer
            |> List.groupBy (id)
            |> List.map (fun (k, v) -> k, List.length v)
            |> Map.ofList
        if (Map.containsKey StartGroup tokenMap) && (Map.containsKey StartGroup tokenMap) then
            (Map.find StartGroup tokenMap) = (Map.find EndGroup tokenMap)
        else
            false
    | _ -> false

let calculateScore buffer =
    let score, _ =
        buffer
        |> Seq.rev
        |> Seq.filter (fun t -> t <> CancelNextChar && t <> CancelledChar)
        |> Seq.fold (fun (score, inGarbage) token ->
            match token with
            | StartGarbage -> (score, true)
            | EndGarbage -> (score, false)
            | _ -> ((if inGarbage then score + 1 else score), inGarbage)
            ) (0,false)
    score

type State = { Score : int ; Buffer : Token list ; IsGarbage : bool ; IsCancelled : bool }

let getGroupsScore (chars : Char seq) : int =
    let initial = { Score = 0 ; Buffer = [] ; IsGarbage = false ; IsCancelled = false }
    let final =
        chars
        |> Seq.fold (fun state char ->
            let token = parseToken state.IsCancelled state.IsGarbage char
            let score, buffer =
                match token with
                | CancelNextChar -> (state.Score, state.Buffer)
                | Other -> (state.Score, if state.IsGarbage then token::state.Buffer else state.Buffer)
                | EndGroup ->
                    let newBuffer = token::state.Buffer            
                    if (isCompleted newBuffer) then 
                        let newScore = state.Score + (calculateScore newBuffer)
                        (newScore, [])
                    else
                        (state.Score, newBuffer)
                | t -> (state.Score, t::state.Buffer)
            let isCancelled =
                match token with
                | CancelNextChar -> true
                | _ -> false
            let isGarbage =
                match (List.filter (fun t -> t <> Other && t <> CancelledChar) buffer) with
                | StartGarbage::_ -> true
                | _ -> false
            { Score = score ; Buffer = buffer ; IsGarbage = isGarbage ; IsCancelled = isCancelled }
            ) initial
    final.Score

if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s dataFile" fsi.CommandLineArgs.[0]
    exit 1

let fileName = fsi.CommandLineArgs.[1]

let input = File.ReadAllText(fileName).Trim()

let score = getGroupsScore input

printfn "Score: %i" score