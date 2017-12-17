#!/usr/bin/fsharpi

open System
open System.IO

type Token =
    | StartGroup
    | EndGroup
    | StartGarbage
    | EndGarbage
    | CancelNextChar
    | Other

type State = { Score : int ; Buffer : Token list ; IsGarbage : bool ; IsCancelled : bool }

let parseToken isCancelled isGarbage token =
    if isCancelled then
        Other
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
        |> Seq.fold (fun (score, depth) token ->
            match token with
            | StartGroup -> (score, depth + 1)
            | EndGroup -> (score + depth, depth - 1)
            | _ -> (score, depth)
            ) (0,0)
    score

let getGroupsScore (chars : Char seq) : int =
    let initial = { Score = 0 ; Buffer = [] ; IsGarbage = false ; IsCancelled = false }
    let final =
        chars
        |> Seq.fold (fun state char ->
            let token = parseToken state.IsCancelled state.IsGarbage char
            let score, buffer =
                match token with
                | CancelNextChar -> (state.Score, state.Buffer)
                | Other -> (state.Score, state.Buffer)
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
                match buffer with
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