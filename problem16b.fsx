#!/usr/bin/fsharpi

open System.IO

type Move =
    | Spin of int
    | Exchange of int*int
    | Partner of char*char

type Parser<'a> = char list -> ('a * char list) option

let parseDigits : int Parser =
    let rec parseInner = function
    | (n, x::rest) when System.Char.IsDigit x ->
        let m = (n*10) + (int (x.ToString()))
        parseInner (m, rest)
    | (0, []) ->
        printfn "Attempt to parse zero characters as int"
        None
    | (n, xs) -> Some (n, xs)

    function
    | [] -> None
    | xs -> parseInner (0, xs)

let parseSpin : Move Parser =
    let rec parseInner = function
        | [] -> None
        | rest ->
            Option.map (fun (n, rs) -> (Spin n, rs)) (parseDigits rest)
    
    function
    | 's'::rest -> parseInner rest
    | _ -> None

let parseExchange : Move Parser =
    let rec parseInner = function
        | [] -> None
        | rest -> 
            match (parseDigits rest) with
            | None -> None
            | Some (a, rs) ->
                match rs with
                | '/'::ss ->
                    match (parseDigits ss) with
                    | Some (b, ts) ->
                        Some(Exchange (a,b), ts)
                    | None -> None
                | _ -> None
    
    function
    | 'x'::rest -> parseInner rest
    | _ -> None

let parsePartner : Move Parser =
    let rec parseInner = function
        | a::'/'::b::rest -> Some(Partner (a,b), rest)
        | _ -> None
    
    function
    | 'p'::rest -> parseInner rest
    | _ -> None

let rec parseMove : Move Parser =
    function
    | 's'::rest -> parseSpin ('s'::rest)
    | 'x'::rest -> parseExchange ('x'::rest)
    | 'p'::rest -> parsePartner ('p'::rest)
    | m ->
        printfn "Invalid move: %s" (m.ToString())
        None

let rec parseMoves : (Move list) Parser =
    let rec parseInner = function
        | (moves, []) -> Some (moves, [])
        | (moves, ','::rest) -> parseInner (moves, rest)
        | (moves, rest) ->
            match parseMove rest with
            | None -> None
            | Some (move, r) -> parseInner (move::moves,r)

    function
    | [] -> None
    | cs -> parseInner ([], cs) |> Option.map (fun (moves, rest) -> (List.rev moves, rest))


let spin len programs =
    let idx = (Array.length programs) - len
    let a, b = programs |> Array.splitAt idx
    Array.concat [ b ; a ]

let exchange a b (programs : char[]) =
    let progA = programs.[a]
    let progB = programs.[b]
    let newPrograms = Array.copy programs
    newPrograms.[a] <- progB
    newPrograms.[b] <- progA
    newPrograms

let partner a b programs =
    let idxA = programs |> Array.findIndex (fun prog -> prog = a)
    let idxB = programs |> Array.findIndex (fun prog -> prog = b)
    let newPrograms = Array.copy programs
    newPrograms.[idxA] <- b
    newPrograms.[idxB] <- a
    newPrograms

let executeMove move programs =
    match move with
    | Spin n -> spin n programs
    | Exchange (a,b) -> exchange a b programs
    | Partner (a,b) -> partner a b programs

let getMappings originalPrograms newPrograms =
    newPrograms
    |> Array.map (fun p -> Array.findIndex (fun p2 -> p2 = p) originalPrograms)

let runMoves moves programs = 
    moves |> List.fold (fun ps move -> executeMove move ps) programs

let runMappings (mappings : int[]) (programs : char[]) =
    Array.map (fun i -> programs.[i]) mappings


if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s dataFile" fsi.CommandLineArgs.[0]
    exit 1

let fileName = fsi.CommandLineArgs.[1]
let input = (File.ReadAllText(fileName).Trim())

let length = 16
let programs =
    [ 'a'..'z' ]
    |> List.take length
    |> Array.ofList

printfn "Parsing..."
let movesOpt = parseMoves (input |> List.ofSeq)

let makeGenerator moves programs =
        Seq.unfold (fun state ->
            let value = state
            let nextState = moves |> List.fold (fun ps move -> executeMove move ps) state
            Some (value, nextState)
        ) programs

printfn "Running..."
Option.iter (fun (moves, _) ->
    let repeatCount =
        ((makeGenerator moves programs)
        |> Seq.skip 1
        |> Seq.takeWhile (fun progs -> progs <> programs)
        |> Seq.length) + 1

    printfn "Repeat count: %i" repeatCount

    let iterations = 1_000_000_000

    let final =
        (makeGenerator moves programs)
        |> Seq.take ((iterations % repeatCount) + 1)
        |> Seq.last

    printfn "Answer: %s" (final |> System.String)
    ) movesOpt
