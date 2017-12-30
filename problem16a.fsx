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
    let idx = (List.length programs) - len
    let a, b = programs |> List.splitAt idx
    b @ a

let exchange a b programs =
    let progA = programs |> List.item a
    let progB = programs |> List.item b
    programs
    |> List.mapi (fun idx prog ->
        if idx = a then
            progB
        elif idx = b then
            progA
        else
            prog)

let partner a b programs =
    programs
    |> List.map (fun prog ->
        if prog = a then
            b
        elif prog = b then
            a
        else prog)

let executeMove move programs =
    match move with
    | Spin n -> spin n programs
    | Exchange (a,b) -> exchange a b programs
    | Partner (a,b) -> partner a b programs


if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s dataFile" fsi.CommandLineArgs.[0]
    exit 1

let fileName = fsi.CommandLineArgs.[1]
let input = (File.ReadAllText(fileName).Trim())

let length = 16
let programs =
    [ 'a'..'z' ]
    |> List.take length


Option.iter (fun (moves, _) ->
    let final =
        moves
        |> List.fold (fun ps move ->
            let result = executeMove move ps
            result) programs

    printfn "Programs at start: %A" programs
    printfn "Programs at end:   %A" final
    printfn "Answer: %s" (final |> Array.ofList |> System.String)
    ) (parseMoves (input |> List.ofSeq))
