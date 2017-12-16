#!/usr/bin/fsharpi

open System
open System.IO
open System.Text.RegularExpressions

type Register = string

type Command =
    | Increment of Register*int
    | Decrement of Register*int

type Condition =
    | GreaterThan of Register*int
    | GreaterThanOrEqual of Register*int
    | LessThan of Register*int
    | LessThanOrEqual of Register*int
    | Equal of Register*int
    | NotEqual of Register*int

type Instruction = Command*Condition

let parseCommand (str : string) : Command =
    let parts = str.Trim().Split(' ')
    let register = parts.[0]
    let cmd = parts.[1]
    let value = Int32.Parse(parts.[2])
    match cmd with
        | "inc" -> Increment (register, value)
        | "dec" -> Decrement (register, value)
        | _ -> failwith (sprintf "Invalid command: %s" cmd)

let parseCondition (str : string) : Condition =
    let parts = str.Trim().Split(' ')
    let register = parts.[0]
    let condition = parts.[1]
    let value = Int32.Parse(parts.[2])
    match condition with
        | ">"  -> GreaterThan (register, value)
        | ">=" -> GreaterThanOrEqual (register, value)
        | "<"  -> LessThan (register, value)
        | "<=" -> LessThanOrEqual (register, value)
        | "==" -> Equal (register, value)
        | "!=" -> NotEqual (register, value)
        | _ -> failwith (sprintf "Invalid condition: %s" condition)


let parse (line : string) : Instruction =
    let parts = line.Split([| " if " |], StringSplitOptions.RemoveEmptyEntries)
    if Array.length parts <> 2 then
        failwith (sprintf "Invalid instruction %s" line)
    let command = parseCommand parts.[0]
    let condition = parseCondition parts.[1]
    (command, condition)

let someOrDefault defaultValue a =
    match a with
        | Some a -> a
        | None -> defaultValue

let evaluateCondition condition state =
    let r, fn, v =
        match condition with
        | GreaterThan (r, v) -> (r, (>), v)
        | GreaterThanOrEqual (r, v) -> (r, (>=), v)
        | LessThan (r,v) -> (r, (<), v)
        | LessThanOrEqual (r, v) -> (r, (<=), v)
        | Equal (r,v) -> (r, (=), v)
        | NotEqual (r,v) -> (r, (<>), v)
    let reg =
        state
        |> Map.tryFind r
        |> someOrDefault 0
    fn reg v

let evaluateCommand command state =
    let r, fn =
        match command with
        | Increment (r, v) -> r, fun reg -> reg + v
        | Decrement (r, v) -> r, fun reg -> reg - v
    let s =
        if (Map.containsKey r state) then
            state
        else
            Map.add r 0 state
    let regValue = Map.find r s
    s
    |> Map.remove r
    |> Map.add r (fn regValue)

let getHighest registers =
    registers
    |> Map.toSeq
    |> Seq.maxBy (fun (_, value) -> value)


if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s dataFile" fsi.CommandLineArgs.[0]
    exit 1

let fileName = fsi.CommandLineArgs.[1]

let lines = File.ReadAllLines(fileName)
let instructions =
    lines
    |> Array.map parse

let highestValue, _ =
    instructions
    |> Array.fold (fun (maxValue, state) (command, condition) ->
        if evaluateCondition condition state then
            let newState = evaluateCommand command state
            let _, highest = getHighest newState
            let newMax = max maxValue highest
            (newMax, newState)
        else
            (maxValue, state)) (0, Map.empty)

// let reg, highestValue =
//     registers
//     |> Map.toSeq
//     |> Seq.maxBy (fun (_, value) -> value)

//printfn "Register: %s value: %i" reg highestValue
printfn "highestValue: %i" highestValue