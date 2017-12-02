#!/usr/bin/fsharpi

open System
open System.IO
open System.Text.RegularExpressions


let rowDivisables (row : int seq) : int =
    let lst = List.ofSeq row
    lst
    |> List.sumBy (fun x ->
        lst
        |> List.sumBy (fun y ->
            if (x > y) && (x % y = 0) then
                x / y
            else
                0
        )
    )

let calculateChecksum spreadsheet =
    spreadsheet
    |> Seq.sumBy (rowDivisables)

if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s dataFile" fsi.CommandLineArgs.[0]
    exit 1

let fileName = fsi.CommandLineArgs.[1]

let lines = File.ReadAllLines(fileName)

let regWhiteSpace = Regex(@"\s+")

let rows =
    lines
    |> Array.map (fun line ->
        regWhiteSpace.Replace(line, "\t")
            .Trim()
            .Split([| '\t' |], StringSplitOptions.None)
        |> Array.map (fun digits ->
            if (digits |> Seq.exists (Char.IsDigit >> not)) then                
                eprintfn "Input must contain digits only"
                exit 1
            Int32.Parse(digits)
        )
    )

let checksum = calculateChecksum rows

printfn "Checksum for %s is %d" (String.Join(Environment.NewLine, lines)) checksum
