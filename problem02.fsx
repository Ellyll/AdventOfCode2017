#!/usr/bin/fsharpi

open System
open System.IO
open System.Text.RegularExpressions

let rowDiff row =
    let min = Seq.min row
    let max = Seq.max row
    max - min

let calculateChecksum spreadsheet =
    spreadsheet
    |> Seq.sumBy (rowDiff)

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
