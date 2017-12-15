#!/usr/bin/fsharpi

open System
open System.IO
open System.Text.RegularExpressions

type Program = { Name : string ; Weight : int ; Children : Set<string> }


let parse line =
    // bqyqwn (68) -> wscqe, cwxspl, syogw, xnxudsh
    let reg = Regex(@"^(\S+) \((\d+)\)( -> (.*))?$")
    if not (reg.IsMatch(line)) then
        failwith (sprintf "No match for %s" line)
    let name = reg.Replace(line, "$1")
    let weight = Int32.Parse(reg.Replace(line, "$2"))
    let children = reg.Replace(line, "$4").Replace(" ", "").Split([| ',' |]) |> Set.ofArray
    { Name = name ; Weight = weight ; Children = children }


if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s dataFile" fsi.CommandLineArgs.[0]
    exit 1

let fileName = fsi.CommandLineArgs.[1]

let lines = File.ReadAllLines(fileName)

let programs =
    lines
    |> Array.map parse

let allChildren =
    programs
    |> Seq.collect (fun x -> x.Children)
    |> Set.ofSeq

let AllParents =
    programs
    |> Seq.map (fun p -> p.Name)
    |> Set.ofSeq

let top =
    AllParents - allChildren

printfn "Top: %A" top