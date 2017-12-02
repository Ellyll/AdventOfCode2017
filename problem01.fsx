#!/usr/bin/fsharpi

open System

let circularPairwise (xs : 'a list) : ('a*'a) list =
    let len = xs |> List.length
    if len <= 2 then
        xs |> List.pairwise
    else
        (List.last xs, List.head xs)::(List.pairwise xs)
            

let solve (x : string) =
    x
    |> List.ofSeq
    |> List.map (fun x -> Int32.Parse(x.ToString()))
    |> circularPairwise
    |> Seq.filter (fun (a,b) -> a = b)    
    |> Seq.sumBy (fun (a,_) -> a)


if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s digits" fsi.CommandLineArgs.[0]
    exit 1

let digits = fsi.CommandLineArgs.[1].Trim()

if (digits |> Seq.exists (Char.IsDigit >> not)) then
    eprintfn "Input must be digits only"
    exit 1

let result = solve digits
printfn "The result of %s is %d" digits result
