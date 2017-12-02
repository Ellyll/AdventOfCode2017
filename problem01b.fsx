#!/usr/bin/fsharpi

open System

let circularPairwiseBy (f : int -> int*int) (xs : 'a[]) : ('a*'a)[] =
    let len = xs |> Array.length
    if len < 2 then
        Array.empty
    else
        xs
        |> Array.mapi (fun idx _ ->
            let i,j = f idx
            let a = xs.[i]
            let b = xs.[j]
            (a, b)
            )
           
let solve (x : string) =
    let arr = x.ToCharArray() |> Array.map (fun x -> Int32.Parse(x.ToString()))
    let len = Array.length arr

    arr
    |> circularPairwiseBy (fun i -> (i, (i+(len/2)) % len))
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
