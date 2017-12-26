#!/usr/bin/fsharpi

module Knot =

    let doRound startPosition startSkipSize lengths hash =
        let numberOfElements = hash |> Map.count
        let lastPosition, lastSkipSize, newHash =
            lengths
            |> List.fold (fun (currentPosition, skipSize, xs) length ->
                let nextXs =
                    if length = 0 then
                        xs
                    else
                        let keysToRev = [ for i in currentPosition..currentPosition+length-1 -> i % numberOfElements ]
                        let revdValues = [ for k in keysToRev -> Map.find k xs ] |> List.rev
                        let revdMap = List.zip keysToRev revdValues |> Map.ofSeq
                        keysToRev
                        |> List.fold (fun m k ->
                            Map.add k (Map.find k revdMap) m
                        ) (xs |> Map.filter (fun k _ -> not (Map.containsKey k revdMap)))
                let nextPosition = (currentPosition+length+skipSize) % numberOfElements
                let nextSkipSize = skipSize + 1
                (nextPosition, nextSkipSize, nextXs)) (startPosition, startSkipSize, hash)
        (lastPosition, lastSkipSize, newHash)

    let makeDenseHash sparseHash =
        sparseHash
        |> List.chunkBySize 16
        |> List.map (fun block -> List.reduce (^^^) block)

    let makeSparseHash (input : string) =
        let lengths =
            ((input
            |> Seq.map (byte)
            |> List.ofSeq)
            @ [ 17uy ; 31uy ; 73uy ; 47uy ; 23uy ])
            |> List.map int
        let _, _, sparseHash =
            let initialHash = [ 0..255 ] |> List.map (fun x -> (x,x)) |> Map.ofSeq
            seq { 0..63 }
            |> Seq.fold (fun (pos, skip, hash) _ ->
                (doRound pos skip lengths hash)) (0, 0, initialHash)
        sparseHash
        |> Map.toList
        |> List.map (fun (_,v) -> v)

    let byteToHex (bytes: byte seq) = Seq.fold (fun state x-> state + sprintf "%02X" x) ("") bytes

    let makeHash input =
        input
        |> makeSparseHash
        |> makeDenseHash
        |> List.map (byte)


let countSetBits (b : byte) : int =
    let binString = (System.Convert.ToString(b,2))
    binString
    |> Seq.filter (fun c -> c = '1')
    |> Seq.length

let getUsed (bytes : byte seq) =
    bytes
    |> Seq.sumBy (countSetBits)


if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s key" fsi.CommandLineArgs.[0]
    exit 1

let key = fsi.CommandLineArgs.[1]

printfn "Getting hashes..."
let hashes =
    [0..127]
    |> List.map (fun row -> Knot.makeHash (sprintf "%s-%i" key row))

printfn "Calulating used squares..."
let used =
    hashes
    |> List.sumBy (getUsed)

printfn "Used squares: %i" used