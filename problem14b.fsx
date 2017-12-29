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

let byteToBools b =
    [ 7..-1..0 ]
    |> List.map (fun pos -> b &&& (1uy <<< pos) <> 0uy)

let bytesToBools bytes =
    bytes
    |> Seq.collect (byteToBools)

let createGrid (rows : (byte seq) seq) =
    rows
    |> Seq.mapi (fun r row ->
        let columns =
            bytesToBools row
            |> Seq.mapi (fun c bl -> (c, bl))
            |> Map.ofSeq
        (r, columns))
    |> Map.ofSeq

let getRegions grid =
    let squaresInUse =
        grid
        |> Map.toSeq
        |> Seq.fold (fun used (r, columns) ->
                columns
                |> Map.toSeq
                |> Seq.fold (fun used2 (c, isUsed) ->
                    if isUsed then
                        Set.add (r,c) used2
                    else
                        used2
                    ) used
            ) Set.empty

    let rec findConnected (r,c) found remaining =        
        if (Set.contains (r,c) found) then
            (found, remaining)
        else
            let adjacents =
                ([
                    (r-1, c) ;
                    (r, c-1) ;
                    (r, c+1) ;
                    (r+1, c)
                 ]
                |> Set.ofList
                |> Set.intersect remaining)
            let nFound = Set.add (r,c) found
            let nRemaining = Set.remove (r,c) remaining
            if (Set.isEmpty adjacents) then
                (nFound, nRemaining)
            else
                Set.fold (fun (fnd,rem) (ra, ca) -> findConnected (ra, ca) fnd rem) (nFound, nRemaining) adjacents
    
    let rec getRegionsRec regions remaining =
        if (Set.isEmpty remaining) then
            regions
        else
            let r, c = Set.minElement remaining
            let region, rem = findConnected (r,c) Set.empty remaining
            getRegionsRec (region::regions) rem

    getRegionsRec List.empty squaresInUse



if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s key" fsi.CommandLineArgs.[0]
    exit 1

let key = fsi.CommandLineArgs.[1]

printfn "Getting hashes..."
let hashes =
    [ 0..127 ]
    |> Seq.map (fun row -> Seq.ofList (Knot.makeHash (sprintf "%s-%i" key row)))

printfn "Creating grid..."
let grid = createGrid hashes

printfn "Getting regions..."
let regions = getRegions grid

let numberOfRegions = List.length regions
printfn "Number of regions: %i" numberOfRegions
