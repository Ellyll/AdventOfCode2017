#!/usr/bin/fsharpi

//let ns = [ 0..4 ] |> List.map (fun x -> (x,x)) |> Map.ofSeq
//let lengths = [ 3 ; 4 ; 1; 5 ]
let ns = [ 0..255 ] |> List.map (fun x -> (x,x)) |> Map.ofSeq
let lengths = [ 227 ; 169 ; 3 ; 166 ; 246 ; 201 ; 0 ; 47 ; 1 ; 255 ; 2 ; 254 ; 96 ; 3 ; 97 ; 144 ]

let hashedList =
    let numberOfElements = ns |> Map.count
    let _, _, newNs =
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
            (nextPosition, nextSkipSize, nextXs)) (0,0, ns)
    newNs
    |> Map.toList
    |> List.map (fun (_,v) -> v)

let result =
    hashedList
    |> List.take 2
    |> List.reduce (*)

printfn "Result: %i" result
