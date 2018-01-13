#!/usr/bin/fsharpi

type Rule = { In : bool[,] ; Out : bool[,] }

let parsePart (part : string) =
    let arrOfArr =
        part.Split '/'
        |> Array.map (fun rowStr ->
                rowStr
                |> Seq.map (fun c ->
                    match c with
                    | '.' -> false
                    | '#' -> true
                    | _ -> failwithf "Invalid character: %s" (c.ToString())
                    )
                |> Seq.toArray
            )
    Array2D.init (Array.length arrOfArr) (Array.length arrOfArr.[0]) (fun r c -> arrOfArr.[r].[c])


let parseRule (line : string) =
    // ../.. => ..#/.#./...
    let parts =
        line.Replace(" => ", "|").Split('|')
        |> Array.map (parsePart)
    { In = parts.[0] ; Out = parts.[1] }

let getRow (rowIndex : int) (arr2D : 'a[,]) : 'a[] =
    [| 0..(Array2D.length2 arr2D)-1 |]
    |> Array.map (fun col -> arr2D.[rowIndex,col])

let toRows arr2D =
    [| 0..(Array2D.length1 arr2D)-1 |]
    |> Array.map (fun r -> arr2D |> getRow r)

let partToString (part : bool[,]) =
    part
    |> Array2D.map (fun b -> if b then '#' else '.')
    |> toRows
    |> Array.map (fun row -> System.String(row))
    |> fun strs -> (System.String.Join("/", strs))

let ruleToString { In = input ; Out = output } =
    (partToString input) + " => " + (partToString output)

let rotate (arr : 'a[,]) : 'a[,] =
    let newRowLength = Array2D.length2 arr
    let newColLength = Array2D.length1 arr
    Array2D.init newRowLength newColLength  (fun i j -> arr.[newColLength-1-j,i])

let rec rotateN n arr =
    match n with
    | 0 -> arr
    | _ -> rotateN (n - 1) (rotate arr)

let flipV arr =
    let l = (Array2D.length2 arr) - 1
    arr |> Array2D.mapi (fun r c _ -> arr.[r, l-c])

let flipH arr =
    let l = (Array2D.length1 arr) - 1
    arr |> Array2D.mapi (fun r c _ -> arr.[l-r, c])

let isRuleMatch block rule =
    let combinations =
        [ 0..3 ]
        |> List.collect (fun n ->
            let rl = rotateN n rule.In
            [ rl ; flipH rl ; flipV rl ]
            )
    let isMatch =
        combinations
        |> List.contains (block)
    isMatch

let findRule (block : bool[,]) rules =
    let size = Array2D.length1 block
    rules
    |> Seq.filter (fun rule -> (Array2D.length2 rule.In) = size)
    |> Seq.find (isRuleMatch block)

let iterateBlock rules block =
    let rule = findRule block rules
    rule.Out

let chunkify image =
    if (Array2D.length1 image) <> (Array2D.length2 image) then
        failwithf "Image not square, lengths: %ix%i" (Array2D.length1 image) (Array2D.length2 image)
    let length = Array2D.length1 image

    let chunkSize =
        if length % 2 = 0 then
            2
        elif length % 3 = 0 then
            3
        else
            failwithf "Invalid length for chunking: %i" length

    array2D (List.fold (fun rows r ->
                let row =
                    List.fold (fun columns c ->
                            Array2D.init chunkSize chunkSize (fun r2 c2 -> image.[r+r2, c+c2]) :: columns
                        ) [] [ 0..chunkSize..length-1 ] // columns
                    |> List.rev
                row::rows
                ) [] [ 0..chunkSize..length-1 ] // rows
            |> List.rev            
        )     

let dechunkify (blocks : 'a[,][,]) =
    let chunkLength = Array2D.length1 blocks.[0,0]
    let length = (chunkLength) * (Array2D.length1 blocks)
    Array2D.init length length (fun r c ->
        blocks.[r/chunkLength, c/chunkLength].[r % chunkLength, c % chunkLength]
        )

let iterateImage rules image =
    chunkify image
    |> Array2D.map (iterateBlock rules)
    |> dechunkify

let array2Dfold (f) state (arr : 'a[,]) =
    seq {
        for r in 0..(Array2D.length1 arr)-1 do
            for c in 0..(Array2D.length2 arr)-1 -> arr.[r,c]        
    }
    |> Seq.fold f state


if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s filename" fsi.CommandLineArgs.[0]
    exit 1

printfn "Running..."

let filename = fsi.CommandLineArgs.[1]
let lines = System.IO.File.ReadAllLines(filename)
let rules = lines |> Array.map (parseRule)

let initialImage = ".#./..#/###" |> parsePart

let numberOfIterations = 5

let finalImage =
    Seq.unfold (fun state ->
        let newState = iterateImage rules state
        Some (newState, newState)
        ) initialImage
    |> Seq.skip (numberOfIterations - 1)
    |> Seq.head

let numberOfpixelsOn =
    finalImage
    |> array2Dfold (fun state b -> if b then state + 1 else state) 0

printfn "InitialImage: %s" (partToString initialImage)
printfn "Final Image:\n%s" ((partToString finalImage).Replace("/", "\n"))
printfn "Number of iterations: %i" numberOfIterations
printfn "Number of pixels on: %i" numberOfpixelsOn
