#!/usr/bin/fsharpi

let parseNode (str : string) nodes  =
    // 2 <-> 0, 3, 4
    let parts = str.Split([| "<->" |], System.StringSplitOptions.RemoveEmptyEntries)
    if (Array.length parts) <> 2 then
        failwithf "Invalid node string: %s" str
    let key = int parts.[0]
    let value = (parts.[1].Split ',') |> Array.map (fun x -> int (x.Trim())) |> Set.ofArray
    Map.add key value nodes

let getProgramsInGroup programId nodes =
    let rec getProgs progId progsFound =
        let progsToTraverse = (Map.find progId nodes) - progsFound
        if (Set.isEmpty progsToTraverse) then
            (Set.add progId progsFound)
        else
            (Set.fold (fun pFound pId -> (getProgs pId pFound)) (Set.add progId progsFound) progsToTraverse)
    getProgs programId (Set.add programId Set.empty)

let fileName = fsi.CommandLineArgs.[1]

let lines = System.IO.File.ReadAllLines(fileName)

let nodes =
    lines
    |> Array.fold (fun state line -> parseNode line state) Map.empty

let progsIn0 = getProgramsInGroup 0 nodes

printfn "Number of programs in group 0: %i" (Set.count progsIn0)
