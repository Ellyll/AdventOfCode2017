#!/usr/bin/fsharpi

let memoryBanks = [| 14 ; 0 ; 15 ; 12 ; 11 ; 11 ; 3 ; 5 ; 1 ; 6 ; 8 ; 4 ; 9 ; 1 ; 8 ; 4 |]
//[| 0 ; 2 ; 7 ; 0 |]

let rec distribute amount index memoryBanks =
    if amount = 0 then
        memoryBanks
    else
        let nextMBs =
            memoryBanks
            |> Array.mapi (fun i x -> if i = index then x + 1 else x)
        let nextIndex = if (index + 1) >= (Array.length memoryBanks) then 0 else (index + 1)
        distribute (amount - 1) nextIndex nextMBs


let rec getNumberOfConfigurations configs count memoryBanks =
    if configs |> Set.contains memoryBanks then
        count
    else
        let nextConfigs = Set.add memoryBanks configs
        let maxIndex =  
            memoryBanks
            |> Array.mapi (fun i x -> i, x)
            |> Array.maxBy snd 
            |> fst
        let nextIndex = if (maxIndex+1) >= (Array.length memoryBanks) then 0 else (maxIndex+1)
        let nextMBs =
            memoryBanks
            |> Array.mapi (fun i x -> if i = maxIndex then 0 else x)
            |> distribute memoryBanks.[maxIndex] nextIndex
        getNumberOfConfigurations nextConfigs (count + 1) nextMBs

let numberOfConfigs = getNumberOfConfigurations (Set.empty) 0 memoryBanks

printfn "Number of configs: %i" numberOfConfigs