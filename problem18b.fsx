#!/usr/bin/fsharpi

type Parser<'a> = char list -> ('a * char list) option

type RegisterOrValue =
    | Register of string
    | Value of int64

type RegisterOnly = | Register of string
type ValueOnly = | Value of int64

type Instruction =
    | Snd of RegisterOrValue
    | Set of RegisterOnly*RegisterOrValue
    | Add of RegisterOnly*RegisterOrValue
    | Mul of RegisterOnly*RegisterOrValue
    | Mod of RegisterOnly*RegisterOrValue
    | Rcv of RegisterOnly
    | Jgz of RegisterOrValue*RegisterOrValue

type ProgramStatus =
    | Running
    | Terminated

type State = { Id : int64 ; TargetId : int64 ; Registers : Map<string, int64> ; InstructionPtr : int64 ; Instructions : Instruction list ; ExecutionCount : int64 ; Status : ProgramStatus }

type ManagerReplyMessage =
    | Run
    | Deadlock
    | Value of int64

type ProgramMessage =
    | Receive of int64*int64*AsyncReplyChannel<ManagerReplyMessage>
    | Send of int64*int64*int64

type ProgramManagedStatus = { AwaitingReply : bool ; ReplyChannel : AsyncReplyChannel<ManagerReplyMessage> option ; Queue : int64 list ; SendCount : int }


let parseLetters : Parser<string> =
    let rec parseInnerLetters =
        function
        | (str, x::rest) when System.Char.IsLetter x ->
            let s = str + (x.ToString())
            parseInnerLetters (s, rest)
        | (str, xs) -> Some (str, xs)

    function
    | [] ->
        printfn "Attempt to parse zero characters as letters"
        None
    | c::cs when System.Char.IsLetter(c) -> parseInnerLetters ("", c::cs)
    | c::_ ->
        eprintfn "Invalid character while parsing letters: %s" (c.ToString())
        None

let parseDigits : Parser<int64> =
    let rec parseInnerDigits =
        function
        | (n, x::rest) when System.Char.IsDigit x ->
            let m = (n*10L) + (int64 (x.ToString()))
            parseInnerDigits (m, rest)
        | (n, xs) -> Some (n, xs)

    function
    | [] ->
        eprintfn "Attempt to parse zero characters as digits"
        None
    | '-'::c::cs when System.Char.IsDigit(c) ->
        match parseInnerDigits (0L, c::cs) with
        | Some (n, rest) -> Some (n * -1L, rest)
        | None -> None
    | c::cs when System.Char.IsDigit(c) -> parseInnerDigits (0L, c::cs)
    | c::_ ->
        eprintfn "Invalid character while parsing digits: %s" (c.ToString())
        None

let parseRegisterOrValue : Parser<RegisterOrValue> =
    function
    | '-'::c::cs when System.Char.IsDigit(c) ->
        match parseDigits ('-'::c::cs) with
        | Some (num, rest) -> Some (RegisterOrValue.Value num, rest)
        | None -> None
    | c::cs when System.Char.IsDigit(c) ->
        match parseDigits (c::cs) with
        | Some (num, rest) -> Some (RegisterOrValue.Value num, rest)
        | None -> None
    | c::cs when System.Char.IsLetter(c) ->
        match parseLetters (c::cs) with
        | Some (s, rest) -> Some (RegisterOrValue.Register s, rest)
        | None -> None
    | c ->
        eprintfn "Invalid character while parsing RegisterOrValue: %s" (c.ToString())
        None

let parseRegisterOnly: Parser<RegisterOnly> =
    function
    | c::cs when System.Char.IsLetter(c) ->
        match parseLetters (c::cs) with
        | Some (s, rest) -> Some (Register s, rest)
        | None -> None
    | c ->
        eprintfn "Invalid character while parsing RegisterOnly: %s" (c.ToString())
        None

let parseValueOnly : Parser<ValueOnly> =
    function
    | c::cs when System.Char.IsDigit(c) ->
        match parseDigits (c::cs) with
        | Some (num, rest) -> Some (ValueOnly.Value num, rest)
        | None -> None
    | c ->
        eprintfn "Invalid character while parsing ValueOnly: %s" (c.ToString())
        None

let parseSnd : Parser<Instruction> =
    function
    | [] ->
        eprintfn "Empty snd parameter"
        None
    | cs ->
        match parseRegisterOrValue cs with
        | Some (regOrVal, rest) -> Some (Snd regOrVal, rest)
        | None ->
            eprintfn "Invalid snd parameter"
            None

let parseSet : Parser<Instruction> =
    function
    | [] ->
        eprintfn "Error parsing set - missing parameters"
        None
    | cs ->
        match parseRegisterOnly cs with
        | Some (reg, ' '::rest) ->
            match parseRegisterOrValue rest with
            | Some (regOrVal, rest2) -> Some (Set (reg, regOrVal), rest2)
            | None ->
                eprintfn "Error parsing set - invalid set 2nd parameter"
                None
        | Some (_, x::_) ->
            eprintfn "Error parsing set - expecting space but got '%s'" (x.ToString())
            None
        | Some (_, []) ->
            eprintfn "Error parsing set - missing 2nd parameter"
            None
        | None ->
            eprintfn "Error parsing set - invalid 1st parameter"
            None

let parseAdd : Parser<Instruction> =
    function
    | [] ->
        eprintfn "Error parsing add - missing parameters"
        None
    | cs ->
        match parseRegisterOnly cs with
        | Some (reg, ' '::rest) ->
            match parseRegisterOrValue rest with
            | Some (regOrVal, rest2) -> Some (Add (reg, regOrVal), rest2)
            | None ->
                eprintfn "Error parsing add - invalid 2nd parameter"
                None
        | Some (_, x::_) ->
            eprintfn "Error parsing add- expecting space but got '%s'" (x.ToString())
            None
        | Some (_, []) ->
            eprintfn "Error parsing add - missing 2nd parameter"
            None
        | None ->
            eprintfn "Error parsing add - invalid 1st parameter"
            None

let parseMul : Parser<Instruction> =
    function
    | [] ->
        eprintfn "Error parsing mul - missing parameters"
        None
    | cs ->
        match parseRegisterOnly cs with
        | Some (reg, ' '::rest) ->
            match parseRegisterOrValue rest with
            | Some (regOrVal, rest2) -> Some (Mul (reg, regOrVal), rest2)
            | None ->
                eprintfn "Error parsing mul - invalid 2nd parameter"
                None
        | Some (_, x::_) ->
            eprintfn "Error parsing mul- expecting space but got '%s'" (x.ToString())
            None
        | Some (_, []) ->
            eprintfn "Error parsing mul - missing 2nd parameter"
            None
        | None ->
            eprintfn "Error parsing mul - invalid 1st parameter"
            None

let parseMod : Parser<Instruction> =
    function
    | [] ->
        eprintfn "Error parsing mod - missing parameters"
        None
    | cs ->
        match parseRegisterOnly cs with
        | Some (reg, ' '::rest) ->
            match parseRegisterOrValue rest with
            | Some (regOrVal, rest2) -> Some (Mod (reg, regOrVal), rest2)
            | None ->
                eprintfn "Error parsing mod - invalid 2nd parameter"
                None
        | Some (_, x::_) ->
            eprintfn "Error parsing mod- expecting space but got '%s'" (x.ToString())
            None
        | Some (_, []) ->
            eprintfn "Error parsing mod - missing 2nd parameter"
            None
        | None ->
            eprintfn "Error parsing mod - invalid 1st parameter"
            None

let parseRcv : Parser<Instruction> =
    function
    | [] ->
        eprintfn "Empty rcv parameter"
        None
    | cs ->
        match parseRegisterOnly cs with
        | Some (reg, rest) -> Some (Rcv reg, rest)
        | None ->
            eprintfn "Invalid rcv parameter"
            None

let parseJgz : Parser<Instruction> =
    function
    | [] ->
        eprintfn "Error parsing jgz - missing parameters"
        None
    | cs ->
        match parseRegisterOrValue cs with
        | Some (regOrVal1, ' '::rest) ->
            match parseRegisterOrValue rest with
            | Some (regOrVal2, rest2) -> Some (Jgz (regOrVal1, regOrVal2), rest2)
            | None ->
                eprintfn "Error parsing jgz - invalid 2nd parameter"
                None
        | Some (_, x::_) ->
            eprintfn "Error parsing jgz- expecting space but got '%s'" (x.ToString())
            None
        | Some (_, []) ->
            eprintfn "Error parsing jgz - missing 2nd parameter"
            None
        | None ->
            eprintfn "Error parsing jgz - invalid 1st parameter"
            None

let parseInstruction : Parser<Instruction> =
    function
    | 's'::'n'::'d'::' '::rest -> parseSnd rest
    | 's'::'e'::'t'::' '::rest -> parseSet rest
    | 'a'::'d'::'d'::' '::rest -> parseAdd rest
    | 'm'::'u'::'l'::' '::rest -> parseMul rest
    | 'm'::'o'::'d'::' '::rest -> parseMod rest
    | 'r'::'c'::'v'::' '::rest -> parseRcv rest
    | 'j'::'g'::'z'::' '::rest -> parseJgz rest
    | str ->
        eprintfn "Invalid instruction: %s" (str |> List.toArray |> System.String)
        None

let getRegisterOrValue (regOrVal : RegisterOrValue) state =
    match regOrVal with
    | RegisterOrValue.Register reg -> Map.find reg state.Registers
    | RegisterOrValue.Value value -> value

let getRegister (regOnly : RegisterOnly) state =
    match regOnly with
    | RegisterOnly.Register reg -> Map.find reg state.Registers

let setRegister regOnly (value : int64) registers =
    let key = match regOnly with | Register r -> r
    registers
    |> Map.remove key
    |> Map.add key value

let executeSnd regOrVal (manager : MailboxProcessor<ProgramMessage>) state =
    let value = getRegisterOrValue regOrVal state
    manager.Post (ProgramMessage.Send (state.Id, state.TargetId, value))
    { state with InstructionPtr = state.InstructionPtr + 1L }
 
let executeSet regOnly regOrVal state =
    let value = (getRegisterOrValue regOrVal state)
    let registers = (setRegister regOnly value state.Registers)
    { state with Registers = registers ; InstructionPtr = state.InstructionPtr + 1L }

let executeOp op regOnly regOrVal state =
    let a = getRegister regOnly state
    let b = getRegisterOrValue regOrVal state
    let value = op a b
    let registers = (setRegister regOnly value state.Registers)
    { state with Registers = registers ; InstructionPtr = state.InstructionPtr + 1L }

let executeAdd regOnly regOrVal state =
    executeOp (+) regOnly regOrVal state

let executeMul regOnly regOrVal state =
    executeOp (*) regOnly regOrVal state

let executeMod regOnly regOrVal state =
    executeOp (%) regOnly regOrVal state

let executeRcv regOnly (manager : MailboxProcessor<ProgramMessage>) state =
        let message = manager.PostAndReply (fun replyChannel -> Receive (state.Id, state.TargetId, replyChannel))
        match message with
        | Deadlock ->
            { state with Status = Terminated }
        | Value value ->
            let registers = (setRegister regOnly value state.Registers)
            { state with Registers = registers ; InstructionPtr = state.InstructionPtr + 1L }
        | _ ->
            failwithf "Unexpected message: %A" message

let executeJgz regOrVal1 regOrVal2 state =
    let testValue = getRegisterOrValue regOrVal1 state
    let jmpValue =
        if testValue <= 0L then
            1L
        else
            getRegisterOrValue regOrVal2 state
    { state with InstructionPtr = state.InstructionPtr + jmpValue }

let printState state =
    let registers =
        state.Registers
        |> Map.toList
        |> List.map (fun (k,v) -> sprintf "%s:%i" k v)
        |> fun xs -> System.String.Join(" ", xs)
    printfn "id:%i targetId:%i %s ptr:%i cnt:%i" state.Id state.TargetId registers state.InstructionPtr state.ExecutionCount


let runInstruction manager state =
    let newState =
        if state.Status = Terminated then
            state
        elif state.InstructionPtr >= (int64 (List.length state.Instructions)) || state.InstructionPtr < 0L then
            { state with Status = Terminated }
        else
            let instruction = state.Instructions |> List.item (int state.InstructionPtr)
            let state2 =
                match instruction with
                | Snd regOrVal -> executeSnd regOrVal manager state
                | Set (regOnly, regOrVal) -> executeSet regOnly regOrVal state
                | Add (regOnly, regOrVal) -> executeAdd regOnly regOrVal state
                | Mul (regOnly, regOrVal) -> executeMul regOnly regOrVal state
                | Mod (regOnly, regOrVal) -> executeMod regOnly regOrVal state
                | Rcv regOnly -> executeRcv regOnly manager state
                | Jgz (regOrVal1, regOrVal2) -> executeJgz regOrVal1 regOrVal2 state
            { state2 with ExecutionCount = state2.ExecutionCount + 1L }
    newState


let makeManager programIds=
    MailboxProcessor<ProgramMessage>.Start(fun inbox ->
        printfn "manager starting"
        let initialQueue =
            programIds
            |> Seq.map (fun pId -> (pId, { AwaitingReply = false ; ReplyChannel = None ; Queue = [] ; SendCount = 0 }))
            |> Map.ofSeq
        let rec loop states =
            async {
                let! message = inbox.Receive()
                let newStatesOpt =
                    match message with
                    | Receive (source, target, replyChannel) ->
                        if source = target then
                            failwithf "Program %i attempted to Receive from self" source
                        let sourceState = states |> Map.find source
                        match sourceState.Queue with
                        | x::xs ->
                            replyChannel.Reply (Value x)
                            states
                            |> Map.remove source
                            |> Map.add source ({ sourceState with Queue = xs })
                            |> Some
                        | [] ->
                            let targetState = states |> Map.find target
                            if targetState.AwaitingReply then
                                replyChannel.Reply Deadlock
                                targetState.ReplyChannel
                                |> Option.iter (fun (rc : AsyncReplyChannel<ManagerReplyMessage>) -> rc.Reply Deadlock)
                                printfn "Deadlock detected: source=%i target=%i" source target
                                None
                            else
                                states
                                |> Map.remove source
                                |> Map.add source ({ sourceState with AwaitingReply = true ; ReplyChannel = Some replyChannel })
                                |> Some
                    | Send (source, target, value) ->
                        if source = target then
                            failwithf "Program %i attempted to Send to self" source
                        let sourceState = states |> Map.find source
                        let targetState = states |> Map.find target
                        match targetState.AwaitingReply with
                        | true ->
                            targetState.ReplyChannel
                            |> Option.iter (fun (rc : AsyncReplyChannel<ManagerReplyMessage>) -> rc.Reply (Value value))
                            states
                            |> Map.remove target
                            |> Map.add target ({ targetState with AwaitingReply = false ; ReplyChannel = None })
                            |> Map.remove source
                            |> Map.add source ({ sourceState with SendCount = sourceState.SendCount + 1})
                            |> Some
                        | false ->
                            states
                            |> Map.remove target
                            |> Map.add target ({ targetState with Queue = targetState.Queue @ [ value ] })
                            |> Map.remove source
                            |> Map.add source ({ sourceState with SendCount = sourceState.SendCount + 1})
                            |> Some
                match newStatesOpt with
                | Some newStates -> do! loop newStates
                | None ->
                    states |> Map.iter (fun pid state -> printfn "ProgramId: %i: SendCount: %i" pid state.SendCount)
                    ()
            }
        loop initialQueue)

let makeProgramAgent programId targetId instructions manager =
    let initialState =
        {
            Id = programId ;
            TargetId = targetId ;
            Registers = ([ 'a'..'p' ] |> List.map (fun r -> ((r.ToString()),0L)) |> Map.ofList) |> setRegister (Register "p") programId ;
            InstructionPtr = 0L ;
            Instructions = instructions ;
            ExecutionCount = 0L ;
            Status = Running
        }
    MailboxProcessor<ManagerReplyMessage>.Start(fun inbox ->
        printfn "Program %i starting" initialState.Id
        let rec loop state =
            async {
                let newState = runInstruction manager state
                if newState.Status = Terminated then
                    ()
                else
                    do! loop newState
            }
        loop initialState)




if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s filename" fsi.CommandLineArgs.[0]
    exit 1

let filename = fsi.CommandLineArgs.[1]

let input = System.IO.File.ReadAllLines(filename)

printfn "Parsing..."
let instructions =
    input
    |> Seq.map (Seq.toList >> parseInstruction)
    |> Seq.choose id
    |> Seq.map (fst)
    |> Seq.toList

printfn "Runnning..."

let manager = makeManager [ 0L ; 1L ]
let programAgent1 = makeProgramAgent 0L 1L instructions manager
let programAgent2 = makeProgramAgent 1L 0L instructions manager

printfn "Press return to finish"
System.Console.ReadLine() |> ignore
printfn "Finished"
