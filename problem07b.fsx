#!/usr/bin/fsharpi

open System
open System.IO
open System.Text.RegularExpressions

type Program = { Name : string ; Weight : int ; Children : Set<Program> }


let parse line =
    // bqyqwn (68) -> wscqe, cwxspl, syogw, xnxudsh
    let reg = Regex(@"^(\S+) \((\d+)\)( -> (.*))?$")
    if not (reg.IsMatch(line)) then
        failwith (sprintf "No match for %s" line)
    let name = reg.Replace(line, "$1")
    let weight = Int32.Parse(reg.Replace(line, "$2"))
    let children =
        reg.Replace(line, "$4").Replace(" ", "").Split([| ',' |])
        |> Array.filter (fun childName -> childName <> "")
        |> Set.ofArray
    (name, (weight, children))

let rec buildProgram (nodeName : string) (programsData : Map<string, int*Set<string>>) : Program =
    let weight, children = programsData |> Map.find nodeName
    { Name = nodeName ; Weight = weight ; Children = children |> Set.map (fun childName -> buildProgram childName programsData) }

let load lines =
    let programsData =
        lines
        |> Array.map parse
        |> Map.ofArray
    let allChildren =
        programsData
        |> Map.toSeq
        |> Seq.collect (fun (_, (_, children)) ->  children)
        |> Set.ofSeq
    let allPrograms =
        programsData
        |> Map.toSeq
        |> Seq.map (fun (name, _) -> name)
        |> Set.ofSeq
    let rootName =
        (allPrograms - allChildren)
        |> Set.minElement
    buildProgram rootName programsData

let rec getWeight program =
    program.Weight + (program.Children |> Seq.sumBy getWeight)

let rec findUnbalancedCause (siblings : seq<Program*int>) (program : Program) (weight : int) : seq<Program*int> * (Program * int) =
    let childrenWithWeights =
        program.Children
        |> Seq.map (fun child -> (child, getWeight child))
    let childrenByWeights =
        childrenWithWeights
        |> Seq.groupBy (fun (_,weight) -> weight)
    if Seq.length childrenByWeights <= 1 then
        (siblings, (program, weight))
    else
        let incorrectChild, incorrectWeight =
            childrenByWeights
            |> Seq.sortBy (fun (_, children) -> children |> Seq.length)
            |> Seq.head
            |> snd
            |> Seq.head

        let siblings =
            childrenWithWeights
            |> Seq.filter (fun (child, _) -> child <> incorrectChild)

        findUnbalancedCause siblings incorrectChild incorrectWeight

if (Array.length fsi.CommandLineArgs) <> 2 then
    eprintfn "Usage: %s dataFile" fsi.CommandLineArgs.[0]
    exit 1

let fileName = fsi.CommandLineArgs.[1]

let lines = File.ReadAllLines(fileName)

let program = load lines

let correctSiblings, (incorrectChild, incorrectWeight) = findUnbalancedCause Seq.empty program (getWeight program)
let correctWeight =
    correctSiblings
    |> Seq.head
    |> snd

let newWeight = incorrectChild.Weight - (incorrectWeight - correctWeight)

printfn "incorrect: %A Total weight: %i should be: %i, programs's weight should be changed from %i to %i"
        incorrectChild.Name incorrectWeight correctWeight incorrectChild.Weight newWeight
