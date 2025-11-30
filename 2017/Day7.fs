module Day7

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day7.txt")

type Program =
    {
        Name: string
        Weight: int
        Supported: string list
    }

let parseLine (s: string) =
    let parts = s.Split(' ')
    let name = parts.[0]
    let weight = Int32.Parse(parts.[1].Substring(1, parts.[1].Length - 2))
    let supported =
        if parts.Length = 2 then
            []
        else
            parts
            |> Array.skip 3
            |> Array.map (fun p -> p.Replace(",", ""))
            |> Array.toList
    { Name = name; Weight = weight; Supported = supported }

let allPrograms =
    input
    |> Array.map parseLine

let allSupported =
    allPrograms
    |> Array.map (fun p -> p.Supported)
    |> Seq.concat

let baseProgram =
    allPrograms
    |> Seq.map _.Name
    |> Seq.except allSupported
    |> Seq.exactlyOne

let part1() =
    printfn "Base program: %s" baseProgram

let part2() =
    let mutable totalWeights: Map<string, int> = Map.empty

    let rec populateWeights name =
        let program = allPrograms |> Array.find (fun p -> p.Name = name)
        let supportedWeights =
            program.Supported
            |> List.sumBy (fun p ->
                match Map.tryFind p totalWeights with
                | Some w -> w
                | None -> populateWeights p)
        let totalWeight = program.Weight + supportedWeights
        totalWeights <- Map.add name totalWeight totalWeights
        totalWeight
    
    let _ = populateWeights baseProgram

    let allUnbalanced =
        allPrograms |>
        Array.choose (fun p ->
            match p.Supported with
            | [] -> None
            | supported ->
                let supportedWeights = supported |> List.map (fun s -> s, totalWeights.[s])
                let groups = supportedWeights |> List.map snd |> List.countBy id |> List.sortByDescending snd
                match groups with
                | [ correctWeight, _; badWeight, 1 ] ->
                    let adjustmentNeeded = correctWeight - badWeight
                    let badName = supportedWeights |> List.find (fun (_, w) -> w = badWeight) |> fst
                    let badProgram = allPrograms |> Array.find (fun p -> p.Name = badName)
                    let adjustedWeight = badProgram.Weight + adjustmentNeeded
                    Some (totalWeights.[p.Name], adjustedWeight)
                | _ -> None)
    
    let toCorrect =
        allUnbalanced
        |> Array.minBy fst
        |> snd

    printfn "Weight should be: %i" toCorrect
