module Day16

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day16.txt")

type Sue =
    {
        Id: int
        Compounds: Map<string, int>
    }

let parseLine (s: string) =
    let parts = s.Replace(":", "").Replace(",","").Split(' ')
    if parts.Length <> 8 then
        failwithf "unexpected: %s" s
    else
        {
            Id = parts.[1] |> Int32.Parse
            Compounds =
                [
                    parts.[2], parts.[3] |> Int32.Parse
                    parts.[4], parts.[5] |> Int32.Parse
                    parts.[6], parts.[7] |> Int32.Parse
                ] |> Map.ofList
        }

let aunts =
    input |> Array.map parseLine

let target =
    [
        "children", 3
        "cats", 7
        "samoyeds", 2
        "pomeranians", 3
        "akitas", 0
        "vizslas", 0
        "goldfish", 5
        "trees", 3
        "cars", 2
        "perfumes", 1
    ]

let part1() =
    let check aunt (compound, number) =
        match Map.tryFind compound aunt.Compounds with
        | Some n -> n = number
        | None -> true
    
    let checkAll aunt =
        target |> List.forall (check aunt)

    let sueNumber =
        aunts
        |> Array.filter checkAll
        |> Array.exactlyOne
        |> _.Id
    
    printfn "Number: %i" sueNumber

let part2() =
    let check aunt (compound, targetNumber) =
        match compound, Map.tryFind compound aunt.Compounds with
        | "trees", Some n | "cats", Some n -> n > targetNumber
        | "pomeranians", Some n | "goldfish", Some n -> n < targetNumber
        | _, Some n -> n = targetNumber
        | _, None -> true
    
    let checkAll aunt =
        target |> List.forall (check aunt)

    let sueNumber =
        aunts
        |> Array.filter checkAll
        |> Array.exactlyOne
        |> _.Id
    
    printfn "Number: %i" sueNumber
