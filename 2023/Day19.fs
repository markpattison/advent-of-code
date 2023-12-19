module Day19

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day19.txt")

type Rating = X | M | A | S
type Condition = Gt | Lt
type FinalDestination = Rejected | Accepted
type Destination = Workflow of string | Final of FinalDestination

type Rule =
    {
        Rating: Rating
        Condition: Condition
        Value: int
        Destination: Destination
    }

type Workflow =
    {
        Rules: Rule list
        FinalDestination: Destination
    }

type Part = Map<Rating, int>

let parseRating (s: string) =
    match s with
    | "x" -> X
    | "m" -> M
    | "a" -> A
    | "s" -> S
    | _ -> failwith "invalid rating"

let parseDestination (s: string) =
    match s with
    | "R" -> Final Rejected
    | "A" -> Final Accepted
    | wf -> Workflow wf

let parseRule (s: string) =
    let rating = parseRating (s.Substring(0, 1))
    let condition =
        if s.Contains('<') then
            Lt
        elif s.Contains('>') then
            Gt
        else
            failwith "invalid rule"
    
    let splits = s.Split(":")
    let value = splits.[0].Substring(2) |> Int32.Parse
    let dest = splits.[1] |> parseDestination

    {
        Rating = rating
        Condition = condition
        Value = value
        Destination = dest
    }

let parseWorkflow (s: string) =
    let splits = s.Split(",")
    let rules =
        splits
        |> Array.take (splits.Length - 1)
        |> Array.map parseRule
        |> Array.toList
    let final = splits.[splits.Length - 1] |> parseDestination

    { Rules = rules; FinalDestination = final }

let parseWorkflowWithName (s: string) =
    let splits = s.Split("{")
    let workflowName = splits.[0]
    let workflow = parseWorkflow (splits.[1].Substring(0, splits.[1].Length - 1))

    (workflowName, workflow)

let numWorkflows =
    Array.findIndex (fun s -> s = "") input

let workflows =
    input
    |> Array.take numWorkflows
    |> Array.map parseWorkflowWithName
    |> Map.ofArray

let parsePart (s: string) : Part =
    let splits = s.Substring(1, s.Length - 2).Split(",")

    [
        X, splits.[0].Substring(2) |> Int32.Parse
        M, splits.[1].Substring(2) |> Int32.Parse
        A, splits.[2].Substring(2) |> Int32.Parse
        S, splits.[3].Substring(2) |> Int32.Parse
    ] |> Map.ofList

let parts =
    input
    |> Array.skip (numWorkflows + 1)
    |> Array.map parsePart

let matchesCondition (part: Part) rule =
    let relevantValue = part.[rule.Rating]
    
    match rule.Condition with
        | Gt -> relevantValue > rule.Value
        | Lt -> relevantValue < rule.Value

let rec findDestination part finalDest rules =
    match rules with
    | [] -> finalDest
    | rule :: remainingRules ->
        if matchesCondition part rule then
            rule.Destination
        else
            findDestination part finalDest remainingRules

let rec finalDestination (wfs: Map<string, Workflow>) part nextWorkflowName =
    let wf = wfs.[nextWorkflowName]
    let dest = findDestination part wf.FinalDestination wf.Rules

    match dest with
    | Final f -> f
    | Workflow w -> finalDestination wfs part w

let score (part: Part) = part.[X] + part.[M] + part.[A] + part.[S]

let part1() =
    let findFinalDestination part = finalDestination workflows part "in"

    let acceptedParts =
        parts
        |> Array.filter (fun part -> findFinalDestination part = Accepted)
    
    let totalRatings =
        acceptedParts
        |> Array.sumBy score

    printfn "Total rating: %i" totalRatings

type Range =
    {
        Min: int
        Max: int
    }

type PartRange = Map<Rating, Range>

// returns rangeIfMatches, rangeIfDoesn'tMatch
let findRangeOverlap rule (partRange: PartRange) =
    let relevantRange = partRange.[rule.Rating]
    
    let newRange, remainingRange =
        match rule.Condition with
        | Gt ->
            if rule.Value >= relevantRange.Max then
                None, Some relevantRange
            elif rule.Value < relevantRange.Min then
                Some relevantRange, None
            else
                Some { Min = rule.Value + 1; Max = relevantRange.Max }, Some { Min = relevantRange.Min; Max = rule.Value }
        | Lt ->
            if rule.Value <= relevantRange.Min then
                None, Some relevantRange
            elif rule.Value > relevantRange.Max then
                Some relevantRange, None
            else
                Some { Min = relevantRange.Min; Max = rule.Value - 1 }, Some { Min = rule.Value; Max = relevantRange.Max }
    
    match newRange, remainingRange with
    | None, Some _ -> None, Some partRange
    | Some _, None -> Some partRange, None
    | Some newR, Some remR -> Some (Map.add rule.Rating newR partRange), Some (Map.add rule.Rating remR partRange)
    | None, None -> failwith "unexpected"

let rec processRangeForRules partRange (finalDest: Destination) rules =
    match rules with
    | [] -> [| (partRange, finalDest) |]
    | rule :: remainingRules ->
        let rangeIfMatches, rangeIfDoesnt = findRangeOverlap rule partRange
        let resultsIfMatches =
            match rangeIfMatches with
            | Some r -> [| (r, rule.Destination) |]
            | None -> [||]
        let resultsIfDoesnt =
            match rangeIfDoesnt with
            | Some r -> processRangeForRules r finalDest remainingRules
            | None -> [||]
        Array.append resultsIfMatches resultsIfDoesnt

let rec finalDestinations (wfs: Map<string, Workflow>) partRange nextWorkflowName =
    let wf = wfs.[nextWorkflowName]
    let dest = processRangeForRules partRange wf.FinalDestination wf.Rules

    dest
    |> Array.map (fun (pr, d) ->
                    match d with
                    | Final f -> [| (pr, f) |]
                    | Workflow w -> finalDestinations wfs pr w)
    |> Array.concat

let rangeSize range = (1 + range.Max - range.Min) |> int64
let includedParts (partRange: PartRange) =
    (rangeSize partRange.[X]) * (rangeSize partRange.[M]) * (rangeSize partRange.[A]) * (rangeSize partRange.[S])

let part2() =
    let findFinalDestinations partRange = finalDestinations workflows partRange "in"

    let initialRange = { Min = 1; Max = 4000 }
    let initialPartRange : PartRange =
        [ X, initialRange; M, initialRange; A, initialRange; S, initialRange ]
        |> Map.ofList

    let numAccepted =
        findFinalDestinations initialPartRange
        |> Array.filter (fun (_, f) -> f = Accepted)
        |> Array.sumBy (fun (r, _) -> includedParts r)
    
    printfn "Parts accepted: %i" numAccepted
