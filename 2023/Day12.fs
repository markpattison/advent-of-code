module Day12

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day12.txt")

type Spring = Operational | Damaged | Unknown
type Row =
    {
        Springs: Spring[]
        Groups: int[]
    }

let parseSpring c =
    match c with
    | '.' -> Operational
    | '#' -> Damaged
    | '?' -> Unknown
    | _ -> failwith "unexpected"

let parseLine (s: string) =
    let split = s.Split(' ')
    let springs = split.[0].ToCharArray() |> Array.map parseSpring
    let groups = split.[1].Split(',') |> Array.map Int32.Parse

    { Springs = springs; Groups = groups }

let rows =
    input
    |> Array.map parseLine

type GroupStatus =
    | Finished of int list
    | Unfinished of int list

let rec toGroups revAcc (springs: Spring[]) =
    if springs.Length = 0 then
        Finished (List.rev revAcc)
    else
        match springs |> Array.tryFindIndex (fun s -> s <> Damaged) with
        | None ->
            toGroups (springs.Length :: revAcc) [||]
        | Some 0 ->
            if springs.[0] = Operational then
                toGroups revAcc springs.[1 ..]
            else
                Unfinished (List.rev revAcc)
        | Some firstNonDamaged ->
            if springs.[firstNonDamaged] = Operational then
                toGroups (firstNonDamaged :: revAcc) springs.[(firstNonDamaged + 1) ..]
            else
                Unfinished (List.rev (firstNonDamaged :: revAcc))

let rec matches tryGroups targetGroups =
    match tryGroups, targetGroups with
    | [], [] -> true
    | [], _ -> true
    | _, [] -> false
    | [ xTry ], xTarget :: _ when xTry <= xTarget -> true
    | [ _ ], _ -> false
    | xTry :: xsTry, xTarget :: xsTarget when xTry = xTarget -> matches xsTry xsTarget
    | _ -> false

let unfold row =
    let newSprings =
        [|
            yield! row.Springs
            yield Unknown
            yield! row.Springs
            yield Unknown
            yield! row.Springs
            yield Unknown
            yield! row.Springs
            yield Unknown
            yield! row.Springs
        |]
    let newGroups =
        [|
            yield! row.Groups
            yield! row.Groups
            yield! row.Groups
            yield! row.Groups
            yield! row.Groups
        |]
    { Springs = newSprings; Groups = newGroups }

let findNumMatches row =
    let mutable cache : Map<int * int * int, int64> = Map.empty

    let rec numMatches pos group currLength =
        let key = (pos, group, currLength)
        match cache |> Map.tryFind key with
        | Some n -> n
        | None ->
            if pos = row.Springs.Length then
                if (group = row.Groups.Length && currLength = 0) || (group = row.Groups.Length - 1 && currLength = Array.last row.Groups) then 1L else 0L
            else
            let matches =
                match row.Springs.[pos] with
                | Damaged ->
                    numMatches (pos + 1) group (currLength + 1)
                | Operational when currLength = 0 ->
                    numMatches (pos + 1) group 0
                | Operational ->
                    if group < row.Groups.Length && currLength = row.Groups.[group] then
                        numMatches (pos + 1) (group + 1) 0
                    else
                        0L
                | Unknown when currLength = 0 ->
                    (numMatches (pos + 1) group 1) +
                    (numMatches (pos + 1) group 0)
                | Unknown ->
                    if group < row.Groups.Length && currLength = row.Groups.[group] then
                        (numMatches (pos + 1) group (currLength + 1)) +
                        (numMatches (pos + 1) (group + 1) 0)
                    else
                        numMatches (pos + 1) group (currLength + 1)

            cache <- Map.add key matches cache
            matches
    
    numMatches 0 0 0

let part1() =
    let sumNumMatches =
        rows
        |> Array.map findNumMatches
        |> Array.sum

    printfn "Sum: %i" sumNumMatches

let part2() =
    let sumNumMatches =
        rows
        |> Array.map unfold
        |> Array.map findNumMatches
        |> Array.sum

    printfn "Sum: %i" sumNumMatches
