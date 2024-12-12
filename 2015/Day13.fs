module Day13

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day13.txt")

let parseLine (s: string) =
    let parts = s.Substring(0, s.Length - 1).Split(' ')
    let from = parts.[0]
    let nextTo = parts.[10]
    let happiness =
        match parts.[2] with
        | "gain" -> parts.[3] |> Int32.Parse
        | "lose" -> -1 * (parts.[3] |> Int32.Parse)
        | _ -> failwith "unexpected"
    
    (from, nextTo, happiness)

let allRows =
    input
    |> Array.map parseLine

let allNames =
    allRows
    |> Array.map (fun (from, _, _) -> from)
    |> Array.distinct

let numNames = allNames.Length

let happinessMap =
    allNames
    |> Array.map (fun name ->
        let internalMap =
            allRows
            |> Array.filter (fun (n, _, _) -> n = name)
            |> Array.map (fun (_, nextTo, happiness) -> (nextTo, happiness))
            |> Map.ofArray
        (name, internalMap))
    |> Map.ofArray

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

let score seatingPlan =
    seatingPlan
    |> Array.mapi (fun i name ->
        let prev = (i + numNames - 1) % numNames
        let next = (i + 1) % numNames
        happinessMap.[name].[seatingPlan.[prev]] + happinessMap.[name].[seatingPlan.[next]])
    |> Array.sum

let part1() =
    let permutations =
        allNames
        |> List.ofArray
        |> permute
        |> List.map (List.toArray)
    
    let maxHappiness =
        permutations
        |> List.map score
        |> List.max
    
    printfn "Max happiness: %i" maxHappiness

let score2 seatingPlan =
    seatingPlan
    |> Array.mapi (fun i name ->
        let prev = (i + numNames - 1) % numNames
        let next = (i + 1) % numNames
        (if i > 0 then happinessMap.[name].[seatingPlan.[prev]] else 0)
         + (if i < numNames - 1 then happinessMap.[name].[seatingPlan.[next]] else 0))
    |> Array.sum

let part2() =
    let permutations =
        allNames
        |> List.ofArray
        |> permute
        |> List.map (List.toArray)
    
    let maxHappiness =
        permutations
        |> List.map score2
        |> List.max
    
    printfn "Max happiness: %i" maxHappiness
