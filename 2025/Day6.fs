module Day6

open System
open System.IO

let input = 
    File.ReadAllLines(@"input\day6.txt")

type Op = Add | Mul

let operations =
    input.[4].Split(" ", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s2 -> match s2 with | "+" -> Add | "*" -> Mul | _ -> failwith "unexpected")

let size = operations.Length

let part1() =
    let numbers =
        input
        |> Array.take 4
        |> Array.map (fun s ->
            s.Split(" ", StringSplitOptions.RemoveEmptyEntries)
            |> Array.map Int64.Parse)
    
    let result i =
        match operations.[i] with
        | Add -> numbers.[0].[i] + numbers.[1].[i] + numbers.[2].[i] + numbers.[3].[i]
        | Mul -> numbers.[0].[i] * numbers.[1].[i] * numbers.[2].[i] * numbers.[3].[i]

    let total =
        [ 0 .. size - 1 ] |> List.sumBy result
    
    printfn "Total: %i" total

let part2() =
    let numbers =
        Array.init input.[0].Length (fun i -> [| input.[0].[i]; input.[1].[i]; input.[2].[i]; input.[3].[i] |])
        |> Array.map (fun chars -> match String chars with | "    " -> None | n -> Some (Int64.Parse(n)))
        |> Array.toList

    let rec problems thisSet previousSets remaining =
        match remaining with
        | [] -> thisSet :: previousSets
        | None :: xs -> problems [] (List.rev thisSet :: previousSets) xs
        | Some n :: xs -> problems (n :: thisSet) previousSets xs

    let allProblems = problems [] [] numbers |> List.rev |> List.toArray

    let result i =
        match operations.[i] with
        | Add -> List.fold (+) 0L allProblems.[i]
        | Mul -> List.fold (*) 1L allProblems.[i]

    let total =
        [ 0 .. size - 1 ] |> List.sumBy result
    
    printfn "Total: %i" total
