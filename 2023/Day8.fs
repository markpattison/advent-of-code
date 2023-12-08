module Day8

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day8.txt")

type LeftRight = Left | Right

type Node = { LeftNode: string; RightNode: string }

let parseInstruction c =
    match c with
    | 'L' -> Left
    | 'R' -> Right
    | _ -> failwith "unexpected"

let instructions =
    input.[0].ToCharArray()
    |> Array.map parseInstruction

let parseNode (s: string) =
    let nodeName = s.Substring(0, 3)
    let left = s.Substring(7, 3)
    let right = s.Substring(12, 3)

    (nodeName, { LeftNode = left; RightNode = right })

let nodes =
    input
    |> Array.skip 2
    |> Array.map parseNode
    |> Map.ofArray

let navigate instructionPointer nodeName =
    let instruction = instructions.[instructionPointer % instructions.Length]
    let node = Map.find nodeName nodes

    match instruction with
    | Left -> node.LeftNode
    | Right -> node.RightNode

let part1() =
    let rec navigator movesSoFar node =
        match node with
        | "ZZZ" -> movesSoFar
        | _ ->
            let newNode = navigate movesSoFar node
            navigator (movesSoFar + 1) newNode

    let movesToEnd = navigator 0 "AAA"

    printfn "Moves to end: %i" movesToEnd

let rec gcd a b =
    if b = 0L then a else gcd b (a % b)

let lcm a b = a * b / (gcd a b)

let part2() =
    let rec navigator movesSoFar (node: string) =
        if node.EndsWith('Z') then
            movesSoFar
        else
            let newNode = navigate movesSoFar node
            navigator (movesSoFar + 1) newNode
    
    let startingNodes =
        nodes.Keys
        |> Seq.filter (fun n -> n.EndsWith('A'))
        |> Seq.toArray
    
    let allMovesToEnd = startingNodes |> Array.map (navigator 0)

    let movesToEnd =
        allMovesToEnd
        |> Array.map int64
        |> Array.reduce lcm
    
    printfn "Moves to end: %i" movesToEnd
