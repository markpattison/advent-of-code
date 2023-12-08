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

// this works, but it's not exactly obvious why

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

// with cycle detector...

type Cycle =
    {
        StartTime: int64
        Length: int64
    }

type State = { Node: string; InstructionPointer: int }
let update state = { Node = navigate state.InstructionPointer state.Node; InstructionPointer = (state.InstructionPointer + 1) % instructions.Length }
let canCheckForCycle state = state.Node.EndsWith('Z')

let cycleDetector (initialState: 'a) (toCachedState: 'a -> 'b) (canCheckForCycle: 'a -> bool) update =
    let mutable time = 0L
    let mutable foundCycle = false
    let mutable state = initialState
    let mutable cache : Map<'b, int64> = Map.empty

    let mutable cycle = { StartTime = 0L; Length = 0L }

    while not foundCycle do
        let cachedState = toCachedState state
        if canCheckForCycle state then
            match Map.tryFind cachedState cache with
            | None ->
                cache <- cache.Add(cachedState, time)
                time <- time + 1L
                state <- update state
            | Some cycleStart ->
                cycle <- { StartTime = cycleStart; Length = time - cycleStart }
                foundCycle <- true
        else
            cache <- cache.Add(cachedState, time)
            time <- time + 1L
            state <- update state            
    
    cycle

let part2b() =
    let startingNodes =
        nodes.Keys
        |> Seq.filter (fun n -> n.EndsWith('A'))
        |> Seq.toArray

    startingNodes |>
    Array.iter (fun s ->
        let cycle = cycleDetector { Node = s; InstructionPointer = 0 } id canCheckForCycle update
        printfn "Cycle detected: start %i, length %i" cycle.StartTime cycle.Length)
