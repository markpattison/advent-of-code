module Day6

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day6.txt").[0]

let initialBlocks =
    input.Split("\t")
    |> Array.map Int32.Parse

let redistribute blocks =
    let highest = Array.max blocks
    let indexToRedistribute = Array.findIndex (fun m -> m = highest) blocks
    let blocksToRedistribute = blocks.[indexToRedistribute]

    let newBlocks = Array.copy blocks

    newBlocks.[indexToRedistribute] <- 0

    for i in 1 .. blocksToRedistribute do
        newBlocks.[(indexToRedistribute + i) % blocks.Length] <- newBlocks.[(indexToRedistribute + i) % blocks.Length] + 1
    
    newBlocks

let redistributeUntilSeen startBlocks =
    let mutable seen : Set<int[]> = Set.empty

    let rec loop stepsSoFar blocks =
        if Set.contains blocks seen then
            stepsSoFar, blocks
        else
            seen <- Set.add blocks seen
            let newBlocks = redistribute blocks
            loop (stepsSoFar + 1) newBlocks
    
    loop 0 startBlocks    

let numRepeat, blocksRepeat = redistributeUntilSeen initialBlocks

let part1() =
    printfn "Number of redistributions: %i" numRepeat

let part2() =
    let nextRepeat = redistributeUntilSeen blocksRepeat |> fst

    printfn "Number of redistributions in cycle: %i" nextRepeat
