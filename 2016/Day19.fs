module Day19

open System
open System.IO
open FSharpx.Collections

let input =
    File.ReadAllLines(@"input\day19.txt").[0]

let numElves = Int32.Parse(input)

let part1() =
    let hasPresents = Array.create numElves true
    let mutable numRemaining  = numElves
    let mutable index = 0
    let mutable removeNext = false

    while numRemaining > 1 do
        if hasPresents.[index] then
            if removeNext then
                hasPresents.[index] <- false
                numRemaining <- numRemaining - 1
                removeNext <- false
            else
                removeNext <- true
        index <- (index + 1) % numElves
    
    let winningElf = 1 + (hasPresents |> Array.findIndex id)

    printfn "Winning elf: %i" winningElf

let part2() =
    let mutable firstHalf = seq { 1 .. numElves / 2 } |> Deque.ofSeq
    let mutable countFirstHalf = numElves / 2
    let mutable secondHalf = seq { 1 + numElves / 2 .. numElves } |> Deque.ofSeq
    let mutable countSecondHalf = numElves - countFirstHalf

    while countFirstHalf + countSecondHalf > 1 do
        let elf, newFirstHalf = Deque.uncons firstHalf
        firstHalf <- newFirstHalf
        countFirstHalf <- countFirstHalf - 1

        if countFirstHalf = countSecondHalf then
            let newFirstHalf2, _ = Deque.unconj firstHalf
            firstHalf <- newFirstHalf2
            countFirstHalf <- countFirstHalf - 1
        else
            let _, newSecondHalf = Deque.uncons secondHalf
            secondHalf <- newSecondHalf
            countSecondHalf <- countSecondHalf - 1
        
        secondHalf <- Deque.conj elf secondHalf
        countSecondHalf <- countSecondHalf + 1

        let switch, newSecondHalf2 = Deque.uncons secondHalf
        secondHalf <- newSecondHalf2
        countSecondHalf <- countSecondHalf - 1

        firstHalf <- Deque.conj switch firstHalf
        countFirstHalf <- countFirstHalf + 1

    let winningElf = Deque.head firstHalf
    printfn "Winning elf: %i" winningElf