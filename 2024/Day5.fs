module Day5

open System
open System.IO

let input = File.ReadAllLines(@"input\day5.txt")

let blankLine = input |> Array.findIndex (fun s -> s.Length = 0)

let pageOrderingRules =
    input
    |> Array.take blankLine
    |> Array.map (fun s ->
        let parts = s.Split("|") |> Array.map (Int32.Parse)
        (parts.[0], parts.[1]))

let updates =
    input
    |> Array.skip (blankLine + 1)
    |> Array.map (fun s ->
        s.Split(",") |> Array.map (Int32.Parse))

let isCorrect (update: int[]) =
    let mutable correct = true
    for i in 0 .. (update.Length - 2) do
        for j in (i + 1) .. (update.Length - 1) do
            let page1 = update.[i]
            let page2 = update.[j]
            if pageOrderingRules |> Array.contains (page1, page2) then
                ()
            else
                correct <- false
    correct

let part1() =
    let correctUpdates =
        updates
        |> Array.filter isCorrect
    
    let sumMiddles =
        correctUpdates
        |> Array.sumBy (fun update -> update.[(update.Length - 1) / 2])
    
    printfn "Sum: %i" sumMiddles

let rec fixUpdate (update: int[]) =
    if update.Length < 2 then
        update
    else
        let relevantRules =
            pageOrderingRules
            |> Array.filter (fun (p1, p2) -> Array.contains p1 update && Array.contains p2 update)
        let first =
            update
            |> Array.find (fun p ->
                let rules =
                    relevantRules
                    |> Array.filter (fun (p1, p2) -> p = p1 || p = p2)
                rules |> Array.forall (fun (p1, _) -> p = p1))
        Array.append [| first |] (fixUpdate (update |> Array.except [| first |]))

let part2() =
    let incorrectUpdates =
        updates
        |> Array.filter (isCorrect >> not)
    
    let fixedUpdates =
        incorrectUpdates
        |> Array.map fixUpdate
    
    let sumMiddles =
        fixedUpdates
        |> Array.sumBy (fun update -> update.[(update.Length - 1) / 2])
    
    printfn "Sum: %i" sumMiddles
