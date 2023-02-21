module Day5

open System
open System.IO

let lines = File.ReadAllLines(@"input\day5.txt")

let separators = [| ","; " -> " |]

let vents =
    lines
    |> Array.map (fun s -> s.Split([| ","; " -> " |], StringSplitOptions.None))
    |> Array.map (Array.map System.Int32.Parse)
    |> Array.map (fun arr -> ((arr.[0], arr.[1]), (arr.[2], arr.[3])))

let howManyTwoOrMore arr =
    let mutable count = 0
    arr |> Array2D.iter (fun d -> if d >= 2 then count <- count + 1)
    count

let part1() =
    let isHorizontalOrVertical ((x1, y1), (x2, y2)) =
        x1 = x2 || y1 = y2

    let horizontalOrVerticalVents = vents |> Array.filter isHorizontalOrVertical

    let danger = Array2D.create 1000 1000 0

    horizontalOrVerticalVents
    |> Array.iter (fun ((x1, y1), (x2, y2)) ->
        if x1 = x2 then
            for y in (min y1 y2) .. (max y1 y2) do danger.[x1, y] <- danger.[x1, y] + 1
        else
            for x in (min x1 x2) .. (max x1 x2) do danger.[x, y1] <- danger.[x, y1] + 1
        )

    printfn "Points with overlap: %i" (howManyTwoOrMore danger)

let part2() =
    let danger = Array2D.create 1000 1000 0

    vents
    |> Array.iter (fun ((x1, y1), (x2, y2)) ->
        let xInc = sign (x2 - x1)
        let yInc = sign (y2 - y1)
        let distance = max (abs (x2 - x1)) (abs (y2 - y1))

        for i in 0 .. distance do danger.[x1 + i * xInc, y1 + i * yInc] <- danger.[x1 + i * xInc, y1 + i * yInc] + 1
        )

    printfn "Points with overlap: %i" (howManyTwoOrMore danger)
