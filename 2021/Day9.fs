module Day9

open System.IO

let lines = File.ReadAllLines(@"input\day9.txt")

let numRows = lines |> Array.length
let numCols = lines.[0].Length

let heights = Array2D.init numCols numRows (fun x y -> lines.[y].Substring(x, 1) |> System.Int32.Parse)

let isLowPoint x y =
    let height = heights.[x, y]

    (x = 0           || heights.[x - 1, y] > height) &&
    (x = numCols - 1 || heights.[x + 1, y] > height) &&
    (y = 0           || heights.[x, y - 1] > height) &&
    (y = numRows - 1 || heights.[x, y + 1] > height)

let part1() =

    let risks = seq {
        for x in 0 .. numCols - 1 do
            for y in 0 .. numRows - 1 do
                match isLowPoint x y with
                | true -> heights.[x, y] + 1
                | false -> ()
    }

    let sumRisks = risks |> Seq.sum

    printfn "Total of risks: %i" sumRisks

let part2() =
    
    let lowPoints = seq {
        for x in 0 .. numCols - 1 do
            for y in 0 .. numRows - 1 do
                match isLowPoint x y with
                | true -> (x, y)
                | false -> ()
    }

    let basins =
        let inBasin : int[,] = Array2D.zeroCreate numCols numRows

        lowPoints |> Seq.iteri (fun i (x, y) -> inBasin.[x, y] <- i + 1)

        let mutable keepLooping = true

        while keepLooping do
            keepLooping <- false

            for x in 0 .. numCols - 1 do
                for y in 0 .. numRows - 1 do
                    let height = heights.[x, y]
                    let mutable basin = 0

                    if inBasin.[x, y] = 0 && height < 9 then
                        if x > 0           && heights.[x - 1, y] < height && inBasin.[x - 1, y] > 0 then basin <- inBasin.[x - 1, y]
                        if x < numCols - 1 && heights.[x + 1, y] < height && inBasin.[x + 1, y] > 0 then basin <- inBasin.[x + 1, y]
                        if y > 0           && heights.[x, y - 1] < height && inBasin.[x, y - 1] > 0 then basin <- inBasin.[x, y - 1]
                        if y < numRows - 1 && heights.[x, y + 1] < height && inBasin.[x, y + 1] > 0 then basin <- inBasin.[x, y + 1]
                    
                    if basin > 0 then
                        inBasin.[x, y] <- basin
                        keepLooping <- true

        inBasin

    let basinSizes =
        [ 1 .. Seq.length lowPoints ]
        |> List.map (fun i -> 
            let mutable size = 0
            for x in 0 .. numCols - 1 do
                for y in 0 .. numRows - 1 do
                    if basins.[x, y] = i then size <- size + 1
            size)

    let largestBasinSizes =
        basinSizes
        |> List.sortDescending
        |> List.take 3

    let product = Seq.fold (*) 1 largestBasinSizes

    printfn "Product of three largest basin sizes: %i" product
