module Day11

open System.IO

let lines = File.ReadAllLines(@"input\day11.txt")

//let lines =
//    [|
//        "5483143223"
//        "2745854711"
//        "5264556173"
//        "6141336146"
//        "6357385478"
//        "4167524645"
//        "2176841721"
//        "6882881134"
//        "4846848554"
//        "5283751526"
//    |]

let numRows = lines.Length
let numCols = lines.[0].Length

let startEnergy = Array2D.init numCols numRows (fun x y -> lines.[y].Substring(x, 1) |> System.Int32.Parse)
    
let flashUpdate energy =
    let flashed = Array2D.create numCols numRows false
    let updated = Array2D.copy energy

    for x in 0 .. numCols - 1 do
        for y in 0 .. numRows - 1 do
            updated.[x, y] <- energy.[x, y] + 1

    let mutable keepLooping = true

    while keepLooping do
        keepLooping <- false

        for x in 0 .. numCols - 1 do
            for y in 0 .. numRows - 1 do
                if updated.[x, y] > 9 && not flashed.[x, y] then
                    keepLooping <- true
                    flashed.[x, y] <- true

                    for x' in (max 0 (x - 1)) .. (min (numCols - 1) (x + 1)) do
                        for y' in (max 0 (y - 1)) .. (min (numRows - 1) (y + 1)) do
                            updated.[x', y'] <- updated.[x', y'] + 1

    let mutable totalFlashes = 0

    for x in 0 .. numCols - 1 do
        for y in 0 .. numRows - 1 do        
            if flashed.[x, y] then
                totalFlashes <- totalFlashes + 1
                updated.[x, y] <- 0

    totalFlashes, updated

let rec results energy = seq {
    let flashes, updated = flashUpdate energy
    yield flashes
    yield! results updated
}

let part1() =

    let totalFlashes = results startEnergy |> Seq.take 100 |> Seq.sum

    printfn "Total flashes: %i" totalFlashes

let part2() =
    let firstAllFlash =
        results startEnergy
        |> Seq.findIndex (fun f -> f = numRows * numCols)

    printfn "First time all flash: %i" (firstAllFlash + 1)
