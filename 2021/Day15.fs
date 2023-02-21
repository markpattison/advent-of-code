module Day15

open System.IO

let lines = File.ReadAllLines(@"input\day15.txt")

let testLines =
    [|
        "1163751742"
        "1381373672"
        "2136511328"
        "3694931569"
        "7463417111"
        "1319128137"
        "1359912421"
        "3125421639"
        "1293138521"
        "2311944581"
    |]

let toRisks (l: string[]) = 
    Array2D.init l.[0].Length l.Length (fun x y -> l.[y].Substring(x, 1) |> System.Int32.Parse)

let risks = lines |> toRisks

let totalRisk riskMatrix =

    let sizeX, sizeY = Array2D.length1 riskMatrix, Array2D.length2 riskMatrix

    let accRisk = Array2D.create sizeX sizeY 1000000000

    accRisk.[sizeX - 1, sizeY - 1] <- riskMatrix.[sizeX - 1, sizeY - 1]

    let mutable keepLooping = true

    while keepLooping do
        keepLooping <- false

        for x in sizeX - 1 .. -1 .. 0 do
            for y in sizeY - 1 .. -1 .. 0 do
                let currentAcc = accRisk.[x, y]
                let risk = riskMatrix.[x, y]
                if x > 0 && risk + accRisk.[x - 1, y] < currentAcc then accRisk.[x, y] <- risk + accRisk.[x - 1, y]; keepLooping <- true
                if x < sizeX - 1 && risk + accRisk.[x + 1, y] < currentAcc then accRisk.[x, y] <- risk + accRisk.[x + 1, y]; keepLooping <- true
                if y > 0 && risk + accRisk.[x, y - 1] < currentAcc then accRisk.[x, y] <- risk + accRisk.[x, y - 1]; keepLooping <- true
                if y < sizeY - 1 && risk + accRisk.[x, y + 1] < currentAcc then accRisk.[x, y] <- risk + accRisk.[x, y + 1]; keepLooping <- true

    accRisk.[0, 0] - riskMatrix.[0, 0]

let part1() =

    let total = totalRisk risks

    printfn "Lowest total risk: %i" total

let part2() =

    let sizeX, sizeY = Array2D.length1 risks, Array2D.length2 risks

    let bigRisks =
        Array2D.init (sizeX * 5) (sizeY * 5) (fun x y ->
            let modX, divX = x % sizeX, x / sizeX
            let modY, divY = y % sizeY, y / sizeY

            let adjRisk = risks.[modX, modY] + divX + divY

            if adjRisk > 9 then adjRisk - 9 else adjRisk)

    let total = totalRisk bigRisks

    printfn "Lowest total risk: %i" total
