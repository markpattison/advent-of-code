module Day7

open System.IO

let line = File.ReadAllLines(@"input\day7.txt").[0]

let positions = line.Split(',') |> Array.map System.Int32.Parse

let part1() =
    let ordered = positions |> Array.sort
    let median = ordered.[ordered.Length / 2]

    let fuel =
        positions
        |> Array.sumBy (fun p -> abs (p - median))

    printfn "Fuel needed %i" fuel

let part2() =
    let fuelForMove x y =
        let diff = abs (x - y)
        diff * (1 + diff) / 2

    let fuelNeeded moveTo =
        positions
        |> Array.sumBy (fun p -> fuelForMove p moveTo)

    let min = Array.min positions
    let max = Array.max positions

    let fuelCosts =
        [| min .. max |]
        |> Array.map (fun moveTo -> moveTo, fuelNeeded moveTo)

    let cheapestPosition, fuel = Array.minBy snd fuelCosts

    printfn "Fuel needed %i" fuel
