module Day14

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day14.txt").[0]

let toBytes (s: string) = s.ToCharArray() |> Array.map int
let toBinary (arr: int[]) = arr |> Array.map (fun a -> a.ToString("b8")) |> String.concat ""

let rows = Array.init 128 (fun i -> sprintf "%s-%i" input i |> toBytes |> Day10.denseHash |> toBinary)

let part1() =
    let used = rows |> Array.sumBy (Seq.filter ((=) '1') >> Seq.length)

    printfn "Used: %i" used

let part2() =
    let initialGrid = Array2D.init 128 128 (fun x y -> rows.[y].[x] = '1')

    let findUsed (grid: bool[,]) =
        seq {
            for x in 0 .. Array2D.length1 grid - 1 do
                for y in 0 .. Array2D.length2 grid - 1 do
                    yield x, y }
        |> Seq.tryFind (fun (x, y) -> grid.[x, y])
    
    let rec eraseRegion (grid: bool[,]) (x, y) =
        grid.[x, y] <- false
        if x > 0 && grid.[x - 1, y] then eraseRegion grid (x - 1, y)
        if x < Array2D.length1 grid - 1 && grid.[x + 1, y] then eraseRegion grid (x + 1, y)
        if y > 0 && grid.[x, y - 1] then eraseRegion grid (x, y - 1)
        if y < Array2D.length2 grid - 1 && grid.[x, y + 1] then eraseRegion grid (x, y + 1)

    let rec countRegions count grid =
        match findUsed grid with
        | None -> count
        | Some (x, y) ->
            eraseRegion grid (x, y)
            countRegions (count + 1) grid

    let numRegions = countRegions 0 initialGrid

    printfn "Regions: %i" numRegions
