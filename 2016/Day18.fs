module Day18

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day18.txt").[0]

type Tile = Safe | Trap

let firstRow =
    input.ToCharArray()
    |> Array.map (fun c -> match c with | '.' -> Safe | '^' -> Trap | _ -> failwith "unexpected")

let threeTiles (row: Tile[]) i =
    let left = if i > 0 then row.[i - 1] else Safe
    let centre = row.[i]
    let right = if i < Array.length row - 1 then row.[i + 1] else Safe

    left, centre, right

let tile lcr =
    match lcr with
    | Trap, Trap, Safe -> Trap
    | Safe, Trap, Trap -> Trap
    | Trap, Safe, Safe -> Trap
    | Safe, Safe, Trap -> Trap
    | _ -> Safe

let nextRow row =
    Array.init (Array.length row) (threeTiles row >> tile)

let countSafeTiles initialRow numRows =
    let safeTilesInRow row =
        row |> Array.sumBy (fun tile -> if tile = Safe then 1 else 0)

    let rec safeTiles (acc, rowsLeft, lastRow) =
        if rowsLeft = 0 then
            acc
        else
            safeTiles (acc + safeTilesInRow lastRow, rowsLeft - 1, nextRow lastRow)
    
    safeTiles (0, numRows, initialRow)

let part1() =
    let safeTiles = countSafeTiles firstRow 40
    
    printfn "Safe tiles: %i" safeTiles

let part2() =
    let safeTiles = countSafeTiles firstRow 400000
    
    printfn "Safe tiles: %i" safeTiles
