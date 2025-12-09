module Day9

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day9.txt")

let redTiles =
    input
    |> Array.map (fun s ->
        let parts = s.Split(',') |> Array.map Int64.Parse
        parts.[0], parts.[1])

let size (i, j) =
    let x1, y1 = redTiles.[i]
    let x2, y2 = redTiles.[j]

    (1L + abs (x1 -  x2)) * (1L + abs (y1 - y2))

let part1() =
    let allPairs =
        seq {
            for i in 0 .. redTiles.Length - 2 do
                for j in i + 1 .. redTiles.Length - 1 do
                    yield i, j
        }

    let maxSize =
        allPairs |> Seq.map size |> Seq.max
    
    printfn "Max size: %i" maxSize

let part2() =

    // visualized in Excel first...

    let indexTop = 248
    let indexBottom = 249

    let yTop = snd redTiles.[indexTop]
    let yBottom = snd redTiles.[indexBottom]

    let anyIncluded (i, j) =
        let x1, y1 = redTiles.[i]
        let x2, y2 = redTiles.[j]
        redTiles |> Array.exists (fun (x, y) -> x >  min x1 x2 && x < max x1 x2 && y > min y1 y2 && y < max y1 y2)

    let maxSize =
        redTiles
        |> Array.mapi (fun i (_, y) ->
            if y >= yTop then
                if anyIncluded (i, indexTop) then 0L else size (i, indexTop)
            elif y <= yBottom then
                if anyIncluded (i, indexBottom) then 0L else size (i, indexBottom)
            else
                0L)
        |> Array.max
    
    printfn "Max size: %i" maxSize
