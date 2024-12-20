module Day20

open System
open System.IO

let input = File.ReadAllLines(@"input\day20.txt")

let sizeX = input.[0].Length
let sizeY = input.Length

let walls, startX, startY, endX, endY =
    let mutable sx = 0
    let mutable sy = 0
    let mutable ex = 0
    let mutable ey = 0
    let w = Array2D.create sizeX sizeY false

    for x in 0 .. sizeX - 1 do
        for y in 0 .. sizeY - 1 do
            match input.[y].[x] with
            | '#' -> w.[x, y] <- true
            | 'S' ->
                sx <- x
                sy <- y
            | 'E' ->
                ex <- x
                ey <- y
            | '.' -> ()
            | _ -> failwith "unexpected"
    
    w, sx, sy, ex, ey

let minSteps (walls: bool[,]) =
    let dists = Array2D.create sizeX sizeY Int32.MaxValue
    dists.[startX, startY] <- 0

    let mutable unvisited =
        seq {
        for x in 0 .. sizeX - 1 do
            for y in 0 .. sizeY - 1 do
                if not walls.[x, y] then
                    if x = startX && y = startY then
                        (0, x, y)
                    else
                        (Int32.MaxValue, x, y)
        } |> Set.ofSeq
    
    let mutable finished = false

    while not finished do
        if Set.isEmpty unvisited then
            finished <- true
        else
            let (cDist, cx, cy) = Set.minElement unvisited
            if cDist = Int32.MaxValue then
                finished <- true
            else
                let neighbours =
                    seq {
                        if cx > 0 then (cx - 1, cy)
                        if cx < sizeX - 1 then (cx + 1, cy)
                        if cy > 0 then (cx, cy - 1)
                        if cy < sizeY - 1 then (cx, cy + 1)
                    } |> Seq.filter (fun (nx, ny) -> not walls.[nx, ny])
                let nDist = cDist + 1
                neighbours
                |> Seq.iter (fun (nx, ny) ->
                    let oldDist = dists.[nx, ny]
                    if nDist < oldDist then
                        unvisited <- Set.remove (oldDist, nx, ny) unvisited
                        unvisited <- Set.add (nDist, nx, ny) unvisited
                        dists.[nx, ny] <- nDist)
                unvisited <- Set.remove (cDist, cx, cy) unvisited
    
    dists

let origDists = minSteps walls

let part1() =
    let mutable cheatsSaving100 = 0
    for x in 1 .. sizeX - 2 do
        for y in 1 .. sizeY - 2 do
            if walls.[x, y] then
                let ew = if walls.[x - 1, y] || walls.[x + 1, y] then 0 else origDists.[x + 1, y] - origDists.[x - 1, y] - 2
                let we = if walls.[x - 1, y] || walls.[x + 1, y] then 0 else origDists.[x - 1, y] - origDists.[x + 1, y] - 2
                let ns = if walls.[x, y - 1] || walls.[x, y + 1] then 0 else origDists.[x, y + 1] - origDists.[x, y - 1] - 2
                let sn = if walls.[x, y - 1] || walls.[x, y + 1] then 0 else origDists.[x, y - 1] - origDists.[x, y + 1] - 2
                let saved = Array.max [| ew; we; ns; sn |]
                if saved >= 100 then cheatsSaving100 <- cheatsSaving100 + 1

    printfn "Cheats saving 100: %i" cheatsSaving100

let part2() =
    let mutable cheatsSaving100 = 0
    for x in 1 .. sizeX - 2 do
        for y in 1 .. sizeY - 2 do
            if not walls.[x, y] then
                for dx in -20 .. 20 do
                    let yRange = 20 - abs dx
                    for dy in -yRange .. yRange do
                        let cheatTime = abs dx + abs dy
                        let ex = x + dx
                        let ey = y + dy
                        if ex >= 1 && ex <= sizeX - 2 && ey >= 1 && ey <= sizeY - 2 && not walls.[ex, ey] then
                            let saved = origDists.[ex, ey] - origDists.[x, y] - cheatTime
                            if saved >= 100 then cheatsSaving100 <- cheatsSaving100 + 1
    
    printfn "Cheats saving 100: %i" cheatsSaving100
