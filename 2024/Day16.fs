module Day16

open System
open System.IO

let input = File.ReadAllLines(@"input\day16.txt")

let sizeX = input.[0].Length
let sizeY = input.Length

let walls, startX, startY, endX, endY =
    let w = Array2D.create sizeX sizeY false
    let mutable sx = 0
    let mutable sy = 0
    let mutable ex = 0
    let mutable ey = 0
    for x in 0 .. sizeX - 1 do
        for y in 0 .. sizeY - 1 do
            match input.[y].[x] with
            | '.' -> ()
            | '#' -> w.[x, y] <- true
            | 'S' ->
                sx <- x
                sy <- y
            | 'E' ->
                ex <- x
                ey <- y
            | _ -> failwith "unexpected"
    w, sx, sy, ex, ey

let dxy dir =
    match dir with
    | 0 -> (0, -1)
    | 1 -> (1, 0)
    | 2 -> (0, 1)
    | 3 -> (-1, 0)
    | _ -> failwith "unexpected"

let distances, previous =
    let dists = Array3D.create sizeX sizeY 4 Int32.MaxValue
    dists.[startX, startY, 1] <- 0
    let prev : ((int * int * int) list) array3d = Array3D.create sizeX sizeY 4 []

    let mutable unvisited =
        seq {
        for x in 0 .. sizeX - 1 do
            for y in 0 .. sizeY - 1 do
                if not walls.[x, y] then
                    if x = startX && y = startY then
                        (Int32.MaxValue, x, y, 0)
                        (0, x, y, 1)
                        (Int32.MaxValue, x, y, 2)
                        (Int32.MaxValue, x, y, 3)
                    else
                        (Int32.MaxValue, x, y, 0)
                        (Int32.MaxValue, x, y, 1)
                        (Int32.MaxValue, x, y, 2)
                        (Int32.MaxValue, x, y, 3)
        } |> Set.ofSeq

    let mutable finished = false

    while not finished do
        if Set.isEmpty unvisited then
            finished <- true
        else
            let (cDist, cx, cy, cDir) = Set.minElement unvisited
            if cDist = Int32.MaxValue then
                finished <- true
            else
                let neighbours =
                    seq {
                        let dx, dy = dxy cDir
                        if not walls.[cx + dx, cy + dy] then
                            (cDist + 1, cx + dx, cy + dy, cDir)
                        (cDist + 1000, cx, cy, (cDir + 1) % 4)
                        (cDist + 1000, cx, cy, (cDir + 3) % 4)  
                    }
                neighbours
                |> Seq.iter (fun (nDist, nx, ny, nDir) ->
                    let oldDist = dists.[nx, ny, nDir]
                    if nDist < oldDist then
                        unvisited <- Set.remove (oldDist, nx, ny, nDir) unvisited
                        unvisited <- Set.add (nDist, nx, ny, nDir) unvisited
                        dists.[nx, ny, nDir] <- nDist
                    if nDist <= oldDist then
                        prev.[nx, ny, nDir] <- (cx, cy, cDir) :: prev.[nx, ny, nDir])
                unvisited <- Set.remove (cDist, cx, cy, cDir) unvisited
    dists, prev

let minDist =
    [ distances.[endX, endY, 0]
      distances.[endX, endY, 1]
      distances.[endX, endY, 2]
      distances.[endX, endY, 3] ] |> List.min

let part1() =
    printfn "Minimum score: %i" minDist

let part2() =
    let onPath = Array2D.create sizeX sizeY false
    let rec markPath (x, y, dir) =
        onPath.[x, y] <- true
        previous.[x, y, dir]
        |> List.iter markPath
    
    for dir in 0 .. 3 do
        if distances.[endX, endY, dir] = minDist then markPath (endX, endY, dir)

    let mutable count = 0
    for x in 0 .. sizeX - 1 do
        for y in 0 .. sizeY - 1 do
            if onPath.[x, y] then count <- count + 1
    
    printfn "On best path: %i" count
