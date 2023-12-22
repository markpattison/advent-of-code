module Day22

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day22.txt")

type Location = { X: int; Y: int; Z: int }
type Dir = X | Y | Z

type Brick = { Start: Location; Dir: Dir; Length: int }

let parseLocation (s: string) =
    let splits = s.Split(",")
    { X = Int32.Parse(splits.[0]); Y = Int32.Parse(splits.[1]); Z = Int32.Parse(splits.[2]) }

let parseLine (s: string) =
    let splits = s.Split("~")
    let start, end' = parseLocation splits.[0], parseLocation splits.[1]
    let dir, length =
        if end'.X > start.X then
            X, end'.X - start.X
        elif end'.Y > start.Y then
            Y, end'.Y - start.Y
        elif end'.Z > start.Z then
            Z, end'.Z - start.Z
        elif start = end' then
            X, 0
        else
            failwith "unexpected"

    { Start = start; Dir = dir; Length = length }

let allBricks = input |> Array.map parseLine

let maxX = allBricks |> Seq.map (fun b -> b.Start.X + if b.Dir = X then b.Length else 0) |> Seq.max
let maxY = allBricks |> Seq.map (fun b -> b.Start.Y + if b.Dir = Y then b.Length else 0) |> Seq.max
let maxZ = allBricks |> Seq.map (fun b -> b.Start.Z + if b.Dir = Z then b.Length else 0) |> Seq.max

let dxyz dir = match dir with | X -> 1, 0, 0 | Y -> 0, 1, 0 | Z -> 0, 0, 1

let setBrick value (space: bool[,,])  b =
    let dx, dy, dz = dxyz b.Dir
    for i in 0 .. b.Length do
        space.[b.Start.X + i * dx, b.Start.Y + i * dy, b.Start.Z + i * dz] <- value

let fillBrick = setBrick true
let clearBrick = setBrick false

let getFilled bricks =
    let filled = Array3D.create (maxX + 1) (maxY + 1) (maxZ + 1) false
    bricks |> Array.iter (fillBrick filled)
    filled

let canBrickFall (space: bool[,,]) brick =
    let mutable blocked = false
    let mutable howFar = 0

    while not blocked do
        if brick.Dir = Z then
            if brick.Start.Z - howFar = 1 || space.[brick.Start.X, brick.Start.Y, brick.Start.Z - howFar - 1] then 
                blocked <- true
        else
            let dx, dy, _ = dxyz brick.Dir
            for j in 0 .. brick.Length do
                if brick.Start.Z - howFar = 1 || space.[brick.Start.X + j * dx, brick.Start.Y + j * dy, brick.Start.Z - howFar - 1] then
                    blocked <- true
        if not blocked then howFar <- howFar + 1
    
    if howFar = 0 then None else Some howFar

let findFallingBrick (space: bool[,,]) (bricks: Brick[]) =
    let mutable found = false
    let mutable fallIndex = 0
    let mutable howFar = 0

    let mutable i = 0

    while i < bricks.Length && not found do
        let brick = bricks.[i]
        match canBrickFall space brick with
        | Some hf ->
            found <- true
            fallIndex <- i
            howFar <- hf
        | None ->
            i <- i + 1
    
    if found then Some (fallIndex, howFar) else None

let dropBrick (space: bool[,,]) (bricks: Brick[]) fallIndex howFar =
    let brick = bricks.[fallIndex]
    clearBrick space brick
    let newBrick = { brick with Start = { bricks.[fallIndex].Start with Z = bricks.[fallIndex].Start.Z - howFar } }
    fillBrick space newBrick
    bricks.[fallIndex] <- newBrick

let settleBricks startBricks =
    let bricks = Array.copy startBricks
    let filled = getFilled bricks

    let mutable falling = true

    while falling do
        falling <- false
        match findFallingBrick filled bricks with
        | None -> ()
        | Some (fallIndex, howFar) ->
            falling <- true
            dropBrick filled bricks fallIndex howFar
    
    filled, bricks

let canBeDisintegrated (space: bool[,,]) (bricks: Brick[]) brickIndex =
    let brick = bricks.[brickIndex]
    clearBrick space brick

    let canAnyFall =
        match findFallingBrick space bricks with // careful, this includes disintegrated brick
        | Some _ -> true
        | None -> false

    fillBrick space brick // tidy up

    not canAnyFall

let settledSpace, settledBricks = settleBricks allBricks

let part1() =
    printfn ""
    let mutable canDisintegrate = 0
    for i in 0 .. settledBricks.Length - 1 do
        if canBeDisintegrated settledSpace settledBricks i then
            canDisintegrate <- canDisintegrate + 1

    printfn "Can disintegrate: %i" canDisintegrate

let howManyOthersFall (bricks: Brick[]) brickIndex =
    let newBricks = Array.removeAt brickIndex bricks

    let _, newSettledBricks = settleBricks newBricks

    Seq.zip newBricks newSettledBricks
    |> Seq.filter (fun (b1, b2) -> b1.Start.Z <> b2.Start.Z)
    |> Seq.length

let part2() =
    let howManyFall = Array.zeroCreate allBricks.Length

    System.Threading.Tasks.Parallel.For(0, allBricks.Length, fun i ->
        howManyFall.[i] <- howManyOthersFall settledBricks i) |> ignore

    let totalOthers = Array.sum howManyFall

    printfn "Total other bricks: %i" totalOthers
