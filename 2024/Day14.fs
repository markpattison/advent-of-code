module Day14

open System
open System.IO

let input = File.ReadAllLines(@"input\day14.txt")

type Position ={ X: int; Y: int }
type Robot = { Pos: Position; Vel: Position }

let parseLine (s: string) =
    let parts = s.Split([| 'p'; '='; ','; ' '; 'v' |])
    let posX = parts.[2] |> Int32.Parse
    let posY = parts.[3] |> Int32.Parse
    let velX = parts.[6] |> Int32.Parse
    let velY = parts.[7] |> Int32.Parse
    { Pos = { X = posX; Y = posY }; Vel = { X = velX; Y = velY } }

let allRobots =
    input
    |> Array.map parseLine

let sizeX = 101
let sizeY = 103

let positionAfterMoves n robot =
    let x = (robot.Pos.X + n * (robot.Vel.X + sizeX)) % sizeX
    let y = (robot.Pos.Y + n * (robot.Vel.Y + sizeY)) % sizeY
    { X = x; Y = y }

let updatedPositionsAfterMoves n =
    allRobots
    |> Array.map (positionAfterMoves n)

let part1() =
    let updatedPositions = updatedPositionsAfterMoves 100
    
    let q1 = updatedPositions |> Array.where (fun r -> r.X < sizeX / 2 && r.Y < sizeY / 2) |> Array.length
    let q2 = updatedPositions |> Array.where (fun r -> r.X < sizeX / 2 && r.Y > sizeY / 2) |> Array.length
    let q3 = updatedPositions |> Array.where (fun r -> r.X > sizeX / 2 && r.Y < sizeY / 2) |> Array.length
    let q4 = updatedPositions |> Array.where (fun r -> r.X > sizeX / 2 && r.Y > sizeY / 2) |> Array.length

    let safetyFactor = q1 * q2 * q3 * q4

    printfn "Safety factor: %i " safetyFactor

let part2() =
    let mutable found = false
    let mutable moves = -1

    let robotCount = allRobots |> Array.length

    while not found do
        moves <- moves + 1
        let updatedPositions = updatedPositionsAfterMoves moves
        if updatedPositions |> Array.distinct |> Array.length = robotCount then found <- true
    
    // let updatedPositions = updatedPositionsAfterMoves moves
   
    // for y in 0 .. (sizeY - 1) do
    //     for x in 0 .. (sizeX - 1) do
    //         if updatedPositions |> Array.contains { X = x; Y = y } then printf "*" else printf "."
    //     printfn ""

    printfn "Moves: %i" moves
