module Day23

open System
open System.IO
open System.Threading

let input =
    File.ReadAllLines(@"input\day23.txt")
//     """#.#####################
// #.......#########...###
// #######.#########.#.###
// ###.....#.>.>.###.#.###
// ###v#####.#v#.###.#.###
// ###.>...#.#.#.....#...#
// ###v###.#.#.#########.#
// ###...#.#.#.......#...#
// #####.#.#.#######.#.###
// #.....#.#.#.......#...#
// #.#####.#.#.#########v#
// #.#...#...#...###...>.#
// #.#.#v#######v###.###v#
// #...#.>.#...>.>.#.###.#
// #####v#.#.###v#.#.###.#
// #.....#...#...#.#.#...#
// #.#########.###.#.#.###
// #...###...#...#...#.###
// ###.###.#.###v#####v###
// #...#...#.#.>.>.#.>.###
// #.###.###.#.###.#.#v###
// #.....###...###...#...#
// #####################.#""".Split("\r\n")

type Dir = N | S | E | W
type Tile = Path | Forest | Slope of Dir

let parseChar c =
    match c with
    | '.' -> Path
    | '#' -> Forest
    | '^' -> Slope N
    | '>' -> Slope E
    | 'v' -> Slope S
    | '<' -> Slope W
    | _ -> failwith "unexpected"

let xLength = input.[0].Length
let yLength = input.Length

let tiles =
    Array2D.init xLength yLength (fun x y -> input.[y].[x] |> parseChar)

let xStart = tiles.[*, 0] |> Array.findIndex (fun t -> t = Path)
let yStart = 0

let yEnd = yLength - 1

type State = { X: int; Y: int; Unvisited: bool[,]; Steps: int }

let rec maxSteps state =
    let x, y = state.X, state.Y

    if y = yEnd then
        state.Steps
    else
        let nextPositions =
            match tiles.[x, y] with
            | Path -> seq {
                if x > 0 && tiles.[x - 1, y] <> Forest && state.Unvisited.[x - 1, y] then (x - 1, y)
                if x < xLength - 1 && tiles.[x + 1, y] <> Forest && state.Unvisited.[x + 1, y] then (x + 1, y)
                if y > 0 && tiles.[x, y - 1] <> Forest && state.Unvisited.[x, y - 1] then (x, y - 1)
                if y < yLength - 1 && tiles.[x, y + 1] <> Forest && state.Unvisited.[x, y + 1] then (x, y + 1) }
            | Slope N -> seq { if y > 0 && tiles.[x, y - 1] <> Forest && state.Unvisited.[x, y - 1] then (x, y - 1) }
            | Slope E -> seq { if x < xLength - 1 && tiles.[x + 1, y] <> Forest && state.Unvisited.[x + 1, y] then (x + 1, y) }
            | Slope S -> seq { if y < yLength - 1 && tiles.[x, y + 1] <> Forest && state.Unvisited.[x, y + 1] then (x, y + 1) }
            | Slope W -> seq { if x > 0 && tiles.[x - 1, y] <> Forest && state.Unvisited.[x - 1, y] then (x - 1, y) }
            | Forest -> failwith "unexpected"
        
        if Seq.isEmpty nextPositions then
            0
        else
            let newSteps = state.Steps + 1

            nextPositions
            |> Seq.map (fun (nx, ny) ->
                state.Unvisited.[x, y] <- false
                let ms = maxSteps { X = nx; Y = ny; Unvisited = state.Unvisited; Steps = newSteps }
                state.Unvisited.[x, y] <- true
                ms)
            |> Seq.max

let part1() =
    let initialState = { X = xStart; Y = yStart; Unvisited = Array2D.create xLength yLength true; Steps = 0 }

    let longestPath = maxSteps initialState

    printfn "Longest path: %i" longestPath

// let part2work() =
//     let mutable longest = 0

//     let rec maxSteps2 state =
//         let x, y = state.X, state.Y

//         if y = yEnd then
//             if state.Steps > longest then
//                 longest <- state.Steps
//                 printfn "%i" longest
//             state.Steps
//         else
//             let nextPositions =
//                 match tiles.[x, y] with
//                 | Path | Slope _ -> seq {
//                     if x > 0 && tiles.[x - 1, y] <> Forest && state.Unvisited.[x - 1, y] then (x - 1, y)
//                     if x < xLength - 1 && tiles.[x + 1, y] <> Forest && state.Unvisited.[x + 1, y] then (x + 1, y)
//                     if y > 0 && tiles.[x, y - 1] <> Forest && state.Unvisited.[x, y - 1] then (x, y - 1)
//                     if y < yLength - 1 && tiles.[x, y + 1] <> Forest && state.Unvisited.[x, y + 1] then (x, y + 1) }
//                 | Forest -> failwith "unexpected"
            
//             if Seq.isEmpty nextPositions then
//                 0
//             else
//                 let newSteps = state.Steps + 1

//                 nextPositions
//                 |> Seq.map (fun (nx, ny) ->
//                     state.Unvisited.[x, y] <- false
//                     let ms = maxSteps2 { X = nx; Y = ny; Unvisited = state.Unvisited; Steps = newSteps }
//                     state.Unvisited.[x, y] <- true
//                     ms)
//                 |> Seq.max
    
//     let initialState = { X = xStart; Y = yStart; Unvisited = Array2D.create xLength yLength true; Steps = 0 }

//     let longestPath = maxSteps2 initialState

//     printfn "Longest path: %i" longestPath

// let part2() =
//     let threadStart = new ThreadStart(fun _ -> part2work())
//     let thread = new Thread(threadStart, 16 * 1024 * 1024)
//     thread.SetApartmentState(ApartmentState.STA)
//     thread.Start()
//     thread.Join()

let part2() =
    let initialUnvisited =
        Array2D.init xLength yLength (fun x y -> tiles.[x, y] <> Forest)

    let initialPath = { X = xStart; Y = yStart; Unvisited = initialUnvisited; Steps = 0 }
    let mutable activePaths = [ initialPath ]

    let mutable longest = 0

    while not (List.isEmpty activePaths) do
        match activePaths with
        | [] -> failwith "unexpected"
        | path :: remPaths ->
            let x, y = path.X, path.Y
            if y = yEnd then
                longest <- max longest path.Steps
                printfn "%i, %i, %i" path.Steps (List.length activePaths) longest
                activePaths <- remPaths
            else
                let nextPositions = [
                    if x > 0 && path.Unvisited.[x - 1, y] then (x - 1, y)
                    if x < xLength - 1 && path.Unvisited.[x + 1, y] then (x + 1, y)
                    if y > 0 && path.Unvisited.[x, y - 1] then (x, y - 1)
                    if y < yLength - 1 && path.Unvisited.[x, y + 1] then (x, y + 1) ]
                match nextPositions with
                | [] ->
                    activePaths <- remPaths
                | [ (nx, ny) ] ->
                    path.Unvisited.[x, y] <- false
                    activePaths <- { path with X = nx; Y = ny; Steps = path.Steps + 1 } :: remPaths
                | mult ->
                    let newPaths =
                        mult
                        |> List.map (fun (nx, ny) ->
                                        let newUnvisited = Array2D.copy path.Unvisited
                                        newUnvisited.[x, y] <- false
                                        { X = nx; Y = ny; Unvisited = newUnvisited; Steps = path.Steps + 1})
                    activePaths <- List.append newPaths remPaths
    
    printfn "Longest path: %i" longest
