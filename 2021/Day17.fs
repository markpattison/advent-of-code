module Day17

open System.IO

let line = File.ReadAllLines(@"input\day17.txt").[0]

let testLine = "target area: x=20..30, y=-10..-5"

let xyText = line.Substring(15).Split(", y=")

let toRange (s: string) =
    let xy = s.Split("..")
    let x = xy.[0] |> System.Int32.Parse
    let y = xy.[1] |> System.Int32.Parse

    (x, y)

let minX, maxX = toRange xyText.[0]
let minY, maxY = toRange xyText.[1]

type Status = Success | Failure | InFlight | Finished
type State = { X: int; Y: int; Dx: int; Dy: int; Time: int; Status: Status }

let toInitialState dx dy = { X = 0; Y = 0; Dx = dx; Dy = dy; Time = 0; Status = InFlight }

let update state =
    let x = state.X + state.Dx
    let y = state.Y + state.Dy
    let dx = state.Dx - sign state.Dx
    let dy = state.Dy - 1

    {
        X = x
        Y = y
        Dx = dx
        Dy = dy
        Time = state.Time + 1
        Status =
            if state.Status <> InFlight then Finished
            elif x >= minX && x <= maxX && y >= minY && y <= maxY then Success
            elif state.Y < minY && state.Dy < 0 then Failure
            else InFlight
    }

let updateForUnfold state =
    let updated = update state
    if state.Status = Finished then
        None
    else
        Some (state, updated)

let trajectory dx dy =
    toInitialState dx dy
    |> Array.unfold updateForUnfold

if minX <= 0 then failwith "assumed minX > 0"
if maxY > 0 then failwith "assumed maxY <= 0"

let maxPossibleDx = maxX // can't shoot past target area on first step
let minPossibleDx = 1 // must shoot forward as assumed minX > 0
    
let minPossibleDy = minY // can't shoot past target area on first step
let maxPossibleDy = 1000 // ??? yucky guess

let part1() =

    let mutable maxSuccessfulY = 0

    for dx in minPossibleDx .. maxPossibleDx do
        for dy in minPossibleDy .. maxPossibleDy do
            let traj = trajectory dx dy
            if (Array.last traj).Status = Success then
                let maxY = (Array.maxBy (fun state -> state.Y) traj).Y
                if maxY > maxSuccessfulY then maxSuccessfulY <- maxY

    printfn "Max Y: %i" maxSuccessfulY

let part2() =

    let mutable numSuccessful = 0

    for dx in minPossibleDx .. maxPossibleDx do
        for dy in minPossibleDy .. maxPossibleDy do
            let traj = trajectory dx dy
            if (Array.last traj).Status = Success then
                numSuccessful <- numSuccessful + 1

    printfn "Number of velocities: %i" numSuccessful
