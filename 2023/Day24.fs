module Day24

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day24.txt")

type Hailstone = { X: int64; Y: int64; Z: int64; vX: int64; vY: int64; vZ: int64 }

let parseTriple (s: string) =
    let splits = s.Split(",") |> Array.map (fun sp -> sp.Trim())

    (splits.[0] |> Int64.Parse, splits.[1] |> Int64.Parse, splits.[2] |> Int64.Parse)

let parseLine (s: string) =
    let splits = s.Split("@")
    let x, y, z = parseTriple splits.[0]
    let vx, vy, vz = parseTriple splits.[1]

    { X = x; Y = y; Z = z; vX = vx; vY = vy; vZ = vz}

let hailstones =
    input
    |> Array.map parseLine

let part1() =
    let minX = 200000000000000.0
    let minY = 200000000000000.0
    let maxX = 400000000000000.0
    let maxY = 400000000000000.0

    let allPairs =
        [|
            for i in 0 .. hailstones.Length - 2 do
                for j in i + 1 .. hailstones.Length - 1 do
                    yield (hailstones.[i], hailstones.[j])
        |]

    let findIntersection (h1, h2) =
        let denom1 = float h1.vX * float h2.vY - float h1.vY * float h2.vX
        let denom2 = float h2.vX * float h1.vY - float h2.vY * float h1.vX

        if denom1 = 0 || denom2 = 0 then
            // printfn "parallel"
            false
        else
            let t1 = (float h2.vX * (float h1.Y - float h2.Y) + float h2.vY * (float h2.X - float h1.X)) / denom1
            let t2 = (float h1.vX * (float h2.Y - float h1.Y) + float h1.vY * (float h1.X - float h2.X)) / denom2
            
            let xI = float h1.X + t1 * float h1.vX
            let yI = float h1.Y + t1 * float h1.vY
            
            let valid =
                if t1 < 0.0 && t2 < 0.0 then
                    // printfn "In past for both"
                    false
                elif t1 < 0.0 then
                    // printfn "In past for A"
                    false
                elif t2 < 0.0 then
                    // printfn "In past for B"
                    false
                elif xI < minX || xI > maxX || yI < minY || yI > maxY then
                    // printfn "Outside at %.3f, %.3f" xI yI
                    false
                else
                    // printfn "Intersect at %.3f, %.3f" xI yI
                    true
            valid
    
    let count =
        allPairs
        |> Array.map findIntersection
        |> Array.filter id
        |> Array.length
    
    printfn "Count: %i" count

    ()

let inline isClose a b = abs (a - b) < 0.5
let round (x: float) = int64 (Math.Round(x))

let findIntersection h1 h2 =
    let denom = h1.vX * h2.vY - h1.vY * h2.vX

    if denom = 0 then
        None
    else
        let t1 = (float h2.vX * (float h1.Y - float h2.Y) + float h2.vY * (float h2.X - float h1.X)) / float denom
        let t2 = (float h1.vX * (float h2.Y - float h1.Y) + float h1.vY * (float h1.X - float h2.X)) / float -denom
        
        if t1 >= 0.0 && t2 >= 0.0 then
            let zI1 = float h1.Z + t1 * float h1.vZ
            let zI2 = float h2.Z + t2 * float h2.vZ
            if isClose zI1 zI2 then
                Some (round (float h1.X + t1 * float h1.vX), round (float h1.Y + t1 * float h1.vY), round zI1)
            else
                None
        else
            None

let transform (vx: int64, vy: int64, vz: int64) hs =
    { hs with vX = hs.vX - vx; vY = hs.vY - vy; vZ = hs.vZ - vz }

let tryVelocity (targets: Hailstone[]) velocity =
    let transformed0 = transform velocity targets.[0]
    let transformed1 = transform velocity targets.[1]

    match findIntersection transformed0 transformed1 with
    | None -> None
    | Some candidate ->
        let mutable finished = false
        let mutable throwPosition : (int64 * int64 * int64) option = Some candidate
        let mutable i = 2

        while not finished do
            if i > targets.Length - 1 then
                finished <- true
            else
                let transformed = transform velocity targets.[i]
                if findIntersection transformed0 transformed = Some candidate then
                    i <- i + 1
                else
                    finished <- true
                    throwPosition <- None

        throwPosition

let part2() =
    let extent = 250L

    let velocities = seq {
        for x in -extent .. extent do
            for y in -extent .. extent do
                for z in -extent .. extent do
                    yield (x, y, z)
    }
    
    let throwPosition =
        velocities
        |> Seq.choose (tryVelocity hailstones)
        |> Seq.tryHead

    match throwPosition with
    | Some (x, y, z) -> printfn "%i" (x + y + z)
    | None -> printfn "not found"
