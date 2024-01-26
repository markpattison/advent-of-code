module Day19

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day19.txt")

type Position = { X: int; Y: int; Z: int }
let add p1 p2 = { X = p1.X + p2.X; Y = p1.Y + p2.Y; Z = p1.Z + p2.Z }
let subtract p1 p2 = { X = p1.X - p2.X; Y = p1.Y - p2.Y; Z = p1.Z - p2.Z }

type Scanner =
    {
        ScannerId: int
        Beacons: Position list
    }

type Line = Scanner of int | Position of Position

let parseLine (s: string) =
    if String.IsNullOrWhiteSpace(s) then
        None
    elif s.StartsWith("---") then
        let scannerId = s.Substring(12, s.Length - 16) |> Int32.Parse
        Some (Scanner scannerId)
    else
        let coords = s.Split(',') |> Array.map Int32.Parse
        Some (Position { X = coords.[0]; Y = coords.[1]; Z = coords.[2] })

let scanners =
    let allLines = input |> Array.choose parseLine
    let mutable revScanners : Scanner list = []

    for i in 0 .. allLines.Length - 1 do
        match allLines.[i] with
        | Position _ -> ()
        | Scanner scannerId ->
            let mutable revPositions : Position list = []
            let mutable j = i + 1
            let mutable finished = false
            while j <= allLines.Length - 1 && not finished do
                match allLines.[j] with
                | Scanner _ ->
                    finished <- true
                | Position p ->
                    revPositions <- p :: revPositions
                    j <- j + 1
            revScanners <- { ScannerId = scannerId; Beacons = List.rev revPositions } :: revScanners

    List.rev revScanners

type Matrix =
    {
        M11: int
        M12: int
        M13: int
        M21: int
        M22: int
        M23: int
        M31: int
        M32: int
        M33: int
    }

let empty = { M11 = 0; M12 = 0; M13 = 0; M21 = 0; M22 = 0; M23 = 0; M31 = 0; M32 = 0; M33 = 0 }
let identity = { empty with M11 = 1; M22 = 1; M33 = 1 }
let rotX = { empty with M11 = 1; M23 = 1; M32 = -1 }
let rotY = { empty with M13 = -1; M22 = 1; M31 = 1 }
let rotZ = { empty with M12 = 1; M21 = -1; M33 = 1 }

let mul a b =
    {
        M11 = a.M11 * b.M11 + a.M12 * b.M21 + a.M13 * b.M31
        M12 = a.M11 * b.M12 + a.M12 * b.M22 + a.M13 * b.M32
        M13 = a.M11 * b.M13 + a.M12 * b.M23 + a.M13 * b.M33
        M21 = a.M21 * b.M11 + a.M22 * b.M21 + a.M23 * b.M31
        M22 = a.M21 * b.M12 + a.M22 * b.M22 + a.M23 * b.M32
        M23 = a.M21 * b.M13 + a.M22 * b.M23 + a.M23 * b.M33
        M31 = a.M31 * b.M11 + a.M32 * b.M21 + a.M33 * b.M31
        M32 = a.M31 * b.M12 + a.M32 * b.M22 + a.M33 * b.M32
        M33 = a.M31 * b.M13 + a.M32 * b.M23 + a.M33 * b.M33
    }

let mulPos m p =
    {
        X = m.M11 * p.X + m.M12 * p.Y + m.M13 * p.Z
        Y = m.M21 * p.X + m.M22 * p.Y + m.M23 * p.Z
        Z = m.M31 * p.X + m.M32 * p.Y + m.M33 * p.Z
    }

let det m =
    m.M11 * (m.M22 * m.M33 - m.M23 * m.M32)
    - m.M12 * (m.M21 * m.M33 - m.M23 * m.M31)
    + m.M13 * (m.M21 * m.M32 - m.M22 * m.M31)

let allOrientations =
    let mutable orientations = [ identity ]
    let mutable finished = false

    let allRots m = [ mul rotX m; mul rotY m; mul rotZ m ]

    while not finished do
        let possibleNew = orientations |> List.collect allRots
        let actuallyNew = possibleNew |> List.except orientations
        match actuallyNew with
        | [] -> finished <- true
        | _ -> orientations <- actuallyNew @ orientations
    
    orientations

let inverses =
    allOrientations
    |> List.map (fun m ->
        let inverse = allOrientations |> List.find (fun inv -> mul m inv = identity)
        m, inverse)
    |> Map.ofList

let checkMatches scanner1 scanner2 orientation =
    let inverse2 = inverses.[orientation]
    let transformedBeacons2 = scanner2.Beacons |> List.map (mulPos inverse2)

    let offset, overlap =
        List.allPairs scanner1.Beacons transformedBeacons2
        |> List.map (fun (p1, p2) -> subtract p1 p2)
        |> List.countBy id
        |> List.maxBy snd
    
    if overlap >= 12 then
        Some (orientation, offset)
    else
        None

let checkAllMatches scanner1 scanner2 =
    allOrientations
    |> List.tryPick (checkMatches scanner1 scanner2)

type LocatedScanner =
    {
        Scanner: Scanner
        OrientationFromScannerZero: Matrix
        OffsetFromScannerZero: Position
    }

let allLocatedScanners =
    let scannerZero =
        {
            Scanner = scanners.[0]
            OrientationFromScannerZero = identity
            OffsetFromScannerZero = { X = 0; Y = 0; Z = 0 }
        }
    let mutable locatedScanners = [ scannerZero ]
    let mutable unlocatedScanners = List.tail scanners

    while not (List.isEmpty unlocatedScanners) do
        List.allPairs locatedScanners unlocatedScanners |> List.iter (fun (located, unlocated) ->
            match checkAllMatches located.Scanner unlocated with
            | None -> ()
            | Some (orientation, offset) ->
                let orientationFromZero = mul orientation located.OrientationFromScannerZero
                let offsetFromZero = add located.OffsetFromScannerZero (mulPos inverses.[located.OrientationFromScannerZero] offset)
                locatedScanners <- { Scanner = unlocated; OrientationFromScannerZero = orientationFromZero; OffsetFromScannerZero = offsetFromZero } :: locatedScanners
                unlocatedScanners <- unlocatedScanners |> List.filter (fun s -> s <> unlocated)
            )
    
    locatedScanners |> List.distinct

let part1() =
    let allBeaconsFromScannerZero =
        allLocatedScanners
        |> List.collect (fun located ->
            let inverseOrientation = inverses.[located.OrientationFromScannerZero]
            located.Scanner.Beacons
            |> List.map (fun p -> add located.OffsetFromScannerZero (mulPos inverseOrientation p)))
        |> List.distinct
    
    printfn "Number of beacons: %i" (List.length allBeaconsFromScannerZero)

let part2() =
    let allDistances =
        List.allPairs allLocatedScanners allLocatedScanners
        |> List.map (fun (s1, s2) -> subtract s1.OffsetFromScannerZero s2.OffsetFromScannerZero)
        |> List.map (fun offset -> abs offset.X + abs offset.Y + abs offset.Z)
    
    let maxDistance = List.max allDistances

    printfn "Max distance: %i" maxDistance
