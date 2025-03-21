module Day11

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day11.txt")

type Item =
    | Microchip of string
    | Generator of string

let parseLine (s: string) =
    let parts = s.Replace(",", "").Replace(".", "").Split(' ')
    let n =
        match parts.[1] with
        | "first" -> 1
        | "second" -> 2
        | "third" -> 3
        | "fourth" -> 4
        | _ -> failwith "unexpected"
    
    let findIndicesBeforeWord word =
        parts
        |> Array.indexed
        |> Array.choose (fun (i, w) -> if w = word then Some (i - 1) else None)
    
    let generators =
        findIndicesBeforeWord "generator"
        |> Array.map (fun i -> parts.[i] |> Generator)
        |> Set.ofArray
    
    let microchips =
        findIndicesBeforeWord "microchip"
        |> Array.map (fun i -> parts.[i].Split('-').[0] |> Microchip)
        |> Set.ofArray

    
    let items = Set.union generators microchips

    n, items

let numFloors = input.Length

[<CustomComparison; CustomEquality>]
type State =
    {
        FloorContents: array<Set<Item>>
        Locations: int[]
        ElevatorLevel: int
    }
    interface IEquatable<State> with
        member this.Equals other = other.ElevatorLevel = this.ElevatorLevel && other.Locations = this.Locations
    override this.Equals other =
        match other with
        | :? State as s -> (this :> IEquatable<_>).Equals s
        | _ -> false
    override this.GetHashCode() = this.Locations.GetHashCode() + 19999 * this.ElevatorLevel
    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? State as s -> (this :> IComparable<_>).CompareTo s
            | _ -> -1
    interface IComparable<State> with
        member this.CompareTo other =
            if this.ElevatorLevel > other.ElevatorLevel then 1
            elif this.ElevatorLevel < other.ElevatorLevel then -1
            else compare this.Locations other.Locations

let toState elements floorContents elevatorLevel =
    let locations =
        elements
        |> Array.map (fun e ->
            floorContents
            |> Array.indexed
            |> Array.sumBy (fun (floor, fc) ->
                (if Set.contains (Microchip e) fc then 10 * (1 + floor) else 0)
                + (if Set.contains (Generator e) fc then (1 + floor) else 0)))
        |> Array.sort
    {
        FloorContents = floorContents
        Locations = locations
        ElevatorLevel = elevatorLevel
    }

let allElements contents =
    contents
    |> Array.collect (fun fc -> Set.toArray fc)
    |> Array.choose (fun item -> match item with | Generator element -> Some element | _ -> None)

let initialState =
    let contents : array<Set<Item>> = Array.create numFloors Set.empty

    input
    |> Array.map parseLine
    |> Array.iter (fun (floor, items) -> contents.[floor - 1] <- items)

    let elements = allElements contents

    toState elements contents 0

let isFinished state =
    let mutable finished = true
    for i in 0 .. numFloors - 2 do
        finished <- finished && Set.isEmpty state.FloorContents.[i]
    finished

let isFloorAllowable contents =
    let generators = contents |> Seq.choose (fun item -> match item with | Generator gen -> Some gen | _ -> None) |> Seq.toArray

    let vulnerableMicrochips =
        contents
        |> Seq.choose (fun item -> match item with | Microchip chip -> Some chip | _ -> None)
        |> Seq.filter (fun chip -> Array.contains chip generators |> not)
        |> Seq.toArray

    Array.isEmpty vulnerableMicrochips || Array.isEmpty generators

let isAllowable state =
    state.FloorContents
    |> Array.forall isFloorAllowable

let allowableMoves elements state =
    let currentFloor = state.FloorContents.[state.ElevatorLevel]
    let currentFloorArray = currentFloor |> Set.toArray

    if Set.isEmpty currentFloor then
        Set.empty
    else
        let singleCombinations =
            seq {
                for i in 0 .. currentFloorArray.Length - 1 do
                    yield set [ currentFloorArray.[i] ] } |> Seq.toArray
        
        let doubleCombinations =
            if currentFloorArray.Length > 1 then
                seq {
                for i in 0 .. currentFloorArray.Length - 1 do
                    for j in i + 1 .. currentFloorArray.Length - 1 do
                        let candidate = set [ currentFloorArray.[i]; currentFloorArray.[j] ]
                        if isFloorAllowable candidate then
                            yield candidate } |> Seq.toArray
            else
                [||]
        
        let moveToFloor newFloor combination =
            let floorContents =
                Array.init numFloors (fun i ->
                    if i = newFloor then
                        Set.union combination state.FloorContents.[newFloor]
                    elif i = state.ElevatorLevel then
                        Set.difference currentFloor combination
                    else
                        state.FloorContents.[i])

            toState elements floorContents newFloor
        
        let minRelevantFloor =
            if state.FloorContents.[0].IsEmpty then
                if state.FloorContents.[1].IsEmpty then
                    if state.FloorContents.[2].IsEmpty then
                        3
                    else
                        2
                else
                    1
            else
                0

        let doubleMovesUp =
            if state.ElevatorLevel < numFloors - 1 then
                doubleCombinations |> Array.map (moveToFloor (state.ElevatorLevel + 1)) |> Array.filter isAllowable
            else
                [||]
        
        let singleMovesUp =
            if state.ElevatorLevel < numFloors - 1 && Array.isEmpty doubleMovesUp then
                singleCombinations |> Array.map (moveToFloor (state.ElevatorLevel + 1)) |> Array.filter isAllowable
            else
                [||]

        let singleMovesDown =
            if state.ElevatorLevel > minRelevantFloor then
                singleCombinations |> Array.map (moveToFloor (state.ElevatorLevel - 1)) |> Array.filter isAllowable
            else
                [||]
        
        let doubleMovesDown =
            if state.ElevatorLevel > minRelevantFloor && Array.isEmpty singleMovesDown then
                doubleCombinations |> Array.map (moveToFloor (state.ElevatorLevel - 1)) |> Array.filter isAllowable
            else
                [||]

        let allowable =
            Seq.concat [| doubleMovesUp; singleMovesUp; singleMovesDown; doubleMovesDown |]
            |> Set.ofSeq
        
        allowable

let bfs state =
    let elements = allElements state.FloorContents

    (set [ state ], set [ state ])
    |> Seq.unfold (fun (nodesAtPrevDistance, visitedNodes) ->
        if Set.isEmpty nodesAtPrevDistance then
            None
        else
            let nextNodes =
                nodesAtPrevDistance
                |> Seq.map (fun n -> allowableMoves elements n |> set)
                |> Set.unionMany
                |> fun ns -> Set.difference ns visitedNodes
            let visitedNow = Set.union visitedNodes nextNodes
            Some (nodesAtPrevDistance, (nextNodes, visitedNow)))

let part1() =
    let minSteps =
        bfs initialState
        |> Seq.findIndex (fun states -> states |> Seq.exists isFinished)
    
    printfn "Minimum steps: %i" minSteps

let part2() =
    let newElements = set [ Generator "elerium"; Microchip "elerium"; Generator "dilithium"; Microchip "dilithium" ]
    let newFloorContents = initialState.FloorContents |> Array.mapi (fun i s -> if i = 0 then Set.union s newElements else s)
    let elements = allElements newFloorContents
    let newState = toState elements newFloorContents 0
    
    let minSteps =
        bfs newState
        |> Seq.findIndex (fun states -> states |> Seq.exists isFinished)
    
    printfn "Minimum steps: %i" minSteps
