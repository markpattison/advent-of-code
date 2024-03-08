module Day23

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day23.txt")

let parseSpace c =
    match c with
    | '.' -> 0
    | 'A' -> 1
    | 'B' -> 2
    | 'C' -> 3
    | 'D' -> 4
    | _ -> failwith "unexpected"

type State =
    {
        Hall: int[] // always 7 spaces, left-to-right
        Rooms: int[][] // ignore, A, B, C, D then 4, bottom-to-top
        LastMovedToHall: int option
        EnergyUsed: int
    }

let energyPerStep = [| 0; 1; 10; 100; 1000 |]

let stepsFromHallToRoomEntrance hallPos room =
    match hallPos, room with
    | 0, 1 -> 2
    | 0, 2 -> 4
    | 0, 3 -> 6
    | 0, 4 -> 8
    | 1, 1 -> 1
    | 1, 2 -> 3
    | 1, 3 -> 5
    | 1, 4 -> 7
    | 2, 1 -> 1
    | 2, 2 -> 1
    | 2, 3 -> 3
    | 2, 4 -> 5
    | 3, 1 -> 3
    | 3, 2 -> 1
    | 3, 3 -> 1
    | 3, 4 -> 3
    | 4, 1 -> 5
    | 4, 2 -> 3
    | 4, 3 -> 1
    | 4, 4 -> 1
    | 5, 1 -> 7
    | 5, 2 -> 5
    | 5, 3 -> 3
    | 5, 4 -> 1
    | 6, 1 -> 8
    | 6, 2 -> 6
    | 6, 3 -> 4
    | 6, 4 -> 2
    | _ -> failwith "unexpected"

let stepsFromRoomEntranceToRoomEntrance room1 room2 = 2 * abs (room1 - room2)

let startStatePart1 (lines: string[]) =
    let populateRow room row =
        if row <= 1 then room else lines.[5 - row].[1 + 2 * room] |> parseSpace
    
    {
        Hall = Array.create 7 0
        Rooms = Array.init 5 (fun room -> if room = 0 then Array.create 4 0 else Array.init 4 (populateRow room))
        LastMovedToHall = None
        EnergyUsed = 0
    }

let startStatePart2 (lines: string[]) =
    let populateRow room row =
        if row = 0 then
            lines.[3].[1 + 2 * room] |> parseSpace
        elif row = 1 then
            "  #D#B#A#C#".[1 + 2 * room] |> parseSpace
        elif row = 2 then
            "  #D#C#B#A#".[1 + 2 * room] |> parseSpace
        elif row = 3 then
            lines.[2].[1 + 2 * room] |> parseSpace
        else
            failwith "unexpected"

    {
        Hall = Array.create 7 0
        Rooms = Array.init 5 (fun room -> if room = 0 then Array.create 4 0 else Array.init 4 (populateRow room))
        LastMovedToHall = None
        EnergyUsed = 0
    }

let canMoveIntoRoom state podType =
    match state.Rooms.[podType] with
    | [| 0; 0; 0; 0 |] -> Some 0
    | [| p; 0; 0; 0 |] when p = podType -> Some 1
    | [| p1; p2; 0; 0 |] when p1 = podType && p2 = podType -> Some 2
    | [| p1; p2; p3; 0 |] when p1 = podType && p2 = podType && p3 = podType -> Some 3
    | _ -> None

let canMoveFromRoom state room = // returns podType, roomPos
    match state.Rooms.[room] with
    | [| 0; 0; 0; 0 |] -> None
    | [| p1; 0; 0; 0 |] when p1 <> room -> Some (p1, 0)
    | [| _; 0; 0; 0 |] -> None
    | [| p1; p2; 0; 0 |] when p1 = room && p2 = room -> None
    | [| _; p2; 0; 0 |] -> Some (p2, 1)
    | [| p1; p2; p3; 0 |] when p1 = room && p2 = room && p3 = room -> None
    | [| _; _; p3; 0 |] -> Some (p3, 2)
    | [| p1; p2; p3; p4 |] when p1 = room && p2 = room && p3 = room && p4 = room -> None
    | [| _; _; _; p4 |] -> Some (p4, 3)
    | _ -> None

let hallToRoom hallPos room roomPos state =
    let podType = room
    let steps = stepsFromHallToRoomEntrance hallPos room + 4 - roomPos
    {
        Hall = Array.init 7 (fun h -> if h = hallPos then 0 else state.Hall.[h])
        Rooms = Array.init 5 (fun r -> if r = room then (Array.init 4 (fun row -> if row = roomPos then podType else state.Rooms.[r].[row])) else state.Rooms.[r])
        LastMovedToHall = None
        EnergyUsed = state.EnergyUsed + steps * energyPerStep.[podType]
    }

let roomToHall hallPos room roomPos podType state =
    let steps = stepsFromHallToRoomEntrance hallPos room + 4 - roomPos
    {
        Hall = Array.init 7 (fun h -> if h = hallPos then podType else state.Hall.[h])
        Rooms = Array.init 5 (fun r -> if r = room then (Array.init 4 (fun row -> if row = roomPos then 0 else state.Rooms.[r].[row])) else state.Rooms.[r])
        LastMovedToHall = Some hallPos
        EnergyUsed = state.EnergyUsed + steps * energyPerStep.[podType]
    }

let roomToRoom roomFrom roomPosFrom roomTo roomPosTo state =
    let podType = roomTo
    let steps = stepsFromRoomEntranceToRoomEntrance roomFrom roomTo + (4 - roomPosFrom) + (4 - roomPosTo)
    {
        Hall = state.Hall
        Rooms = Array.init 5 (fun r -> if r = roomFrom then (Array.init 4 (fun row -> if row = roomPosFrom then 0 else state.Rooms.[r].[row])) elif r = roomTo then (Array.init 4 (fun row -> if row = roomPosTo then podType else state.Rooms.[r].[row])) else state.Rooms.[r])
        LastMovedToHall = None
        EnergyUsed = state.EnergyUsed + steps * energyPerStep.[podType]
    }

let possibleMoveToRoom state = seq {
    // from Hall 0
    if state.LastMovedToHall <> Some 0 && state.Hall.[0] <> 0 then
        match state.Hall.[0] with
        | 1 -> if state.Hall.[1] = 0 then
                match canMoveIntoRoom state 1 with
                | None -> ()
                | Some r -> hallToRoom 0 1 r state
        | 2 -> if state.Hall.[1] = 0 && state.Hall.[2] = 0 then
                match canMoveIntoRoom state 2 with
                | None -> ()
                | Some r -> hallToRoom 0 2 r state
        | 3 -> if state.Hall.[1] = 0 && state.Hall.[2] = 0 && state.Hall.[3] = 0 then
                match canMoveIntoRoom state 3 with
                | None -> ()
                | Some r -> hallToRoom 0 3 r state
        | 4 -> if state.Hall.[1] = 0 && state.Hall.[2] = 0 && state.Hall.[3] = 0 && state.Hall.[4] = 0 then
                match canMoveIntoRoom state 4 with
                | None -> ()
                | Some r -> hallToRoom 0 4 r state
        | _ -> ()
    
    // from Hall 1
    if state.LastMovedToHall <> Some 1 && state.Hall.[1] <> 0 then
        match state.Hall.[1] with
        | 1 ->
                match canMoveIntoRoom state 1 with
                | None -> ()
                | Some r -> hallToRoom 1 1 r state
        | 2 -> if state.Hall.[2] = 0 then
                match canMoveIntoRoom state 2 with
                | None -> ()
                | Some r -> hallToRoom 1 2 r state
        | 3 -> if state.Hall.[2] = 0 && state.Hall.[3] = 0 then
                match canMoveIntoRoom state 3 with
                | None -> ()
                | Some r -> hallToRoom 1 3 r state
        | 4 -> if state.Hall.[2] = 0 && state.Hall.[3] = 0 && state.Hall.[4] = 0 then
                match canMoveIntoRoom state 4 with
                | None -> ()
                | Some r -> hallToRoom 1 4 r state
        | _ -> ()
    
    // from Hall 2
    if state.LastMovedToHall <> Some 2 && state.Hall.[2] <> 0 then
        match state.Hall.[2] with
        | 1 ->
                match canMoveIntoRoom state 1 with
                | None -> ()
                | Some r -> hallToRoom 2 1 r state
        | 2 ->
                match canMoveIntoRoom state 2 with
                | None -> ()
                | Some r -> hallToRoom 2 2 r state
        | 3 -> if state.Hall.[3] = 0 then
                match canMoveIntoRoom state 3 with
                | None -> ()
                | Some r -> hallToRoom 2 3 r state
        | 4 -> if state.Hall.[3] = 0 && state.Hall.[4] = 0 then
                match canMoveIntoRoom state 4 with
                | None -> ()
                | Some r -> hallToRoom 2 4 r state
        | _ -> ()
    
    // from Hall 3
    if state.LastMovedToHall <> Some 3 && state.Hall.[3] <> 0 then
        match state.Hall.[3] with
        | 1 -> if state.Hall.[2] = 0 then
                match canMoveIntoRoom state 1 with
                | None -> ()
                | Some r -> hallToRoom 3 1 r state
        | 2 ->
                match canMoveIntoRoom state 2 with
                | None -> ()
                | Some r -> hallToRoom 3 2 r state
        | 3 ->
                match canMoveIntoRoom state 3 with
                | None -> ()
                | Some r -> hallToRoom 3 3 r state
        | 4 -> if state.Hall.[4] = 0 then
                match canMoveIntoRoom state 4 with
                | None -> ()
                | Some r -> hallToRoom 3 4 r state
        | _ -> ()
    
    // from Hall 4
    if state.LastMovedToHall <> Some 4 && state.Hall.[4] <> 0 then
        match state.Hall.[4] with
        | 1 -> if state.Hall.[2] = 0 && state.Hall.[3] = 0 then
                match canMoveIntoRoom state 1 with
                | None -> ()
                | Some r -> hallToRoom 4 1 r state
        | 2 -> if state.Hall.[3] = 0 then
                match canMoveIntoRoom state 2 with
                | None -> ()
                | Some r -> hallToRoom 4 2 r state
        | 3 ->
                match canMoveIntoRoom state 3 with
                | None -> ()
                | Some r -> hallToRoom 4 3 r state
        | 4 ->
                match canMoveIntoRoom state 4 with
                | None -> ()
                | Some r -> hallToRoom 4 4 r state
        | _ -> ()
    
    // from Hall 5
    if state.LastMovedToHall <> Some 5 && state.Hall.[5] <> 0 then
        match state.Hall.[5] with
        | 1 -> if state.Hall.[2] = 0 && state.Hall.[3] = 0 && state.Hall.[4] = 0 then
                match canMoveIntoRoom state 1 with
                | None -> ()
                | Some r -> hallToRoom 5 1 r state
        | 2 -> if state.Hall.[3] = 0 && state.Hall.[4] = 0 then
                match canMoveIntoRoom state 2 with
                | None -> ()
                | Some r -> hallToRoom 5 2 r state
        | 3 -> if state.Hall.[4] = 0 then
                match canMoveIntoRoom state 3 with
                | None -> ()
                | Some r -> hallToRoom 5 3 r state
        | 4 ->
                match canMoveIntoRoom state 4 with
                | None -> ()
                | Some r -> hallToRoom 5 4 r state
        | _ -> ()
    
    // from Hall 6
    if state.LastMovedToHall <> Some 6 && state.Hall.[6] <> 0 then
        match state.Hall.[6] with
        | 1 -> if state.Hall.[2] = 0 && state.Hall.[3] = 0 && state.Hall.[4] = 0 && state.Hall.[5] = 0 then
                match canMoveIntoRoom state 1 with
                | None -> ()
                | Some r -> hallToRoom 6 1 r state
        | 2 -> if state.Hall.[3] = 0 && state.Hall.[4] = 0 && state.Hall.[5] = 0 then
                match canMoveIntoRoom state 2 with
                | None -> ()
                | Some r -> hallToRoom 6 2 r state
        | 3 -> if state.Hall.[4] = 0 && state.Hall.[5] = 0 then
                match canMoveIntoRoom state 3 with
                | None -> ()
                | Some r -> hallToRoom 6 3 r state
        | 4 -> if state.Hall.[5] = 0 then
                match canMoveIntoRoom state 4 with
                | None -> ()
                | Some r -> hallToRoom 6 4 r state
        | _ -> ()
    
    // from Room 1
    match canMoveFromRoom state 1 with
        | None -> ()
        | Some (podType, roomPos) ->
            match canMoveIntoRoom state podType with
            | Some roomPosTo when podType = 2 && state.Hall.[2] = 0 ->                                              roomToRoom 1 roomPos 2 roomPosTo state
            | Some roomPosTo when podType = 3 && state.Hall.[2] = 0 && state.Hall.[3] = 0 ->                        roomToRoom 1 roomPos 3 roomPosTo state
            | Some roomPosTo when podType = 4 && state.Hall.[2] = 0 && state.Hall.[3] = 0 && state.Hall.[4] = 0 ->  roomToRoom 1 roomPos 4 roomPosTo state
            | _ -> ()
    
    // from Room 2
    match canMoveFromRoom state 2 with
        | None -> ()
        | Some (podType, roomPos) ->
            match canMoveIntoRoom state podType with
            | Some roomPosTo when podType = 1 && state.Hall.[2] = 0 ->                        roomToRoom 2 roomPos 1 roomPosTo state
            | Some roomPosTo when podType = 3 && state.Hall.[3] = 0 ->                        roomToRoom 2 roomPos 3 roomPosTo state
            | Some roomPosTo when podType = 4 && state.Hall.[3] = 0 && state.Hall.[4] = 0 ->  roomToRoom 2 roomPos 4 roomPosTo state
            | _ -> ()
    
    // from Room 3
    match canMoveFromRoom state 3 with
        | None -> ()
        | Some (podType, roomPos) ->
            match canMoveIntoRoom state podType with
            | Some roomPosTo when podType = 1 && state.Hall.[2] = 0 && state.Hall.[3] = 0 -> roomToRoom 3 roomPos 1 roomPosTo state
            | Some roomPosTo when podType = 2 && state.Hall.[3] = 0 ->                       roomToRoom 3 roomPos 2 roomPosTo state
            | Some roomPosTo when podType = 4 && state.Hall.[4] = 0 ->                       roomToRoom 3 roomPos 4 roomPosTo state
            | _ -> ()
    
    // from Room 4
    match canMoveFromRoom state 4 with
        | None -> ()
        | Some (podType, roomPos) ->
            match canMoveIntoRoom state podType with
            | Some roomPosTo when podType = 1 && state.Hall.[2] = 0 && state.Hall.[3] = 0 && state.Hall.[4] = 0 -> roomToRoom 4 roomPos 1 roomPosTo state
            | Some roomPosTo when podType = 2 && state.Hall.[3] = 0 && state.Hall.[4] = 0 ->                       roomToRoom 4 roomPos 2 roomPosTo state
            | Some roomPosTo when podType = 3 && state.Hall.[4] = 0 ->                                             roomToRoom 4 roomPos 3 roomPosTo state
            | _ -> ()
}

let possibleOtherMoves state = seq {
    // from Room 1
    match canMoveFromRoom state 1 with
        | None -> ()
        | Some (podType, roomPos) ->
            if state.Hall.[0] = 0 && state.Hall.[1] = 0 then
                roomToHall 0 1 roomPos podType state
            if state.Hall.[1] = 0 then
                roomToHall 1 1 roomPos podType state
            if state.Hall.[2] = 0 then
                roomToHall 2 1 roomPos podType state
            if state.Hall.[2] = 0 && state.Hall.[3] = 0 then
                roomToHall 3 1 roomPos podType state
            if state.Hall.[2] = 0 && state.Hall.[3] = 0 && state.Hall.[4] = 0 then
                roomToHall 4 1 roomPos podType state
            if state.Hall.[2] = 0 && state.Hall.[3] = 0 && state.Hall.[4] = 0 && state.Hall.[5] = 0 then
                roomToHall 5 1 roomPos podType state
            if state.Hall.[2] = 0 && state.Hall.[3] = 0 && state.Hall.[4] = 0 && state.Hall.[5] = 0 && state.Hall.[6] = 0 then
                roomToHall 6 1 roomPos podType state
    
    // from Room 2
    match canMoveFromRoom state 2 with
        | None -> ()
        | Some (podType, roomPos) ->
            if state.Hall.[0] = 0 && state.Hall.[1] = 0 && state.Hall.[2] = 0 then
                roomToHall 0 2 roomPos podType state
            if state.Hall.[1] = 0 && state.Hall.[2] = 0 then
                roomToHall 1 2 roomPos podType state
            if state.Hall.[2] = 0 then
                roomToHall 2 2 roomPos podType state
            if state.Hall.[3] = 0 then
                roomToHall 3 2 roomPos podType state
            if state.Hall.[3] = 0 && state.Hall.[4] = 0 then
                roomToHall 4 2 roomPos podType state
            if state.Hall.[3] = 0 && state.Hall.[4] = 0 && state.Hall.[5] = 0 then
                roomToHall 5 2 roomPos podType state
            if state.Hall.[3] = 0 && state.Hall.[4] = 0 && state.Hall.[5] = 0 && state.Hall.[6] = 0 then
                roomToHall 6 2 roomPos podType state
    
    // from Room 3
    match canMoveFromRoom state 3 with
        | None -> ()
        | Some (podType, roomPos) ->
            if state.Hall.[0] = 0 && state.Hall.[1] = 0 && state.Hall.[2] = 0 && state.Hall.[3] = 0 then
                roomToHall 0 3 roomPos podType state
            if state.Hall.[1] = 0 && state.Hall.[2] = 0 && state.Hall.[3] = 0 then
                roomToHall 1 3 roomPos podType state
            if state.Hall.[2] = 0 && state.Hall.[3] = 0 then
                roomToHall 2 3 roomPos podType state
            if state.Hall.[3] = 0 then
                roomToHall 3 3 roomPos podType state
            if state.Hall.[4] = 0 then
                roomToHall 4 3 roomPos podType state
            if state.Hall.[4] = 0 && state.Hall.[5] = 0 then
                roomToHall 5 3 roomPos podType state
            if state.Hall.[4] = 0 && state.Hall.[5] = 0 && state.Hall.[6] = 0 then
                roomToHall 6 3 roomPos podType state
    
    // from Room 4
    match canMoveFromRoom state 4 with
        | None -> ()
        | Some (podType, roomPos) ->
            if state.Hall.[0] = 0 && state.Hall.[1] = 0 && state.Hall.[2] = 0 && state.Hall.[3] = 0 && state.Hall.[4] = 0 then
                roomToHall 0 4 roomPos podType state
            if state.Hall.[1] = 0 && state.Hall.[2] = 0 && state.Hall.[3] = 0 && state.Hall.[4] = 0 then
                roomToHall 1 4 roomPos podType state
            if state.Hall.[2] = 0 && state.Hall.[3] = 0 && state.Hall.[4] = 0 then
                roomToHall 2 4 roomPos podType state
            if state.Hall.[3] = 0 && state.Hall.[4] = 0 then
                roomToHall 3 4 roomPos podType state
            if state.Hall.[4] = 0 then
                roomToHall 4 4 roomPos podType state
            if state.Hall.[5] = 0 then
                roomToHall 5 4 roomPos podType state
            if state.Hall.[5] = 0 && state.Hall.[6] = 0 then
                roomToHall 6 4 roomPos podType state
}

let isFinished state =
    let mutable isDone = true

    for room in 1 .. 4 do
        for roomPos in 0 .. state.Rooms.[room].Length - 1 do
            if state.Rooms.[room].[roomPos] <> room then isDone <- false
    
    isDone

let minimumEnergyNeeded state =
    let mutable bestSoFar = Int32.MaxValue

    let rec minEnergy state =
        if isFinished state then
            if state.EnergyUsed < bestSoFar then
                bestSoFar <- state.EnergyUsed
            state.EnergyUsed
        else
            // if we can make any move to a room, do it
            match possibleMoveToRoom state |> Seq.tryHead with
            | Some moveToRoom ->
                minEnergy moveToRoom
            | None ->
                // no moves to a room available
                let otherMoves = possibleOtherMoves state
                if Seq.isEmpty otherMoves then
                    Int32.MaxValue
                else
                    otherMoves |> Seq.map minEnergy |> Seq.min
    
    minEnergy state

let part1() =
    let initialState = startStatePart1 input

    let minEnergy = minimumEnergyNeeded initialState

    printfn "Minimum energy needed: %i" minEnergy

let part2() =
    let initialState = startStatePart2 input

    let minEnergy = minimumEnergyNeeded initialState

    printfn "Minimum energy needed: %i" minEnergy
