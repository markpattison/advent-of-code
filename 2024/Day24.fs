module Day24

open System
open System.IO

let input = File.ReadAllLines(@"input\day24.txt")

let blank = input |> Array.findIndex (fun s -> s = "")

type Op = And | Or | Xor
type Gate = { In1: string; In2: string; Op: Op; Out: string }

let initialWires =
    input
    |> Array.take blank
    |> Array.map (fun s -> s.Substring(0, 3), if s.Substring(5, 1) = "1" then true else false)

let allGates =
    input
    |> Array.skip (blank + 1)
    |> Array.map (fun s ->
        let in1 = s.Substring(0, 3)
        let op, in2, out =
            match s.Substring(4, 3) with
            | "AND" -> And, s.Substring(8, 3), s.Substring(15, 3)
            | "XOR" -> Xor, s.Substring(8, 3), s.Substring(15, 3)
            | "OR " -> Or, s.Substring(7, 3), s.Substring(14, 3)
            | _ -> failwith "unexpected"
        { In1 = in1; In2 = in2; Op = op; Out = out })

let opResult in1 in2 op =
    match op with
    | And -> in1 && in2
    | Or -> in1 || in2
    | Xor -> in1 <> in2

let evaluateSystem startWires =
    let mutable finished = false
    let mutable wires =
        startWires
        |> Map.ofArray
    
    while not finished do
        finished <- true
        allGates
        |> Array.iter (fun gate ->
            match Map.tryFind gate.In1 wires, Map.tryFind gate.In2 wires, Map.tryFind gate.Out wires with
            | Some v1, Some v2, None ->
                wires <- wires |> Map.add gate.Out (opResult v1 v2 gate.Op)
                finished <- false
            | _ -> ())
    
    let zWires =
        wires
        |> Map.keys
        |> Seq.filter (fun s -> s.StartsWith('z'))
        |> Seq.sortDescending
        |> Seq.map (fun s -> if wires.[s] then '1' else '0')
        |> Seq.toArray
        |> System.String
    
    Convert.ToInt64(zWires, 2)

let part1() =
    let zValue = evaluateSystem initialWires
    printfn "Z value: %i" zValue

let fx x = sprintf "x%02i" x
let fy y = sprintf "y%02i" y

let part2() =
    let mutable gates = allGates

    let swap n1 n2 =
        gates <- gates |> Array.map (fun g ->
            { g with Out = if g.Out = n1 then n2 elif g.Out = n2 then n1 else g.Out })

    let rep n1 n2 =
        gates <- gates |> Array.map (fun g ->
            { g with In1 = (if g.In1 = n1 then n2 else g.In1); In2 = (if g.In2 = n1 then n2 else g.In2); Out = (if g.Out = n1 then n2 else g.Out) })
    
    let matchGate op in1 in2 g = g.Op = op && ((g.In1 = in1 && g.In2 = in2) || (g.In1 = in2 && g.In2 = in1))

    // found by eye for now...
    let swaps =
        [|
            "z14", "vss"
            "kdh", "hjf"
            "kpp", "z31"
            "sgj", "z35"
        |]

    swaps |> Array.iter (fun (s1, s2) -> swap s1 s2)

    for i in 0 .. 44 do
        let nXor = gates |> Array.find (matchGate Xor (fx i) (fy i)) |> _.Out
        let nAnd = gates |> Array.find (matchGate And (fx i) (fy i)) |> _.Out
        if i > 0 then rep nXor (sprintf "%02i xor" i)
        rep nAnd (sprintf "%02i and" i)

    for i in 1 .. 44 do
        let target = if i = 1 then "00 and" else sprintf "%02i car" (i - 1)
        match gates |> Array.tryFind (matchGate And (sprintf "%02i xor" i) target) with
        | Some nInt -> rep nInt.Out (sprintf "%02i int" i)
        | None -> printfn "not found: int %i" i
        match gates |> Array.tryFind (matchGate Or (sprintf "%02i int" i) (sprintf "%02i and" i)) with
        | Some nCar -> rep nCar.Out (sprintf "%02i car" i)
        | None -> printfn "not found: car %i" i

    // gates <- gates |> Array.sortBy (fun g -> g.Out)
    // gates |> Array.iter (fun g -> printfn "%s %s %O -> %s" g.In1 g.In2 g.Op g.Out)
    
    let result =
        swaps
        |> Array.collect (fun (s1, s2) -> [| s1; s2 |])
        |> Array.sort
        |> fun arr -> String.Join(",", arr)
    
    printfn "Swaps: %s" result
