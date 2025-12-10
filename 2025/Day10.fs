module Day10

open System
open System.IO

open Microsoft.Z3

let input =
    // """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
// [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
// [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}""".Split("\r\n")
    File.ReadAllLines(@"input\day10.txt")

type Machine =
    {
        Config: bool[]
        Buttons: int[][]
        Joltage: int[]
    }

let parseButton (s: string) =
    s.Substring(1, s.Length - 2).Split(',') |> Array.map Int32.Parse

let parseLine (s: string) =
    let parts = s.Split(' ')

    let config =
        parts.[0].Substring(1, parts.[0].Length - 2).ToCharArray()
        |> Array.map (fun c -> match c with | '#' -> true | '.' -> false | _ -> failwith "unexpected")
    
    let buttons =
        parts
        |> Array.skip 1
        |> Array.take (parts.Length - 2)
        |> Array.map parseButton
    
    let joltage =
        parts
        |> Array.last
        |> parseButton
    
    { Config = config; Buttons = buttons; Joltage = joltage }

let allMachines =
    input
    // |> Array.skip 12
    |> Array.map parseLine

let press machine buttonIndex config =
    let newConfig = Array.copy config
    let button = machine.Buttons.[buttonIndex]

    button |> Array.iter (fun i -> newConfig.[i] <- not newConfig.[i])

    newConfig

let choose n m =
    let rec choose lo =
        function
        |0 -> [[]]
        |i -> [for j in lo .. n - 1 do
                    for ks in choose (j+1) (i-1) do
                    yield j :: ks ]
    choose 0 m

let possiblePresses numButtons =
    seq {
        for i in 0 .. numButtons do
            yield! choose numButtons i
    }

let part1() =
    let findMinPresses machine =
        let initialConfig = Array.create machine.Config.Length false

        let result buttonsToPress =
            Seq.fold (fun config button -> press machine button config) initialConfig buttonsToPress
        
        let minPresses =
            possiblePresses (machine.Buttons.Length)
            |> Seq.find (fun buttons -> result buttons = machine.Config)
            |> List.length
        
        minPresses
    
    let totalPresses =
        allMachines
        |> Array.map findMinPresses
        |> Array.sum
    
    printfn "Total presses: %i" totalPresses

// type Equation =
//     {
//         ButtonIndices: int Set
//         Value: int
//     }

// let findMinPresses machine =
//     let allButtonIndices = [ 0 .. machine.Buttons.Length - 1 ] |> Set.ofList

//     let initialEquations =
//         [ 0 .. machine.Joltage.Length - 1 ]
//         |> List.map (fun j ->
//             let buttons =
//                 machine.Buttons
//                 |> Array.mapi (fun bi affects -> bi, affects)
//                 |> Array.filter (fun (_, affects) -> Array.contains j affects)
//                 |> Array.map fst
//                 |> Set.ofArray
//             { ButtonIndices = buttons; Value = machine.Joltage.[j] })
    
//     let newEquation eq1 eq2 =
//         {
//             ButtonIndices = Set.difference eq1.ButtonIndices eq2.ButtonIndices
//             Value = eq1.Value - eq2.Value
//         }
    
//     let mergeEquations eq1 eq2 =
//         {
//             ButtonIndices = Set.union eq1.ButtonIndices eq2.ButtonIndices
//             Value = eq1.Value + eq2.Value
//         }
    
//     let combineEquations eq1 eq2 =
//         if Set.isProperSuperset eq1.ButtonIndices eq2.ButtonIndices then
//             newEquation eq1 eq2 |> Some
//         elif Set.isProperSuperset eq2.ButtonIndices eq1.ButtonIndices then
//             newEquation eq2 eq1 |> Some
//         elif Set.intersect eq1.ButtonIndices eq2.ButtonIndices |> Set.isEmpty then
//             mergeEquations eq1 eq2 |> Some
//         else
//             None
    
//     let rec findAllEquations equations toAdd =
//         if equations |> List.map _.ButtonIndices |> List.distinct |> List.length <> equations.Length then
//             None
//         else
//             match toAdd with
//             | [] -> equations |> Some
//             | x :: xs ->
//                 let newEquations = List.choose (fun eq -> combineEquations eq x) equations
//                 let newToAdd = newEquations |> List.except equations |> List.except toAdd |> List.distinct
//                 findAllEquations (List.distinct (x :: equations)) (xs @ newToAdd)
    
//     let reduceEquation (b, v) eq =
//         if Set.contains b eq.ButtonIndices then
//             { ButtonIndices = Set.remove b eq.ButtonIndices; Value = eq.Value - v }
//         else
//             eq
    
//     let reduceEquations eqs (b, v) =
//         eqs |> List.map (reduceEquation (b, v)) |> List.filter (fun eq -> eq.ButtonIndices.Count > 0) |> List.distinct

//     let rec findPresses startingEquations toAdd indicesToFind =
//         let allEquationsOpt = findAllEquations startingEquations toAdd
//         match allEquationsOpt with
//         | None -> 100000
//         | Some allEquations ->
//             if allEquations |> List.exists (fun eq -> eq.Value < 0) then
//                 100000
//             else
//                 match allEquations |> List.tryFind (fun eq -> eq.ButtonIndices = indicesToFind) with
//                 | Some eq ->
//                     eq.Value
//                 | None ->
//                     let knownButtons =
//                         indicesToFind
//                         |> Set.toList
//                         |> List.map (fun b -> b, allEquations |> List.tryFind (fun eq -> eq.ButtonIndices = Set.singleton b))
//                         |> List.choose (fun (b, eqOpt) -> match eqOpt with None -> None | Some eq -> Some (b, eq.Value))
//                     let reducedEquations = knownButtons |> List.fold reduceEquations allEquations

//                     let missingButtons = Set.difference indicesToFind (knownButtons |> List.map fst |> Set.ofList) |> Set.toList
//                     let missingButtonsWithMinimumSize =
//                         missingButtons
//                         |> List.map (fun b -> b, reducedEquations |> List.filter (fun eq -> Set.contains b eq.ButtonIndices) |> List.map _.ButtonIndices.Count |> List.min)
//                     let minimumSize = missingButtonsWithMinimumSize |> List.map snd |> List.min
//                     let buttonsCanTry = missingButtonsWithMinimumSize |> List.filter (fun (_, minSize) -> minSize = minimumSize) |> List.map fst
//                     let buttonsCanTryWithMaxPossible = buttonsCanTry |> List.map (fun b -> b, reducedEquations |> List.filter (fun eq -> Set.contains b eq.ButtonIndices) |> List.map _.Value |> List.min) |> List.sortBy snd
//                     let tryButton, maxPossible = buttonsCanTryWithMaxPossible.Head

//                     let missing = Set.ofList missingButtons
//                     let knownTotal = knownButtons |> List.sumBy snd

//                     if maxPossible < 0 then
//                         100000
//                     else
//                         [ 0 .. maxPossible ] |> List.map (fun v ->
//                             findPresses reducedEquations [ { ButtonIndices = Set.singleton tryButton; Value = v } ] missing) |> List.min |> (fun x -> x + knownTotal)
    
//     let presses = findPresses [] initialEquations allButtonIndices

//     presses

// let part2() =
//     let totalPresses =
//         allMachines
//         |> Array.mapi (fun i m ->
//             findMinPresses m)
//         |> Array.sum
    
//     printfn "Total presses: %i" totalPresses

// gave up on my own solution - way too slow
// straightforward port of https://github.com/spj2401Dev/AOC-25/blob/master/Day10/Program.cs

let minPresses machine =
    let context = new Context();
    let opt = context.MkOptimize();

    let presses =
        [| 0 .. machine.Buttons.Length - 1 |]
        |> Array.map (fun i -> context.MkIntConst($"p{i}"), i)
    
    presses |> Array.iter (fun (p, _) -> opt.Add(context.MkGe(p, context.MkInt(0))))

    machine.Joltage
    |> Array.iteri (fun i joltage ->
        let affecting = presses |> Array.filter (fun (_, j) -> Array.contains i machine.Buttons.[j]) |> Array.map fst
        if affecting.Length > 0 then
            let sum : ArithExpr =
                if affecting.Length = 1 then
                    affecting.[0]
                else
                    let aff : ArithExpr[] = affecting |> Seq.cast |> Seq.toArray
                    context.MkAdd(aff)
            opt.Add(context.MkEq(sum, context.MkInt(machine.Joltage.[i]))))
    
    let pr : ArithExpr[] = presses |> Array.map fst |> Seq.cast |> Seq.toArray
    opt.MkMinimize(context.MkAdd(pr)) |> ignore
    opt.Check() |> ignore

    presses |> Array.sumBy (fun (p, _) ->   
        let e = opt.Model.Evaluate(p, true).ToString() |> Int32.Parse
        e
        )

let part2() =
    let totalPresses =
        allMachines
        |> Array.sumBy minPresses
    
    printfn "Total presses: %i" totalPresses