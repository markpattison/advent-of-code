module Day10

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day10.txt").[0]

let allLengths = input.Split(',') |> Array.map Int32.Parse

type State =
    {
        Elements: int[]
        Position: int
        SkipSize: int
    }

let initialState size =
    {
        Elements = [| 0 .. size - 1 |]
        Position = 0
        SkipSize = 0
    }

let knot state length =
    let newElements = Array.copy state.Elements

    for i in 0 .. length - 1 do
        newElements.[(state.Position + i) % state.Elements.Length] <- state.Elements.[(state.Position + length - 1 - i) % state.Elements.Length]
    
    {
        Elements = newElements
        Position = (state.Position + length + state.SkipSize) % state.Elements.Length
        SkipSize = state.SkipSize + 1
    }

let part1() =
    let finalState =
        allLengths
        |> Array.fold knot (initialState 256)
    
    let product = finalState.Elements.[0] * finalState.Elements.[1]

    printfn "Product: %i" product

let allLengths2 =
    input.ToCharArray()
    |> Array.map int

let part2() =
    let lengthsPart2 = Array.append allLengths2 [| 17; 31; 73; 47; 23 |]
    
    let round state =
        lengthsPart2 |> Array.fold knot state

    let finalState =
        [| 0 .. 63 |]
        |> Array.fold (fun state _ -> round state) (initialState 256)

    let denseHash =
        finalState.Elements
        |> Array.chunkBySize 16
        |> Array.map (Array.reduce (^^^))
    
    let knotHash =
        denseHash
        |> Array.map (fun n -> n.ToString("x2"))
        |> System.String.Concat
    
    printfn "Knot hash: %s" knotHash
