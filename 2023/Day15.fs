module Day15

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day15.txt").[0].Split(',')

let folder state (c: char) =
    ((state + int c) * 17) % 256

let hash (s: string) =
    s.ToCharArray()
    |> Array.fold folder 0

let part1() =
    let total =
        input
        |> Array.map hash
        |> Array.sum
    
    printfn "Total: %i" total

type Lens =
    {
        Label: string
        FocalLength: int
    }

type Step =
    | Remove of string * int
    | Add of string * int * int

let parseInput (s: string) =
    if s.EndsWith('-') then
        let label = s.Substring(0, s.Length - 1)
        let box = hash label
        Remove (label, box)
    else
        let split = s.Split('=')
        let label = split.[0]
        let box = hash label
        let focalLength = Int32.Parse(split.[1])
        Add (label, box, focalLength)

let folder2 (state: (Lens list)[]) step =
    match step with
    | Remove (label, box) ->
        state.[box] <- state.[box] |> List.filter (fun l -> l.Label <> label)
    | Add (label, box, focalLength) ->
        let newLens = { Label = label; FocalLength = focalLength }
        match state.[box] |> List.tryFindIndex (fun l -> l.Label = label) with
        | Some i ->
            state.[box] <- state.[box] |> List.mapi (fun j l -> if i = j then newLens else l)
        | None ->
            state.[box] <- newLens :: state.[box]
    state

let score (state: (Lens list)[]) =
    let scoreBox boxNum (lenses: Lens list) =
        let length = List.length lenses
        lenses
        |> List.mapi (fun i lens -> (1 + boxNum) * (length - i) * lens.FocalLength)
        |> List.sum
    
    state
    |> Array.mapi scoreBox
    |> Array.sum

let part2() =
    let steps =
        input
        |> Array.map parseInput
    
    let (initialState: (Lens list)[]) = Array.create 256 []

    let total =
        steps
        |> Array.fold folder2 initialState
        |> score
    
    printfn "Total: %i" total
