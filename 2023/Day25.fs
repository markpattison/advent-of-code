module Day25

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day25.txt")

type Line = { Left: string; Right: string[] }
type Wire = { Comp1: string; Comp2: string }

let parseLine (s: string) =
    let left = s.Substring(0, 3)
    let rightSplits = s.Substring(5).Split(" ")

    { Left = left; Right = rightSplits }

let toOrderedWire (s1: string) (s2: string) =
    if s1 < s2 then { Comp1 = s1; Comp2 = s2 } else { Comp1 = s2; Comp2 = s1 }

let allWires =
    input
    |> Array.map parseLine
    |> Array.collect (fun line -> line.Right |> Array.map (fun r -> toOrderedWire line.Left r))
    |> Array.distinct

let allComponents =
    allWires
    |> Array.collect (fun w -> [| w.Comp1; w.Comp2 |])
    |> Array.distinct
    |> Array.sort

let numComponents = allComponents.Length

let toIndex =
    allComponents
    |> Array.mapi (fun i c -> (c, i))
    |> Map.ofArray

let adjacency =
    let connected = Array2D.create numComponents numComponents 0

    allWires
    |> Array.iter (fun w ->
        let index1 = toIndex.[w.Comp1]
        let index2 = toIndex.[w.Comp2]
        connected.[index1, index2] <- 1
        connected.[index2, index1] <- 1)
    
    connected

let rand = Random()

let getRandomEdge (vertices: Map<int, int[]>) =
    let mutable a = -1
    let mutable b = -1

    let inits = Map.keys vertices |> Seq.toArray
    let count = inits.Length

    while (a < 0 || b < 0) do
        a <- inits.[rand.Next(count)]
        match Map.tryFind a vertices with
        | None -> ()
        | Some x ->
            b <- rand.Next(x.Length)
            if x.[b] <> a then b <- x.[b] else b <- -1
    if not (Map.containsKey b vertices) then failwith "oops"
    (a, b)

let part1() =

    // Karger's algorithm

    let connectedTo wireIndex =
        seq {
            for i in 0 .. numComponents - 1 do
                if i <> wireIndex && adjacency.[wireIndex, i] = 1 then yield i
        }
        |> Seq.toArray

    let originalGraph = Array.init numComponents (fun i -> (i, connectedTo i)) |> Map.ofArray

    let mutable found = false
    let mutable product = 0

    while not found do
        let mutable vertices = originalGraph
        let included = Array.init numComponents (fun i -> [| i |])

        while Map.count vertices > 2 do
            let (vFrom, vTo) = getRandomEdge vertices

            for i in 0 .. vertices.[vTo].Length - 1 do
                let vertex = vertices.[vTo].[i]

                if vertex <> vFrom then
                    vertices <- vertices |> Map.map (fun v existing -> if v = vFrom then Array.append [| vertex |] existing else existing)

                vertices <- vertices |> Map.map (fun v existing -> if v = vertex then existing |> Array.filter (fun p -> p <> vTo) else existing)

                if vertex <> vFrom then
                    vertices <- vertices |> Map.map (fun v existing -> if v = vertex then Array.append [| vFrom |] existing else existing)
                
            vertices <- vertices |> Map.remove vTo
            included.[vFrom] <- Array.concat [| [| vTo |]; included.[vTo]; included.[vFrom] |] |> Array.distinct

        let cutSize = vertices |> Map.toSeq |> Seq.head |> snd |> Array.length

        if cutSize = 3 then
            found <- true
            let verts = vertices |> Map.toArray |> Array.map fst
            product <- included.[verts.[0]].Length * included.[verts.[1]].Length

    printfn "Product: %i" product
