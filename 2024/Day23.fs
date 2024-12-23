module Day23

open System
open System.IO

let input = File.ReadAllLines(@"input\day23.txt")

let parseLine (s: string) =
    s.Substring(0, 2), s.Substring(3, 2)

let allLinks = input |> Array.map parseLine

let allNames =
    allLinks
    |> Array.collect (fun (a, b) -> [| a; b |])
    |> Array.distinct

let numNames = allNames.Length

let nameIndices, indexNames =
    let indexed =
        allNames
        |> Array.mapi (fun i name -> (name, i))
    (indexed |> Map.ofArray, indexed |> Array.map (fun (a, b) -> (b, a)) |> Map.ofArray)

let connections =
    let arr = Array2D.create numNames numNames false
    allLinks
    |> Array.iter (fun (n1, n2) ->
        let i1 = nameIndices.[n1]
        let i2 = nameIndices.[n2]
        arr.[i1, i2] <- true
        arr.[i2, i1] <- true)
    arr

let part1() =
    let triples =
        seq {
            for i in 0 .. numNames - 3 do
                for j in i + 1 .. numNames - 2 do
                    for k in j + 1 .. numNames - 1 do
                        if connections.[i, j] && connections.[i, k] && connections.[j, k] then
                            (indexNames.[i], indexNames.[j], indexNames.[k])
        } |> Seq.toArray
    
    let tTriples =
        triples
        |> Array.filter (fun (a, b, c) -> a.StartsWith('t') || b.StartsWith('t') || c.StartsWith('t'))

    let count = Array.length tTriples

    printfn "Count: %i" count

let part2() =
    let allNeighbours =
        Array.init numNames (fun i ->
            seq {
                for j in 0 .. numNames - 1 do
                    if connections.[i, j] then j
            } |> Set.ofSeq)

    let mutable maximalSize = 0
    let mutable maximals: string list = []

    let rec bronKerbosch (r: Set<int>) (p: Set<int>) (x: Set<int>) =
        if Set.isEmpty p && Set.isEmpty x then
            let size = Set.count r
            if size >= maximalSize then
                let names = r |> Seq.map (fun i -> indexNames.[i]) |> Seq.sort |> Seq.toArray
                let password = String.Join(',', names)
                if size > maximalSize then
                    maximalSize <- size
                    maximals <- [ password ]
                else
                    maximals <- password :: maximals
        else
            let u = Set.union p x |> Set.minElement
            let pnu = Set.difference p allNeighbours.[u]
            let mutable p1 = p
            let mutable x1 = x
            for v in pnu do
                let nv = allNeighbours.[v]
                bronKerbosch (Set.add v r) (Set.intersect p1 nv) (Set.intersect x1 nv)
                p1 <- Set.remove v p
                x1 <- Set.add v x1

    let all = [| 0 .. numNames - 1 |] |> Set.ofArray

    bronKerbosch Set.empty all Set.empty
    
    for p in List.distinct maximals do
        printfn "%s" p
