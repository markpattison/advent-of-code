module Day14

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day14.txt").[0]

let md5 = System.Security.Cryptography.MD5.Create()

let hash (s: string) =
    let bytes = System.Text.Encoding.ASCII.GetBytes(s);
    let hash = md5.ComputeHash(bytes)
    Convert.ToHexString(hash).ToLower()

let containsTriple (s: string) =
    if s.Length < 3 then
        None
    else
        let chars = s.ToCharArray()
        let mutable foundRepeats = false
        let mutable i = 0
        while not (foundRepeats || i > chars.Length - 3) do
            if chars.[i] = chars.[i + 1] && chars.[i] = chars.[i + 2] then
                foundRepeats <- true
            else
                i <- i + 1
        if foundRepeats then Some chars.[i] else None

let containsFive (s: string) (c: char) =
    if s.Length < 5 then
        false
    else
        let chars = s.ToCharArray()
        let mutable foundFive = false
        let mutable i = 0
        while not (foundFive || i > chars.Length - 5) do
            if chars.[i] = c && chars.[i + 1] = c && chars.[i + 2] = c && chars.[i + 3] = c && chars.[i + 4] = c then
                foundFive <- true
            else
                i <- i + 1
        foundFive

let isKey fHash n =
    match containsTriple (fHash n) with
    | None -> false
    | Some c ->
        let mutable foundFive = false
        let mutable i = n + 1
        while not (foundFive || i > n + 1000) do
            if containsFive (fHash i) c then
                foundFive <- true
            else
                i <- i + 1
        foundFive

let part1() =
    let fHash i = hash (input + i.ToString())
    let key64 =
        Seq.initInfinite id
        |> Seq.filter (isKey fHash)
        |> Seq.item 63
    
    printfn "Index of 64th key: %i" key64

let superHash (s: string) =
    let mutable h = s
    for _ in 0 .. 2016 do
        h <- hash h
    h

let mutable hashCache : Map<int, string> = Map.empty

let superHashCache n =
    match Map.tryFind n hashCache with
    | Some s -> s
    | None ->
        let h = superHash (input + n.ToString())
        hashCache <- Map.add n h hashCache
        h

let part2() =
    let key64 =
        Seq.initInfinite id
        |> Seq.filter (isKey superHashCache)
        |> Seq.item 63
    
    printfn "Index of 64th key: %i" key64
