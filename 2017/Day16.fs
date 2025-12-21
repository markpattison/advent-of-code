module Day16

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day16.txt").[0]

type Move =
    | Spin of int
    | Exchange of int * int
    | Partner of char * char

let parseMove (s: string) =
    match s.Substring(0, 1) with
    | "s" ->
        let n = s.Substring(1) |> Int32.Parse
        Spin n
    | "x" ->
        let parts = s.Substring(1).Split('/') |> Array.map Int32.Parse
        Exchange (parts.[0], parts.[1])
    | "p" ->
        let parts = s.Substring(1).Split('/') |> Array.map (fun t -> t.[0])
        Partner (parts.[0], parts.[1])
    | _ -> failwith "unexpected"

let allMoves =
    input.Split(',')
    |> Array.map parseMove

let spin n (s: string) =
    s.Substring(s.Length - n) + s.Substring(0, s.Length - n)

let exchange a b (s: string) =
    let chars = s.ToCharArray()
    let x = chars.[a]
    chars.[a] <- chars.[b]
    chars.[b] <- x
    new String(chars)

let partner c d (s: string) =
    let chars = s.ToCharArray()
    let ic = Array.IndexOf(chars, c)
    let id = Array.IndexOf(chars, d)
    chars.[ic] <- d
    chars.[id] <- c
    new String(chars)

let apply (s: string) move =
    match move with
    | Spin n -> spin n s
    | Exchange (a, b) -> exchange a b s
    | Partner (c, d) -> partner c d s

let start = "abcdefghijklmnop"

let part1() =
    let finalOrder = Array.fold apply start allMoves

    printfn "Order: %s" finalOrder

let part2() =
    let applyAll s = Array.fold apply s allMoves

    let rec findRepeat acc s =
        let next = applyAll s
        if next = start then (acc + 1) else findRepeat (acc + 1) next
    
    let cycleLength = findRepeat 0 start

    let remainder = 1000000000 % cycleLength

    let rec applyMany n s =
        if n = 0 then s else applyMany (n - 1) (applyAll s)
    
    let finalOrder = applyMany remainder start

    printfn "Order: %s" finalOrder
