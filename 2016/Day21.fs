module Day21

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day21.txt")

type Operation =
    | SwapPosition of int * int
    | SwapLetter of char * char
    | RotateLeft of int
    | RotateRight of int
    | RotateRightOnIndex of char
    | Reverse of int * int
    | Move of int * int

let parseLine (s: string) =
    match s.Split(' ') with
    | [| "swap"; "position"; x; "with"; "position"; y  |] -> SwapPosition (Int32.Parse(x), Int32.Parse(y))
    | [| "swap"; "letter"; x; "with"; "letter"; y |] -> SwapLetter (x.[0], y.[0])
    | [| "rotate"; "left"; x; "steps"|] | [| "rotate"; "left"; x; "step"|] -> RotateLeft (Int32.Parse(x))
    | [| "rotate"; "right"; x; "steps"|] | [| "rotate"; "right"; x; "step"|] -> RotateRight (Int32.Parse(x))
    | [| "rotate"; "based"; "on"; "position"; "of"; "letter"; x |] -> RotateRightOnIndex x.[0]
    | [| "reverse"; "positions"; x; "through"; y |] -> Reverse (Int32.Parse(x), Int32.Parse(y))
    | [| "move"; "position"; x; "to"; "position"; y |] -> Move (Int32.Parse(x), Int32.Parse(y))
    | _ -> failwithf "unexpected %s" s

let allOperations = input |> Array.map parseLine

let performOperation (chars: char array) operation =
    match operation with
    | SwapPosition (x, y) ->
        let result = Array.copy chars
        result.[x] <- chars.[y]
        result.[y] <- chars.[x]
        result
    | SwapLetter (c1, c2) ->
        chars |> Array.map (fun c -> if c = c1 then c2 elif c = c2 then c1 else c)
    | RotateLeft x ->
        Array.init chars.Length (fun i -> chars.[(i + x) % chars.Length])
    | RotateRight x ->
        Array.init chars.Length (fun i -> chars.[(i + chars.Length - x) % chars.Length])
    | RotateRightOnIndex c ->
        let index = Array.IndexOf(chars, c)
        let rotateBy = 1 + index + if index >= 4 then 1 else 0
        Array.init chars.Length (fun i -> chars.[(i + chars.Length + chars.Length - rotateBy) % chars.Length])
    | Reverse (x, y) ->
        Array.init chars.Length (fun i -> if i < x || i > y then chars.[i] else chars.[x + y - i])
    | Move (x, y) when x < y ->
        Array.init chars.Length (fun i -> if i < x || i > y then chars.[i] elif i = y then chars.[x] else chars.[i + 1])
    | Move (x, y) when x > y ->
        Array.init chars.Length (fun i -> if i < y || i > x then chars.[i] elif i = y then chars.[x] else chars.[i - 1])
    | _ ->
        chars

let performOperations initial operations =
    Array.fold performOperation initial operations |> String

let part1() =
    let initial = "abcdefgh".ToCharArray()
    let result = performOperations initial allOperations

    printfn "Password: %s" result

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

let part2() =
    let target = "fbgdceah"

    let allPermutations =
        "abcdefgh".ToCharArray() |> Array.toList
        |> permute
        |> List.map List.toArray

    let unscrambled =
        allPermutations
        |> List.find (fun s -> performOperations s allOperations = target)
        |> String
    
    printfn "Unscrambled: %s" unscrambled
