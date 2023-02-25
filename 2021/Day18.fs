module Day18

open System
open System.IO

let lines = File.ReadAllLines(@"input\day18.txt")

type Element =
    | Number of int
    | Pair of Element * Element

type Token =
    | StartList
    | EndList
    | Value of int

let rec toString sfn =
    match sfn with
    | Number n -> string n
    | Pair (el1, el2) -> sprintf "[%s,%s]" (toString el1) (toString el2)

let tokenise line =
    let rec innerTokenise acc (s: string) =
        match s with
        | "" -> List.rev acc
        | x when x.StartsWith('[') -> innerTokenise (StartList :: acc) (x.Substring(1))
        | x when x.StartsWith(']') -> innerTokenise (EndList :: acc) (x.Substring(1))
        | x when x.StartsWith(',') -> innerTokenise acc (x.Substring(1))
        | _ ->
            let value, remaining =
                if s.Length > 1 && Char.IsAsciiDigit(s.[1]) then
                    s.Substring(0, 2) |> int, s.Substring(2)
                else
                    s.Substring(0, 1) |> int, s.Substring(1)
            innerTokenise (Value value :: acc) remaining
    
    innerTokenise [] line

let rec parse tokens =
    let rec innerParse currentItems tokens =
        match tokens with
        | [] -> currentItems, []
        | Value v :: remaining -> innerParse (Number v :: currentItems) remaining
        | StartList :: remaining ->
            let items, thenRemaining = innerParse [] remaining
            match items with
            | [ item1; item2 ] ->
                innerParse (Pair (item2, item1) :: currentItems) thenRemaining
            | _ -> failwith "error"
        | EndList :: remaining ->
            currentItems, remaining
    
    innerParse [] tokens
    |> fst
    |> List.head

let snailfishNumbers = lines |> Array.map (tokenise >> parse)

type Explosion =
    | NotExplodedYet
    | Exploded of int * int * int // indexOfZero, leftAdd, rightAdd

let explodeIfPossible snailfishNumber =
    let mutable numberIndex = 0

    let rec innerExplode el explosion depth =
        match el with
        | Number n ->
            numberIndex <- numberIndex + 1
            Number n, explosion
        | Pair (Number n1, Number n2) when depth >= 4 && explosion = NotExplodedYet ->
            Number 0, Exploded (numberIndex, n1, n2)
        | Pair (el1, el2) ->
            let left, explosionAfterLeft = innerExplode el1 explosion (depth + 1)
            let right, explosionAfterRight = innerExplode el2 explosionAfterLeft (depth + 1)
            Pair (left, right), explosionAfterRight
    
    let explodeResult, explosion = innerExplode snailfishNumber NotExplodedYet 0

    match explosion with
    | NotExplodedYet -> None
    | Exploded (indexOfZero, leftAdd, rightAdd) ->
        let mutable newIndex = 0

        let rec addExplosion el =
            match el with
            | Number n ->
                let n' = if newIndex = indexOfZero - 1 then n + leftAdd elif newIndex = indexOfZero + 1 then n + rightAdd else n
                newIndex <- newIndex + 1
                Number n'
            | Pair (el1, el2) ->
                Pair (addExplosion el1, addExplosion el2)
        
        addExplosion explodeResult |> Some

let splitIfPossible snailfishNumber =
    let rec innerSplit el toSplit =
        match el with
        | Pair (el1, el2) ->
            let left, toSplitAfterLeft = innerSplit el1 toSplit
            let right, toSplitAfterRight = innerSplit el2 toSplitAfterLeft
            Pair (left, right), toSplitAfterRight
        | Number n when n >= 10 && toSplit ->
            Pair (Number (n / 2), Number ((n + 1) / 2)), false
        | Number n -> Number n, toSplit
    
    let splitResult, stillToSplit = innerSplit snailfishNumber true

    if stillToSplit then None else Some splitResult

let rec reduce snailfishNumber =
    match explodeIfPossible snailfishNumber with
    | Some sfn -> reduce sfn
    | None ->
        match splitIfPossible snailfishNumber with
        | Some sfn -> reduce sfn
        | None -> snailfishNumber

let add sfn1 sfn2 =
    Pair (sfn1, sfn2)
    |> reduce

let rec magnitude snailfishNumber =
    match snailfishNumber with
    | Number n -> n
    | Pair (el1, el2) -> 3 * magnitude el1 + 2 * magnitude el2

let part1() =
    let sum = Array.reduce add snailfishNumbers
    let mag = magnitude sum

    printfn "Magnitude: %i" mag

let part2() =
    let pairs =
        Array.allPairs snailfishNumbers snailfishNumbers
        |> Array.filter (fun (sfn1, sfn2) -> sfn1 <> sfn2)
    
    let maxMagnitude =
        pairs
        |> Array.map (fun (sfn1, sfn2) -> add sfn1 sfn2 |> magnitude)
        |> Array.max
    
    printfn "Max magnitude: %i" maxMagnitude
