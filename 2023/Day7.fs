module Day7

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day7.txt")

let toCardValue (c: char) =
    match c with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | n when Char.IsAsciiDigit(n) -> int n - 48
    | _ -> failwith "invalid card"

type Hand =
    {
        Cards: int[]
        Bid: int
    }

let parseLine (s: string) =
    let cardsBid = s.Split(" ")
    let cards = cardsBid.[0].ToCharArray() |> Array.map toCardValue
    let bid = cardsBid.[1] |> Int32.Parse

    { Cards = cards; Bid = bid }

let hands =
    input
    |> Array.map parseLine

type Rank =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard

let toRank cards =
    match cards |> Array.countBy id |> Array.sortByDescending snd with
    | [| (_, 5) |] -> FiveOfAKind
    | [| (_, 4); (_, 1) |] -> FourOfAKind
    | [| (_, 3); (_, 2) |] -> FullHouse
    | [| (_, 3); (_, 1); (_, 1 ) |] -> ThreeOfAKind
    | [| (_, 2); (_, 2); (_, 1 ) |] -> TwoPair
    | [| (_, 2); (_, 1); (_, 1 ); (_, 1) |] -> OnePair
    | [| (_, 1); (_, 1); (_, 1 ); (_, 1); (_, 1) |] -> HighCard
    | _ -> failwith "invalid hand"

let sortHands handsRanks =
    handsRanks
    |> Seq.sortBy (fun (h, _) -> h.Cards.[4])
    |> Seq.sortBy (fun (h, _) -> h.Cards.[3])
    |> Seq.sortBy (fun (h, _) -> h.Cards.[2])
    |> Seq.sortBy (fun (h, _) -> h.Cards.[1])
    |> Seq.sortBy (fun (h, _) -> h.Cards.[0])
    |> Seq.sortByDescending snd
    |> Seq.map fst
    |> Seq.toArray

let valueWinnings hands =
    hands
    |> Array.mapi (fun i hand -> (i + 1) * hand.Bid)
    |> Array.sum

let part1() =
    let totalWinnings =
        hands
        |> Array.map (fun h -> (h, toRank h.Cards))
        |> sortHands
        |> valueWinnings
    
    printfn "Total winnings: %i" totalWinnings

let rec possibleHands (acc: int list) (remaining: int list) =
    match remaining with
    | [] -> [ acc ]
    | x :: xs ->
        match x with
        | 11 ->
            [
                for c in 2 .. 14 do
                    if c <> 11 then yield! possibleHands (c :: acc) xs
            ]
        | c -> possibleHands (c :: acc) xs

let toBestRankWithJokers hand =
    let possibleRanks =
        possibleHands [] (List.ofArray hand.Cards)
        |> List.map (Array.ofList)
        |> List.map toRank
    
    let bestRank =
        possibleRanks
        |> List.min
    
    bestRank

let replaceJokerWithZero hand =
    { hand with Cards = hand.Cards |> Array.map (fun c -> if c = 11 then 0 else c) }

let part2() =
    let totalWinnings =
        hands
        |> Array.map (fun h -> (replaceJokerWithZero h, toBestRankWithJokers h))
        |> sortHands
        |> valueWinnings
    
    printfn "Total winnings: %i" totalWinnings
