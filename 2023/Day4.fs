module Day4

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day4.txt")

type Card =
    {
        CardNumber: int
        Winning: int list
        YouHave: int list
    }

let parseLine (s: string) =
    let cardNumber =
        s.Substring(5, 3).Trim()
        |> Int32.Parse
    let winning =
        s.Substring(10, 29).Trim().Replace("  ", " ").Split(' ')
        |> Array.map Int32.Parse
        |> List.ofArray
    let youHave =
        s.Substring(42, 74).Trim().Replace("  ", " ").Split(' ')
        |> Array.map Int32.Parse
        |> List.ofArray
    
    { CardNumber = cardNumber; Winning = winning; YouHave = youHave }

let howManyWinning card =
    card.YouHave
    |> List.filter (fun n -> List.contains n card.Winning)
    |> List.length

let score card =
    let wins = howManyWinning card
    
    if wins = 0 then
        0
    else
        pown 2 (wins - 1)

let cards = input |> Array.map parseLine

let part1() =
    let scores = cards |> Array.map score

    let sum = scores |> Array.sum
    
    printfn "Total score: %i" sum

let part2() =
    let numberOfCards = Array.create cards.Length 1

    let processCard cardIndex =
        let numCopies = numberOfCards.[cardIndex]
        let wins = howManyWinning cards.[cardIndex]

        for i in cardIndex + 1 .. cardIndex + wins do
            numberOfCards.[i] <- numberOfCards.[i] + numCopies
    
    for i in 0 .. cards.Length - 1 do
        processCard i
    
    let totalCards = Array.sum numberOfCards

    printfn "Total cards: %i" totalCards
