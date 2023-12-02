module Day2

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day2.txt")

type Cube = Red | Green | Blue

type Cubes = { Red: int; Green: int; Blue: int }

type GameResult = { GameId: int; Sets: Cubes[] }

let parseEntry (s: string) =
    if s.EndsWith("green") then
        Green, Int32.Parse(s.Substring(0, s.Length - 6))
    elif s.EndsWith("red") then
        Red, Int32.Parse(s.Substring(0, s.Length - 4))
    elif s.EndsWith("blue") then
        Blue, Int32.Parse(s.Substring(0, s.Length - 5))
    else
        failwithf "Unexpected cube: %s" s

let parseSet (s: string) =
    let entries =
        s.Split(",")
        |> Array.map (fun s -> s.Trim())
        |> Array.map parseEntry
    
    {
        Red = entries |> Array.filter (fun (c, _) -> c = Red) |> Array.sumBy snd
        Green = entries |> Array.filter (fun (c, _) -> c = Green) |> Array.sumBy snd
        Blue = entries |> Array.filter (fun (c, _) -> c = Blue) |> Array.sumBy snd
    }

let parseLine (s: string) =
    let colon = s.IndexOf(":")
    let gameId = Int32.Parse(s.Substring(5, colon - 5))

    let allSets = s.Substring(colon + 1)
    let sets = allSets.Split(";") |> Array.map parseSet

    { GameId = gameId; Sets = sets }

let games = input |> Array.map parseLine

let part1() =
    let possibleGames =
        games
        |> Array.filter (fun gr -> gr.Sets |> Array.forall (fun set -> set.Red <= 12 && set.Green <= 13 && set.Blue <= 14))
    
    let sumIds = possibleGames |> Array.sumBy _.GameId

    printfn "Sum of IDs: %i" sumIds

let power gr =
    let minRed = gr.Sets |> Array.map _.Red |> Array.max
    let minGreen = gr.Sets |> Array.map _.Green |> Array.max
    let minBlue = gr.Sets |> Array.map _.Blue |> Array.max

    let power = minRed * minGreen * minBlue
    power

let part2() =
    let powers = games |> Array.map power

    let sumPowers = powers |> Array.sum

    printfn "Sum of powers: %i" sumPowers
