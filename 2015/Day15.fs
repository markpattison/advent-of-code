module Day15

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day15.txt")

type Ingredient =
    {
        Capacity: int
        Durability: int
        Flavor: int
        Texture: int
        Calories: int
    }

let parseLine (s: string) =
    let parts = s.Replace(",", "").Split(' ')
    {
        Capacity = parts.[2] |> Int32.Parse
        Durability = parts.[4] |> Int32.Parse
        Flavor = parts.[6] |> Int32.Parse
        Texture = parts.[8] |> Int32.Parse
        Calories = parts.[10] |> Int32.Parse
    }

let ingredients =
    input
    |> Array.map parseLine

let scoreAttribute recipe getter =
    recipe
    |> Array.mapi (fun i amount -> amount * getter ingredients.[i])
    |> Array.sum
    |> (max 0)

let score recipe =
    let scorer = scoreAttribute recipe

    (scorer _.Capacity) * (scorer _.Durability) * (scorer _.Flavor) * (scorer _.Texture)

let totalCalories recipe =
    scoreAttribute recipe _.Calories

let possibleRecipes =
    seq {
        for i in 0 .. 100 do
            for j in 0 .. 100 - i do
                for k in 0 .. 100 - i - j do
                    [| i; j; k; 100 - i - j - k |]
    }

let part1() =
    let maxScore =
        possibleRecipes
        |> Seq.map score
        |> Seq.max
    
    printfn "Max score: %i" maxScore

let part2() =
    let maxScore =
        possibleRecipes
        |> Seq.filter (fun recipe -> totalCalories recipe = 500)
        |> Seq.map score
        |> Seq.max
    
    printfn "Max score: %i" maxScore
