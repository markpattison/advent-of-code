module Day3

open System
open System.IO

let digits = 12

let binaryNumbers =
    File.ReadAllLines(@"input\day3.txt")

let totalNumbers = binaryNumbers.Length

let countOnes (bins: string[]) digit =
    bins
    |> Seq.filter (fun bn -> bn.[digit] = '1')
    |> Seq.length

let part1() =
    let onesCount =
        [| 0 .. digits - 1 |]
        |> Array.map (countOnes binaryNumbers)

    let mostCommonDigits =
        onesCount
        |> Array.map (fun oneCount -> if oneCount > (totalNumbers / 2) then '1' else '0')

    let leastCommonDigits =
        mostCommonDigits
        |> Array.map (fun digit -> if digit = '1' then '0' else '1')

    let gamma = Convert.ToInt32(new string(mostCommonDigits), 2)
    let epsilon = Convert.ToInt32(new string(leastCommonDigits), 2)

    let power = gamma * epsilon

    printfn "Power: %i" power

let part2() =
    
    let mostCommonFirstDigitOrOne bins checkDigit =
        let ones = countOnes bins checkDigit
        let zeroes = bins.Length - ones

        if ones >= zeroes then '1' else '0'

    let leastCommonFirstDigitOrZero bins checkDigit =
        let ones = countOnes bins checkDigit
        let zeroes = bins.Length - ones

        if zeroes <= ones then '0' else '1'

    let rec search bitCriteria (bins: string[]) checkDigit =
        match bins with
        | [||] -> failwith "empty"
        | [| s |] -> s
        | _ ->
            let requiredFirstDigit = bitCriteria bins checkDigit
            let filtered = bins |> Array.filter (fun s -> s.[checkDigit] = requiredFirstDigit)
            search bitCriteria filtered (checkDigit + 1)

    let oxygenRating = Convert.ToInt32(search mostCommonFirstDigitOrOne binaryNumbers 0, 2)
    let co2Rating = Convert.ToInt32(search leastCommonFirstDigitOrZero binaryNumbers 0, 2)

    let lifeSupportRating = oxygenRating * co2Rating

    printfn "Life support rating: %i" lifeSupportRating
