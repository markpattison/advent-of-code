module Day17

open System
open System.IO

let input =
    File.ReadAllLines(@"input\day17.txt").[0]

let numSteps = input |> Int32.Parse

type Element =
    {
        Value: int
        mutable Next: Element option
    }

let rec takeSteps element steps =
    if steps = 0 then
        element
    else
        match element.Next with
        | Some next -> takeSteps next (steps - 1)
        | None -> failwith "unexpected"

let addElements steps stopAfter =
    let initialElement = { Value = 0; Next = None }
    initialElement.Next <- Some initialElement

    let rec update current nextValue =
        let stepsToTake = steps % nextValue     // length of buffer is always equal to the next value
        let insertAfter = takeSteps current stepsToTake
        let newElement = { Value = nextValue; Next = insertAfter.Next }
        insertAfter.Next <- Some newElement
        if nextValue % 1000000 = 0 then printfn "%i" nextValue
        if nextValue = stopAfter then
            newElement
        else
            update newElement (nextValue + 1)
    
    update initialElement 1

let part1() =
    let element2017 = addElements numSteps 2017

    let valueAfter2017 = element2017.Next.Value.Value

    printfn "Value after 2017: %i" valueAfter2017

let part2() =
    let target = 50000000

    let rec valueAfterZero length positionRelativeToZero elementAfterZero lastElement =
        if lastElement = target then
            elementAfterZero
        else
            let newPosition = (positionRelativeToZero + numSteps) % length
            if newPosition = 0 then
                valueAfterZero (length + 1) (newPosition + 1) (lastElement + 1) (lastElement + 1)
            else
                valueAfterZero (length + 1) (newPosition + 1) elementAfterZero (lastElement + 1)

    let elementAfterZero = valueAfterZero 1 0 0 0
    
    printfn "Value after 0: %i" elementAfterZero
