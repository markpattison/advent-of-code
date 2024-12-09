module Day9

open System
open System.IO

let input = File.ReadAllText(@"input\day9.txt")

let numFiles = (input.Length + 1) / 2

let (fileLengths, freeLengths) =
    let files = Array.zeroCreate numFiles
    let frees = Array.zeroCreate (numFiles - 1)
    for i in 0 .. (numFiles - 1) do
        files.[i] <- Int32.Parse(input.[2 * i].ToString())
        if i < numFiles - 1 then
            frees.[i] <- Int32.Parse(input.[2 * i + 1].ToString())

    (files, frees)

let totalFileLengths =
    Array.sum fileLengths

let numBlocks =
    totalFileLengths + Array.sum freeLengths

let (initialBlocks, filePositions) =
    let blocks = Array.create numBlocks -1
    let positions = Array.zeroCreate numFiles
    let mutable block = 0

    for file in 0 .. (numFiles - 1) do
        positions.[file] <- block
        for b in block .. block + fileLengths.[file] - 1 do
            blocks.[b] <- file
        block <- block + fileLengths.[file]
        if file < numFiles - 1 then
            block <- block + freeLengths.[file]
    (blocks, positions)

let firstFreeBlock (blocks: int[]) startAt =
    let mutable i = startAt
    let mutable finished = false

    while not finished do
        if blocks.[i] = -1 then
            finished <- true
        else
            i <- i + 1
    
    if finished then i else failwith "Unexpected"

let lastFileBlock (blocks: int[]) startAt =
    let mutable i = startAt
    let mutable finished = false

    while not finished do
        if blocks.[i] > -1 then
            finished <- true
        else
            i <- i - 1
    
    if finished then i else failwith "Unexpected"

let moveOne (blocks: int[]) firstFree lastBlock =
    blocks.[firstFree] <- blocks.[lastBlock]
    blocks.[lastBlock] <- -1

let calculateChecksum (blocks: int[]) =
    let mutable checksum = 0L

    for i in 0 .. numBlocks - 1 do
        if blocks.[i] > -1 then
            checksum <- checksum + int64 i * int64 blocks.[i]
    checksum

let part1() =
    let blocks = Array.copy initialBlocks

    let mutable lastFree = 0
    let mutable lastLastBlock = blocks.Length - 1
    let mutable finished = false

    while not finished do
        let firstFree = firstFreeBlock blocks lastFree
        if firstFree = totalFileLengths then
            finished <- true
        else
            lastLastBlock <- lastFileBlock blocks lastLastBlock
            lastFree <- firstFree
            moveOne blocks firstFree lastLastBlock
    
    let checksum = calculateChecksum blocks
    
    printfn "Checksum: %i" checksum

let countFree (blocks: int[]) start =
    let mutable i = 0
    let mutable finished = false
    
    while not finished do
        if start + 1 >= blocks.Length || blocks.[start + i] > -1 then
            finished <- true
        else
            i <- i + 1
    
    i

let firstFreeOfLengthBy (blocks: int[]) length by =
    let mutable block = 0
    let mutable finished = false
    let mutable result : int option = None

    while not finished do
        if block >= by then
            finished <- true
        elif blocks.[block] > -1 then
            block <- block + 1
        else
            let numFree = countFree blocks block
            if numFree >= length then
                result <- Some block
                finished <- true
            else
                block <- block + numFree
    result

let part2() =
    let blocks = Array.copy initialBlocks

    for file in (numFiles - 1) .. -1 .. 1 do
        let fileLength = fileLengths.[file]
        let filePosition = filePositions.[file]
        match firstFreeOfLengthBy blocks fileLength filePosition with
        | None -> ()
        | Some firstFree ->
            for i in 0 .. fileLength - 1 do
                blocks.[firstFree + i] <- file
                blocks.[filePosition + i] <- -1
    
    let checksum = calculateChecksum blocks

    printfn "Checksum: %i" checksum
