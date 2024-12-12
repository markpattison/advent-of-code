module Day12

open System
open System.IO

let input = File.ReadAllLines(@"input\day12.txt")

let sizeX = input.[0].Length
let sizeY = input.Length

let plots = Array2D.init sizeX sizeY (fun x y -> input.[y].[x])

let regionPriceAndMark (unfencedPlots: char[,]) rx ry =
    let mutable foundAll = false
    let mutable plotSize = 1
    let m = '#'
    let crop = unfencedPlots.[rx, ry]
    unfencedPlots.[rx, ry] <- m

    while not foundAll do
        let mutable foundAny = false
        for x in 0 .. sizeX - 1 do
            for y in 0 .. sizeY - 1 do
                if unfencedPlots.[x, y] = '#' then
                    if x > 0 && unfencedPlots.[x - 1, y] = crop then
                        unfencedPlots.[x - 1, y] <- m
                        plotSize <- plotSize + 1
                        foundAny <- true
                    if x < sizeX - 1 && unfencedPlots.[x + 1, y] = crop then
                        unfencedPlots.[x + 1, y] <- m
                        plotSize <- plotSize + 1
                        foundAny <- true
                    if y > 0 && unfencedPlots.[x, y - 1] = crop then
                        unfencedPlots.[x, y - 1] <- m
                        plotSize <- plotSize + 1
                        foundAny <- true
                    if y < sizeY - 1 && unfencedPlots.[x, y + 1] = crop then
                        unfencedPlots.[x, y + 1] <- m
                        plotSize <- plotSize + 1
                        foundAny <- true
        if (not foundAny) then foundAll <- true
    
    let mutable perimeter = 0
    for x in 0 .. sizeX - 1 do
        for y in 0 .. sizeY - 1 do
            if unfencedPlots.[x, y] = m then
                if x = 0 || unfencedPlots.[x - 1, y] <> m then perimeter <- perimeter + 1
                if x = sizeX - 1 || unfencedPlots.[x + 1, y] <> m then perimeter <- perimeter + 1
                if y = 0 || unfencedPlots.[x, y - 1] <> m then perimeter <- perimeter + 1
                if y = sizeY - 1 || unfencedPlots.[x, y + 1] <> m then perimeter <- perimeter + 1
    
    let mutable sides = 0
    for x in 0 .. sizeX - 1 do
        for y in 0 .. sizeY - 1 do
            if unfencedPlots.[x, y] = m then
                if x = 0 then
                    if y = 0 || unfencedPlots.[x, y - 1] <> m then sides <- sides + 1
                elif unfencedPlots.[x - 1, y] <> m then
                    if y = 0 || unfencedPlots.[x - 1, y - 1] = m || unfencedPlots.[x, y - 1] <> m then sides <- sides + 1

                if x = sizeX - 1 then
                    if y = 0 || unfencedPlots.[x, y - 1] <> m then sides <- sides + 1
                elif unfencedPlots.[x + 1, y] <> m then
                    if y = 0 || unfencedPlots.[x + 1, y - 1] = m || unfencedPlots.[x, y - 1] <> m then sides <- sides + 1
                
                if y = 0 then
                    if x = 0 || unfencedPlots.[x - 1, y] <> m then sides <- sides + 1
                elif unfencedPlots.[x, y - 1] <> m then
                    if x = 0 || unfencedPlots.[x - 1, y - 1] = m || unfencedPlots.[x - 1, y] <> m then sides <- sides + 1

                if y = sizeY - 1 then
                    if x = 0 || unfencedPlots.[x - 1, y] <> m then sides <- sides + 1
                elif unfencedPlots.[x, y + 1] <> m then
                    if x = 0 || unfencedPlots.[x - 1, y + 1] = m || unfencedPlots.[x - 1, y] <> m then sides <- sides + 1

    // clear the region
    for x in 0 .. sizeX - 1 do
        for y in 0 .. sizeY - 1 do
            if unfencedPlots.[x, y] = m then unfencedPlots.[x, y] <- ' '

    (plotSize, perimeter, sides)

let allRegions =
    let unfencedPlots = Array2D.copy plots

    let mutable regions = []
    let mutable allFenced = false

    while not allFenced do
        let mutable foundUnfenced = false
        let mutable x = 0
        let mutable y = 0
        while not foundUnfenced && not allFenced do
            if unfencedPlots.[x, y] <> ' ' then
                foundUnfenced <- true
            else
                if x = sizeX - 1 then
                    if y = sizeY - 1 then
                        allFenced <- true
                    else
                        y <- y + 1
                        x <- 0
                else
                    x <- x + 1
        if foundUnfenced then
            regions <- regionPriceAndMark unfencedPlots x y :: regions
    
    regions

let part1() =
    let totalCost = allRegions |> List.sumBy (fun (size, perimeter, _) -> size * perimeter)

    printfn "Total cost: %i" totalCost

let part2() =
    let totalCost = allRegions |> List.sumBy (fun (size, _, sides) -> size * sides)

    printfn "Total cost: %i" totalCost
