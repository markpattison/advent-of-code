module Day16

open System
open System.IO

let line = File.ReadAllLines(@"input\day16.txt").[0]

let testLine1 = "D2FE28"
let testLine2 = "38006F45291200"
let testLine3 = "EE00D40C823060"
let testLine4 = "8A004A801A8002F478"
let testLine5 = "620080001611562C8802118E34"
let testLine6 = "C0015000016115A2E0802F182340"
let testLine7 = "A0016C880162017C3686B18A3D4780"

let testLine8 = "C200B40A82"
let testLine9 = "04005AC33890"
let testLine10 = "880086C3E88112"
let testLine11 = "CE00C43D881120"
let testLine12 = "D8005AC2A8F0"
let testLine13 = "F600BC2D8F"
let testLine14 = "9C005AC2F8F0"
let testLine15 = "9C0141080250320F1802104A08"

let binaryWords =
    [|
        "0000"
        "0001"
        "0010"
        "0011"
        "0100"
        "0101"
        "0110"
        "0111"
        "1000"
        "1001"
        "1010"
        "1011"
        "1100"
        "1101"
        "1110"
        "1111"
    |]

let charToBinary (c: char) = binaryWords.[Convert.ToInt32(c.ToString(), 16)]

let toBinary hex =
    let words = hex |> Seq.map charToBinary
    String.Join("", words)

let binToInt (bin: string) =
    Convert.ToInt32(bin, 2)

let binToUint64 (bin: string) =
    Convert.ToUInt64(bin, 2)

type OperatorType = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo

let toOperatorType typeId =
    match typeId with
    | 0 -> Sum
    | 1 -> Product
    | 2 -> Minimum
    | 3 -> Maximum
    | 5 -> GreaterThan
    | 6 -> LessThan
    | 7 -> EqualTo
    | _ -> failwithf "unrecognised type id: %i" typeId

type Payload =
    | LiteralValue of uint64
    | Operator of OperatorType * Packet list

and Packet =
    {
        Version: int
        Payload: Payload
    }

type ParseResult =
    {
        Packet: Packet
        Remaining: string
    }

let parseLiteral version (s: string) =
    let words = Seq.chunkBySize 5 s |> Seq.toList

    let rec parse acc (remainingWords: char[] list) =
        match remainingWords with
        | word :: remaining when word.Length = 5 && word.[0] = '0' -> (Array.append acc word.[1 .. 4], remaining)
        | word :: remaining when word.Length = 5 && word.[0] = '1' -> parse (Array.append acc word.[1 .. 4]) remaining
        | _ -> failwith "error"

    let value, remaining = parse [||] words

    let packet = { Version = version; Payload = value |> String |> binToUint64 |> LiteralValue }

    { Packet = packet; Remaining = remaining |> Seq.concat |> Seq.toArray |> String }

let rec parseOperator version typeId (s: string) =
    let lengthTypeId = s.[0]

    match lengthTypeId with
    | '0' ->
        let length = s.[1 .. 15] |> String |> binToInt
        let subPacketData, next = s.[16 .. (16 + length - 1)], s.[(16 + length) ..]
        let subPackets = parseAllPackets subPacketData

        let payload = Operator (toOperatorType typeId, subPackets)
        let packet = { Version = version; Payload = payload }

        { Packet = packet; Remaining = next }

    | '1' ->
        let numberOfPackets = s.[1 .. 11] |> String |> binToInt
        let subPackets, next = parsePackets [] numberOfPackets s.[12 ..]

        let payload = Operator (toOperatorType typeId, subPackets)
        let packet = { Version = version; Payload = payload }

        { Packet = packet; Remaining = next }

    | c -> failwithf "unexpected char: %c" c

and parsePacket (s: string) =
    if s.Length < 6 then failwith "too short"

    let version = s.Substring(0, 3) |> binToInt
    let typeId = s.Substring(3, 3) |> binToInt
    let remaining = s.Substring(6)

    match typeId with
    | 4 -> parseLiteral version remaining
    | _ -> parseOperator version typeId remaining

and parseAllPackets (s: string) =
    let parseResult = parsePacket s
    match parseResult.Remaining with
    | "" -> [ parseResult.Packet ]
    | remaining -> parseResult.Packet :: parseAllPackets remaining

and parsePackets acc numberOfPackets (s: string) =
    let parseResult = parsePacket s
    match numberOfPackets with
    | 1 -> acc @ [ parseResult.Packet ], parseResult.Remaining
    | n -> parsePackets (acc @ [ parseResult.Packet ]) (n - 1) parseResult.Remaining

let rec versionSums packet =
    match packet.Payload with
    | LiteralValue _ -> packet.Version
    | Operator (_, subPackets) -> packet.Version + List.sumBy versionSums subPackets

let parseResult = line |> toBinary |> parsePacket

let part1() =
    let versionTotal = versionSums parseResult.Packet

    printfn "Version total: %i" versionTotal

let part2() =

    let evaluateOp opType =
        match opType with
        | Sum -> List.sum
        | Product -> List.fold (*) 1UL
        | Minimum -> List.fold min UInt64.MaxValue
        | Maximum -> List.fold max UInt64.MinValue
        | GreaterThan -> fun l -> if l.Item 0 > l.Item 1 then 1UL else 0UL
        | LessThan -> fun l -> if l.Item 0 < l.Item 1 then 1UL else 0UL
        | EqualTo -> fun l -> if l.Item 0 = l.Item 1 then 1UL else 0UL

    let rec evaluate packet =
        match packet.Payload with
        | LiteralValue x -> x
        | Operator (opType, subPackets) ->
            let subValues = subPackets |> List.map evaluate
            let evaluator = evaluateOp opType
            evaluator subValues

    let finalValue = evaluate parseResult.Packet

    printfn "Final value: %i" finalValue
