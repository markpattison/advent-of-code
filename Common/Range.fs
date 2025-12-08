[<RequireQualifiedAccess>]
module Common.Range

open Common.Types

// inclusive ranges

let size r = 1L + r.Max - r.Min

let contains n r =  r.Min <= n && r.Max >= n

let merge r1 r2 =
    let ra, rb = if r1.Min > r2.Min then r2, r1 else r1, r2 // now ra.Min >= rb.Min

    if rb.Min > ra.Max + 1L then
        [| ra; rb |] // do not overlap
    else
        let joint = { Min = ra.Min; Max = max ra.Max rb.Max}
        [| joint |]

let private mergeOrdered (orderedRanges: Range[]) r =
    if orderedRanges.Length = 0 then
        [| r |]
    else
        let allButLast = orderedRanges |> Array.take (orderedRanges.Length - 1)
        let lastRange = orderedRanges |> Array.last
        let merged = merge lastRange r
        Array.append allButLast merged

let mergeAll ranges =
    ranges
    |> Array.sortBy _.Min
    |> Array.fold mergeOrdered Array.empty
