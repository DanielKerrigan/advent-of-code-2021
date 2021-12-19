module Day07

open System.IO

let parse filename =
    let lines = File.ReadAllLines filename
    lines.[0].Split(",")
    |> Array.map int
    |> Array.sort

let part1 crabs =
    let total m = Array.sumBy (fun x -> abs (m - x)) crabs

    let numCrabs = Array.length crabs
    let i = numCrabs / 2

    if numCrabs % 2 = 0 || crabs[i] = crabs[i + 1] then total crabs[i]
    else min (total crabs[i]) (total crabs[i + 1])

let part2 crabs =
    seq {(Array.head crabs) .. (Array.last crabs)}
    |> Seq.map (fun x ->
        crabs |> Array.sumBy (fun c ->
            let diff = abs (x - c)
            diff * (diff + 1) / 2
        )
    )
    |> Seq.min

let input = parse "./inputs/Day07.txt"
let result1 = part1 input
let result2 = part2 input
