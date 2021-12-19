module Day01

open System.IO

let parse filename =
    File.ReadAllLines filename |> Array.map int

let part1 depths =
    depths
    |> Array.pairwise
    |> Array.filter (fun (x, y) -> y > x)
    |> Array.length

let part2 depths =
    depths
    |> Array.windowed 3
    |> Array.map Array.sum
    |> part1

let input = parse "./inputs/Day01.txt"
let result1 = part1 input
let result2 = part2 input
