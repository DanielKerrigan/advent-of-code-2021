module Day02

open System.IO

type Command =
    | Forward of int
    | Down of int
    | Up of int

let parse filename =
    File.ReadAllLines filename
    |> Array.map (fun line ->
        let parts = line.Split(' ')
        let units = int parts[1]

        match parts[0] with
        | "forward" -> Forward units
        | "down" -> Down units
        | "up" -> Up units
        | _ -> failwith "unknown command type")

let part1 commands =
    let (horizontal, depth) =
        ((0, 0), commands)
        ||> Array.fold (fun (horizontal, depth) command ->
            match command with
            | Forward x -> (horizontal + x, depth)
            | Down x -> (horizontal, depth + x)
            | Up x -> (horizontal, depth - x))

    horizontal * depth

let part2 commands =
    let (horizontal, depth, aim) =
        ((0, 0, 0), commands)
        ||> Array.fold (fun (horizontal, depth, aim) command ->
            match command with
            | Forward x -> (horizontal + x, depth + (aim * x), aim)
            | Down x -> (horizontal, depth, aim + x)
            | Up x -> (horizontal, depth, aim - x))

    horizontal * depth

let input = parse "inputs/Day02.txt"
let result1 = part1 input
let result2 = part2 input
