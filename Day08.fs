module Day08

open System.IO

let parse filename =
    File.ReadAllLines filename
    |> Array.map (fun line ->
        let parts = line.Split(" | ")

        let patterns =
            parts.[0].Split(" ")
            |> Array.map (fun s -> Set.ofSeq s)

        let output =
            parts.[1].Split(" ")
            |> Array.map (fun s -> Set.ofSeq s)

        patterns, output)

let part1 input =
    input
    |> Array.sumBy (fun (_, output) ->
        output
        |> Array.filter (fun x ->
            let len = Set.count x
            len = 2 || len = 3 || len = 4 || len = 7)
        |> Array.length)

let part2 input =
    let getResult (patterns, output) =
        let getPatternOfLength len =
            patterns
            |> Array.filter (fun p -> Set.count p = len)
            |> Array.head

        // 1, 4, 7, and 8 have unique lengths
        let p1 = getPatternOfLength 2
        let p4 = getPatternOfLength 4
        let p7 = getPatternOfLength 3
        let p8 = getPatternOfLength 7

        // combining 6 and 1 gives 8
        let p6 =
            patterns
            |> Array.filter (fun p -> (Set.count p) = 6 && (Set.union p1 p) = p8)
            |> Array.head

        // combining 4 and 2 gives 8
        let p2 =
            patterns
            |> Array.filter (fun p -> (Set.count p) = 5 && (Set.union p4 p) = p8)
            |> Array.head

        // combining 5 and 6 gives 6
        let p5 =
            patterns
            |> Array.filter (fun p -> (Set.count p) = 5 && (Set.union p6 p) = p6)
            |> Array.head

        // 9 is the union of 1 and 5
        let p9 = Set.union p1 p5

        // difference between 1 and 3 has three segments
        let p3 =
            patterns
            |> Array.filter (fun p -> (Set.difference p p1) |> Set.count = 3)
            |> Array.head

        // 0 has 6 segments and is not 6 or 9
        let p0 =
            patterns
            |> Array.filter (fun p -> (Set.count p) = 6 && p <> p6 && p <> p9)
            |> Array.head

        let mappings =
            [ (p0, 0)
              (p1, 1)
              (p2, 2)
              (p3, 3)
              (p4, 4)
              (p5, 5)
              (p6, 6)
              (p7, 7)
              (p8, 8)
              (p9, 9) ]
            |> Map.ofSeq

        output
        |> Array.rev
        |> Array.mapi (fun i x -> (pown 10 i) * mappings.[x])
        |> Array.sum

    input |> Array.sumBy getResult

let input = parse "./inputs/Day08.txt"
let result1 = part1 input
let result2 = part2 input
